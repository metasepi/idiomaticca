-- | Library to translate IDIOMATIC C into readable ATS.
module Language.Idiomaticca
    ( interpretTranslationUnit
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Debug.Trace
import qualified Control.Monad.State as St
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as Ne
import qualified Language.ATS as A
import qualified Language.C as C

type Pos = A.AlexPosn

dummyPos :: Pos
dummyPos = A.AlexPn 0 0 0

type AAts = A.ATS Pos
type ADecl = A.Declaration Pos
type AExpr = A.Expression Pos
type AType = A.Type Pos
type AArgs = A.Args Pos
type AArg = A.Arg Pos
type APat = A.Pattern Pos
type ALamT = A.LambdaType Pos
type AUni = A.Universal Pos

-- | Comments should be removed in ATS AST
todoBreak, todoCont :: ADecl
todoBreak = A.Comment "(* _I9A_ CBreak *)"
todoCont = A.Comment "(* _I9A_ CCont *)"

-- | Pickup just expr fom `([ADecl], AExpr, [ADecl])`.
justE :: Show a => Show b => ([a], b, [a]) -> b
justE ([], r, []) = r
justE e = traceShow e undefined

-- | Concat `([ADecl], AExpr, [ADecl])` to ATS declarations.
catPreJustPost :: ([ADecl], AExpr, [ADecl]) -> [ADecl]
catPreJustPost (preD, justE, postD) =
  case justE of
    (A.Binary A.Mutate _ _) -> preD ++ [makeVal patVoid justE] ++ postD
    _ -> preD ++ postD

-- | Prefix name for internal usage
prefixI :: String -> String
prefixI name = "i9a_" ++ name

-- | Prefix name for proof
prefixP :: String -> String
prefixP name = prefixI $ "pf_" ++ name

-- | State to keep defined Functions and Vars.
data IEnv = IEnv { iEnvDeclFuns :: Set.Set String -- declared function names
                 , iEnvDeclVars :: [(String, AType)] -- defined var names and types
                 , iEnvDynViews :: [(String, AExpr)] -- views of dynamics
                 , iEnvUsedVars :: Set.Set String -- used var names
                 }
  deriving (Show)

-- | State. `main` function already defined.
defaultIEnv :: IEnv
defaultIEnv = IEnv { iEnvDeclFuns = Set.singleton "main"
                   , iEnvDeclVars = []
                   , iEnvDynViews = []
                   , iEnvUsedVars = Set.empty
                   }

-- | Convert `iEnvDeclVars` to ATS `Args`.
iEnvDeclVarsArgs :: [(String, AType)] -> AArgs
iEnvDeclVarsArgs vars =
  Just $ fmap go vars
  where
    go :: (String, AType) -> AArg
    go (name, aType) = A.Arg $ A.Both name aType

-- | Convert `iEnvDeclVars` to ATS expression for `callArgs`.
iEnvDeclVarsCallArgs :: [(String, AType)] -> [AExpr]
iEnvDeclVarsCallArgs vars =
  A.NamedVal . A.Unqualified <$> fmap fst vars

-- | Convert `iEnvDeclVars` to ATS `TupleEx`.
iEnvDeclVarsTupleEx :: [(String, AType)] -> AExpr
iEnvDeclVarsTupleEx vars =
  A.TupleEx dummyPos $ Ne.fromList $ reverse $ iEnvDeclVarsCallArgs vars

-- | Convert `iEnvDeclVars` to ATS data type for pattern.
iEnvDeclVarsTuplePat :: [(String, AType)] -> APat
iEnvDeclVarsTuplePat vars =
  A.TuplePattern ((\n -> A.UniversalPattern dummyPos n [] Nothing) <$> fmap fst vars)

-- | Keep the function name in `IEnv`.
iEnvRecordFun :: String -> St.State IEnv ()
iEnvRecordFun fname =
  St.modify $ \s -> s { iEnvDeclFuns = Set.insert fname (iEnvDeclFuns s) }

-- | Use the var, and record `IEnv`.
iEnvRecordUsedVar :: String -> St.State IEnv ()
iEnvRecordUsedVar name =
  St.modify $ \s -> s { iEnvUsedVars = Set.insert name (iEnvUsedVars s) }

-- | Declare and use the var, and record `IEnv`.
iEnvRecordDeclUsedVar :: String -> AType -> St.State IEnv ()
iEnvRecordDeclUsedVar name aType = do
  -- xxx Should drop old key/value pair on iEnvDeclVars
  St.modify $ \s -> s { iEnvDeclVars = (name, aType) : iEnvDeclVars s }
  iEnvRecordUsedVar name

-- | Record at-view in `IEnv`.
iEnvProduceDynView :: String -> AExpr -> St.State IEnv ()
iEnvProduceDynView name expr = do
  -- xxx Should consume old key/value pair on iEnvDynViews
  St.modify $ \s -> s { iEnvDynViews = (name, expr) : iEnvDynViews s }

-- | Clear {Decl,Used} vars in `IEnv`.
iEnvClearDVDVUV :: St.State IEnv ()
iEnvClearDVDVUV =
  St.modify $ \s -> s { iEnvDeclVars = []
                      , iEnvDynViews = []
                      , iEnvUsedVars = Set.empty }

-- | Find used and pre-defined vars for args of recursion function
usedTypedVars :: [C.CExpr] -> [C.CStat] -> St.State IEnv [(String, AType)]
usedTypedVars exprs stats = do
  let envExprs = fmap (\e -> St.execState (interpretExpr e) defaultIEnv) exprs
  let envStats = fmap (\s -> St.execState (interpretStatementExp s) defaultIEnv) stats
  let usedVars = fmap iEnvUsedVars $ envExprs ++ envStats
  let usedVars' = Set.toList $ foldr Set.union Set.empty usedVars
  s <- St.get
  return $ mapMaybe (\u -> (,) u <$> lookup u (iEnvDeclVars s)) usedVars'

-- | Convert ATS `Args` to ATS `Var`s.
atsArgsVars :: AArgs -> [ADecl]
atsArgsVars args =
  case args of Nothing -> []
               Just [] -> []
               Just args' -> fmap go args'
  where
    go :: AArg -> ADecl
    go (A.Arg (A.Both name aType)) = v name aType
    go (A.PrfArg _ (A.Arg (A.Both name aType))) = v name aType
    go x = traceShow x undefined
    v :: String -> AType -> ADecl
    v name aType =
      A.Var { A.varT = Just aType
            , A.varPat = A.UniversalPattern dummyPos name [] Nothing
            , A._varExpr1 = Just $ A.NamedVal $ A.Unqualified name
            , A._varExpr2 = Nothing
            }

-- | Convert C unary to ATS expression with
unop :: C.CUnaryOp -> C.CExpr -> St.State IEnv ([ADecl], AExpr, [ADecl])
unop op expr = do
  expr' <- justE <$> interpretExpr expr
  return $ case op of
    C.CPreIncOp ->
      ([makeVal patVoid $ A.Binary A.Mutate expr' $ A.Binary A.Add expr' (A.IntLit 1)],
       expr', [])
    C.CPreDecOp ->
      ([makeVal patVoid $ A.Binary A.Mutate expr' $ A.Binary A.Sub expr' (A.IntLit 1)],
       expr', [])
    C.CPostIncOp ->
      ([], expr',
       [makeVal patVoid $ A.Binary A.Mutate expr' $ A.Binary A.Add expr' (A.IntLit 1)])
    C.CPostDecOp ->
      ([], expr',
       [makeVal patVoid $ A.Binary A.Mutate expr' $ A.Binary A.Sub expr' (A.IntLit 1)])
    C.CAdrOp ->
      ([], A.AddrAt dummyPos expr', [])
    C.CIndOp ->
      ([], A.Unary A.Deref expr', [])
    op ->
      traceShow op undefined

-- | Convert C binary operator to ATS expression.
binop :: C.CBinaryOp -> AExpr -> AExpr -> St.State IEnv AExpr
binop op lhs rhs =
  let op' = case op of C.CMulOp -> A.Mult
                       C.CDivOp -> A.Div
                       C.CAddOp -> A.Add
                       C.CSubOp -> A.Sub
                       C.CLeOp  -> A.LessThan
                       C.CGrOp  -> A.GreaterThan
                       C.CLeqOp -> A.LessThanEq
                       C.CGeqOp -> A.GreaterThanEq
                       C.CEqOp  -> A.StaticEq
                       C.CNeqOp -> A.NotEq
                       o -> traceShow o undefined
  in return $ A.Binary op' lhs rhs

-- | Some names are special in C or special in ATS, or both.
applyRenames :: C.Ident -> String
applyRenames ident = case C.identToString ident of
  name -> name -- xxx Get the rename rule with keywords of ATS language

singleSpec :: C.CTypeSpec -> AType
singleSpec (C.CIntType _) = A.Named $ A.Unqualified "int"
singleSpec (C.CCharType _) = A.Named $ A.Unqualified "char"
singleSpec (C.CVoidType _) = A.Named $ A.Unqualified "void"
singleSpec cType =
  traceShow cType undefined

-- | Convert C declaration specifiers and qualifiers to ATS type.
baseTypeOf :: [C.CDeclSpec] -> AType
baseTypeOf (C.CStorageSpec _:ss) = baseTypeOf ss
baseTypeOf [C.CTypeSpec spec] = singleSpec spec
baseTypeOf specs =
  traceShow specs undefined

-- | Void pattern for ATS `Val`.
patVoid :: Maybe APat
patVoid = Just (A.PLiteral (A.VoidLiteral dummyPos))

-- | Make ATS `Val`.
makeVal :: Maybe APat -> AExpr -> ADecl
makeVal pat aExpr = A.Val { A.add = A.None
                          , A.valT = Nothing
                          , A.valPat = pat
                          , A._valExpression = Just aExpr
                          }

-- | Make ATS condition, which is used by `if`. It needs boolean value.
makeCond :: C.CExpr -> St.State IEnv ([ADecl], AExpr, [ADecl])
makeCond cond = do
  cond'@(preD, justE, postD) <- interpretExpr cond
  return $ case cond of
             (C.CBinary C.CLeOp  _ _ _) -> cond'
             (C.CBinary C.CGrOp  _ _ _) -> cond'
             (C.CBinary C.CLeqOp _ _ _) -> cond'
             (C.CBinary C.CGeqOp _ _ _) -> cond'
             (C.CBinary C.CEqOp  _ _ _) -> cond'
             (C.CBinary C.CNeqOp _ _ _) -> cond'
             _ -> (preD, A.Binary A.NotEq justE (A.IntLit 0), postD)

-- | Make args for function.
makeArgs :: AArgs -> Maybe AExpr -> Maybe AExpr
makeArgs args body =
  case body of
    Nothing -> Nothing
    b -> if nullArgs args then body
         else fmap (A.Let dummyPos (A.ATS $ atsArgsVars args)) (Just b)
  where
    nullArgs :: AArgs -> Bool
    nullArgs Nothing = True
    nullArgs (Just []) = True
    nullArgs _ = False

-- | Make ATS function.
makeFunc :: String -> (AArgs, [AUni]) -> Maybe AExpr -> Maybe AType -> St.State IEnv ADecl
makeFunc fname (args, unis) body ret = do
  iEnvRecordFun fname
  -- Introduce `var` on args
  let body' = makeArgs args body
  return $ A.Func dummyPos
    (A.Fun A.PreF { A.fname = A.Unqualified fname
                  , A.sig = Just ""
                  , A.preUniversals = []
                  , A.universals = unis -- xxx Should be simplified
                  , A.args = fmap reverse args
                  , A.returnType = ret
                  , A.termetric = Nothing
                  , A._expression = body'
                  })

-- | Implement ATS function.
makeImpl :: String -> (AArgs, [AUni]) -> AExpr -> St.State IEnv ADecl
makeImpl fname (args, unis) body = do
  -- Introduce `var` on args
  let (Just body') = makeArgs args (Just body)
  return A.Impl { A.implArgs = Nothing
                , A._impl = A.Implement
                    dummyPos -- pos
                    [] -- preUniversalsI
                    [] -- implicits
                    [] -- universalsI
                    (A.Unqualified fname) -- nameI
                    (fmap reverse args) -- iArgs
                    (Right body') -- _iExpression
                }

-- | Make ATS `Call`
makeCall :: String -> [AExpr] -> St.State IEnv AExpr
makeCall fname args = do
  pa <- proofArgs args
  return $ A.Call { A.callName = A.Unqualified fname
                  , A.callImplicits = []
                  , A.callUniversals = []
                  , A.callProofs = fmap reverse pa
                  , A.callArgs = reverse args
                  }
  where
    proofArgs :: [AExpr] -> St.State IEnv (Maybe [AExpr])
    proofArgs e = do
      r <- proofArgs' e
      return $ case r of [] -> Nothing
                         e' -> Just e'
    proofArgs' :: [AExpr] -> St.State IEnv [AExpr]
    proofArgs' (A.AddrAt _ e:xs) = (A.ViewAt dummyPos e :) <$> proofArgs' xs
    proofArgs' (A.NamedVal (A.Unqualified n):xs) = do
      s <- St.get
      ((maybeToList $ lookup n (iEnvDynViews s)) ++) <$> proofArgs' xs
    proofArgs' (_:xs) = proofArgs' xs
    proofArgs' [] = return []

-- | Make loop body without `break` and `continue` comments
makeLoopBody :: [ADecl] -> [ADecl] -> AExpr -> AExpr -> AExpr
makeLoopBody body post call ret =
  removeBC body post call ret []
  where
    removeBC :: [ADecl] -> [ADecl] -> AExpr -> AExpr -> [ADecl] -> AExpr
    -- `break` is found
    removeBC (A.Val _ _ _ (Just (A.If cond (A.Let _ (A.ATS letDecls) Nothing) Nothing)):decls) post call ret cont | todoBreak `elem` letDecls =
      let letDecls' = takeWhile (/= todoBreak) letDecls
          thenE = if null letDecls' then ret
                  else A.Let dummyPos (A.ATS $ letDecls' ++ post) (Just ret)
      in A.Let dummyPos (A.ATS cont)
         (Just (A.If cond thenE (Just $ A.Let dummyPos (A.ATS $ decls ++ post) (Just call))))
    -- `continue` is found
    removeBC x@(A.Val _ _ _ (Just (A.If cond (A.Let _ (A.ATS letDecls) Nothing) Nothing)):decls) post call ret cont | todoCont `elem` letDecls =
      let letDecls' = takeWhile (/= todoCont) letDecls
          thenE = A.Let dummyPos (A.ATS $ letDecls' ++ post) (Just call)
      in A.Let dummyPos (A.ATS cont)
         (Just (A.If cond thenE (Just $ A.Let dummyPos (A.ATS $ decls ++ post) (Just call))))
    removeBC (x:xs) post call ret cont =
      removeBC xs post call ret (cont ++ [x])
    removeBC [] post call ret cont =
      A.Let dummyPos (A.ATS $ cont ++ post) (Just call)

-- | Make `while` or `for` loop using a recursion function
makeLoop :: String -> Either (Maybe C.CExpr) C.CDecl -> Maybe C.CExpr -> Maybe C.CExpr -> C.CStat -> St.State IEnv [ADecl]
makeLoop nameBase (Left initA) cond incr stat = do
  vars <- usedTypedVars (catMaybes [initA, cond, incr]) [stat]
  -- Make recursion function
  decls <- interpretStatementDecl stat
  let loopName = nameBase -- xxx Should be unique function name
  callLoop <- makeCall loopName $ iEnvDeclVarsCallArgs vars
  incr' <- mapM interpretExpr incr
  let incr'' = fmap catPreJustPost incr'
  (preCondE, justCondE, postCondE) <- makeCond $ fromJust cond
  -- xxx Should use preCondE
  let body = makeLoopBody (postCondE ++ decls) (fromMaybe [] incr'') callLoop (iEnvDeclVarsTupleEx vars)
  let ifte = A.If justCondE body (Just $ iEnvDeclVarsTupleEx vars)
  let args = iEnvDeclVarsArgs vars
  func <- makeFunc loopName (args, []) (Just ifte)
            (Just (A.Tuple dummyPos $ reverse $ fmap snd vars))
  -- Initialize
  initA' <- mapM interpretExpr initA
  let initA'' = fmap justE initA'
  let initA''' = fmap (makeVal patVoid) (maybeToList initA'')
  -- Call the recursion function
  let varsPat = iEnvDeclVarsTuplePat $ fmap (\(n,t) -> (prefixI n,t)) vars
  let callPat = makeVal (Just varsPat) callLoop
  -- Re-assign vars after call the recursion function
  let reAssign = (\n -> makeVal patVoid $ A.Binary A.Mutate
                        (A.NamedVal $ A.Unqualified n)
                        (A.NamedVal $ A.Unqualified $ prefixI n)) <$> fmap fst vars
  return $ [func] ++ initA''' ++ [callPat] ++ reAssign

-- | Convert C expression to ATS expression with preDecls and postDecls.
interpretExpr :: C.CExpr -> St.State IEnv ([ADecl], AExpr, [ADecl])
interpretExpr (C.CConst c) = case c of
  C.CIntConst int _ -> return ([], A.IntLit $ fromInteger $ C.getCInteger int, [])
  C.CCharConst (C.CChar char _) _ -> return ([], A.CharLit char, [])
  _ -> traceShow c undefined
interpretExpr (C.CVar ident _) = do
  let name = applyRenames ident
  iEnvRecordUsedVar name
  return ([], A.NamedVal $ A.Unqualified name, [])
interpretExpr (C.CUnary op expr _) =
  unop op expr
interpretExpr (C.CBinary op lhs rhs _) = do
  (lPre, lJust, lPost) <- interpretExpr lhs
  (rPre, rJust, rPost) <- interpretExpr rhs
  b <- binop op lJust rJust
  return (lPre ++ rPre, b, lPost ++ rPost)
interpretExpr (C.CAssign C.CAssignOp expr1 expr2 _) = do
  expr1' <- justE <$> interpretExpr expr1
  expr2' <- justE <$> interpretExpr expr2
  case (expr1', expr2') of
    (A.NamedVal (A.Unqualified n), A.AddrAt _ e) -> iEnvProduceDynView n $ A.ViewAt dummyPos e
    _ -> return ()
  return ([], A.Binary A.Mutate expr1' expr2', [])
interpretExpr (C.CCall (C.CVar ident _) args _) = do
  args' <- mapM interpretExpr args
  let args'' = fmap justE args'
  just <- makeCall (applyRenames ident) args''
  return ([], just, [])
interpretExpr expr =
  traceShow expr undefined

-- | Convert C declaration to ATS declarations. C can multiple-define vars.
interpretDeclarations :: C.CDecl -> St.State IEnv [ADecl]
-- xxx Abstract duplicated `CDeclr`
interpretDeclarations (C.CDecl specs [(Just (C.CDeclr (Just ident) [derived@C.CFunDeclr{}] _ _ _), _, _)] _) = do
  let fname = applyRenames ident
  args <- interpretCDerivedDeclrArgs derived
  func <- makeFunc fname args Nothing (Just $ baseTypeOf specs)
  return [func]
interpretDeclarations (C.CDecl specs declrs _) =
  mapM go declrs
  where
    go :: (Maybe C.CDeclr, Maybe C.CInit, Maybe C.CExpr) -> St.State IEnv ADecl
    go (Just (C.CDeclr (Just ident) [] Nothing [] _), initi, _) = do
      let name = applyRenames ident
      let aType = baseTypeOf specs
      iEnvRecordDeclUsedVar name aType
      initi' <- mapM cInit initi
      return $ A.Var { A.varT = Just aType
                     , A.varPat = A.UniversalPattern dummyPos name [] Nothing
                     , A._varExpr1 = initi'
                     , A._varExpr2 = Nothing
                     }
    go (Just (C.CDeclr (Just ident) [C.CPtrDeclr _ _] Nothing [] _), Nothing, _) = do
      let name = applyRenames ident
      return $ A.Var { A.varT = Just $ A.Named $ A.Unqualified "ptr"
                     , A.varPat = A.UniversalPattern dummyPos name [] Nothing
                     , A._varExpr1 = Nothing
                     , A._varExpr2 = Nothing
                     }
    go x = traceShow x undefined
    cInit :: C.CInit -> St.State IEnv AExpr
    cInit (C.CInitExpr expr _) = justE <$> interpretExpr expr
interpretDeclarations cDecl =
  traceShow cDecl undefined

-- | Convert C block item to ATS declarations. C can multiple-define vars.
interpretBlockItemDecl :: C.CBlockItem -> St.State IEnv [ADecl]
interpretBlockItemDecl (C.CBlockDecl decl) =
  interpretDeclarations decl
interpretBlockItemDecl (C.CBlockStmt statement) =
  interpretStatementDecl statement
interpretBlockItemDecl bItem =
  traceShow bItem undefined

-- | Convert C block item to ATS expression.
interpretBlockItemExp :: C.CBlockItem -> St.State IEnv AExpr
interpretBlockItemExp (C.CBlockStmt statement) =
  interpretStatementExp statement
interpretBlockItemExp bItem =
  traceShow bItem undefined

-- | Convert C statement to ATS _arms of Case.
interpretStatementCaseArms :: AExpr -> C.CStat -> St.State IEnv [(APat, ALamT, AExpr)]
interpretStatementCaseArms caseE (C.CCompound [] items _) =
  toArms items
  where
    toArms :: [C.CBlockItem] -> St.State IEnv [(APat, ALamT, AExpr)]
    toArms items = mapM toArm (filter (not . null) $ splitBreak items)
    toArm :: [C.CBlockItem] -> St.State IEnv (APat, ALamT, AExpr)
    toArm items = do
      let (cExprs, cItems) = toCaseStats [] items
      aExprs <- mapM interpretExpr cExprs
      let aExprs' = fmap justE aExprs
      aItem <- itemsExpr cItems
      return $ case length aExprs' of
        0 -> (A.PName (A.Unqualified "_") [], A.Plain dummyPos, aItem)
        1 -> (A.PLiteral $ head aExprs', A.Plain dummyPos, aItem)
        _ -> (A.Guarded dummyPos (logicOrAll aExprs') (A.PName (A.Unqualified "_") []),
              A.Plain dummyPos, aItem)
    itemsExpr :: [C.CBlockItem] -> St.State IEnv AExpr
    itemsExpr items =
      if length items == 1 then do
        exprs <- mapM interpretBlockItemExp items
        return $ head exprs
      else do
        decls <- mapM interpretBlockItemDecl items
        return $ A.Let dummyPos (A.ATS $ concat decls) Nothing
    logicOrAll :: [AExpr] -> AExpr
    logicOrAll [x] = A.Binary A.Equal caseE x
    logicOrAll (x:xs) = A.Binary A.Equal caseE (A.Binary A.LogicalOr x (logicOrAll xs))
    toCaseStats :: [C.CExpr] -> [C.CBlockItem] -> ([C.CExpr], [C.CBlockItem])
    toCaseStats exprs (C.CBlockStmt (C.CCase expr stat _):items) =
      toCaseStats (exprs ++ [expr]) (C.CBlockStmt stat:items)
    toCaseStats exprs (C.CBlockStmt (C.CDefault stat _):items) =
      toCaseStats [] (C.CBlockStmt stat:items)
    toCaseStats exprs items = (exprs, items)
    splitBreak :: [C.CBlockItem] -> [[C.CBlockItem]]
    splitBreak items =
      let (stats, res) = break isBreak items
      in if null res then [] else splitBreak (tail res) ++ [stats]
    isBreak :: C.CBlockItem -> Bool
    isBreak (C.CBlockStmt (C.CBreak _)) = True
    isBreak _ = False
interpretStatementCaseArms expr stat =
  traceShow (expr, stat) undefined

-- | Convert C statement to ATS declaration.
interpretStatementDecl :: C.CStat -> St.State IEnv [ADecl]
interpretStatementDecl (C.CExpr (Just expr) _) = do
  (pre, just, post) <- interpretExpr expr
  return $ pre ++ insertJust just ++ post
  where
    insertJust :: AExpr -> [ADecl]
    insertJust e@(A.Binary A.Mutate _ _) = [makeVal patVoid e]
    insertJust e@A.Call{} = [makeVal patVoid e] -- xxx Should `val _ =`
    insertJust x = []
interpretStatementDecl cIf@C.CIf{} = do
  cIf' <- interpretStatementExp cIf
  return [makeVal patVoid cIf']
interpretStatementDecl (C.CWhile cond stat False _) =
  makeLoop "loop_while" (Left Nothing) (Just cond) Nothing stat
interpretStatementDecl (C.CFor initA cond incr stat _) =
  makeLoop "loop_for" initA cond incr stat
interpretStatementDecl (C.CCompound [] items _) =
  concat <$> mapM interpretBlockItemDecl items
interpretStatementDecl (C.CSwitch expr stat _) = do
  expr' <- justE <$> interpretExpr expr
  arms <- interpretStatementCaseArms expr' stat
  return [makeVal patVoid $ A.Case dummyPos A.None expr' arms]
interpretStatementDecl (C.CBreak _) =
  return [todoBreak]
interpretStatementDecl (C.CCont _) =
  return [todoCont]
interpretStatementDecl stat =
  traceShow stat undefined

-- | Convert C statement to ATS expression.
interpretStatementExp :: C.CStat -> St.State IEnv AExpr
interpretStatementExp (C.CCompound [] items _) = do
  let items' = takeReturn items -- Items before return
  let ret = pickReturn items -- A item may be return
  decls <- concat <$> mapM interpretBlockItemDecl items'
  exp <- mapM interpretBlockItemExp ret
  return $ if null decls && isJust exp then fromJust exp
           else A.Let dummyPos (A.ATS decls) exp
  where
    takeReturn :: [C.CBlockItem] -> [C.CBlockItem]
    takeReturn [] = []
    takeReturn (i@(C.CBlockStmt C.CReturn{}):_) = []
    takeReturn (i:is) = i:takeReturn is
    pickReturn :: [C.CBlockItem] -> Maybe C.CBlockItem
    pickReturn [] = Nothing
    pickReturn (i@(C.CBlockStmt C.CReturn{}):_) = Just i
    pickReturn (i:is) = pickReturn is
interpretStatementExp (C.CReturn (Just expr) _) =
  justE <$> interpretExpr expr
interpretStatementExp (C.CExpr (Just expr) _) =
  justE <$> interpretExpr expr
interpretStatementExp (C.CIf cond sthen selse _) = do
  cond' <- makeCond cond
  sthen' <- interpretStatementExp sthen
  selse' <- mapM interpretStatementExp selse
  return $ A.If (justE cond') sthen' selse'
interpretStatementExp stat =
  traceShow stat undefined

-- | Convert C derived declarator to ATS `Args`, and keep the vars in `IEnv`.
interpretCDerivedDeclrArgs :: C.CDerivedDeclr -> St.State IEnv (AArgs, [AUni])
interpretCDerivedDeclrArgs (C.CFunDeclr (Right (decls, _)) _ _) = do
  (_, args, unis) <- foldM go (1, [], []) decls
  return (Just $ sortA [] [] args, unis)
  where
    go :: (Int, [AArg], [AUni]) -> C.CDecl -> St.State IEnv (Int, [AArg], [AUni])
    go (n, as, us) (C.CDecl specs [(Just (C.CDeclr (Just ident) derived _ _ _), _, _)] _) = do
      let name = applyRenames ident
      let aType = baseTypeOf specs
      case derived of
        [C.CPtrDeclr _ _] -> do
          let addr = "l" ++ show n
              pType = A.Dependent { A._typeCall = A.Unqualified "ptr"
                                  , A._typeCallArgs = [A.Named $ A.Unqualified addr]
                                  }
              narg = A.PrfArg [A.Arg (A.Both (prefixP name) (A.Unconsumed (A.AtExpr
                       dummyPos aType (A.StaticVal (A.Unqualified addr)))))] $
                       A.Arg (A.Both name pType)
              nuni = A.Universal {A.bound = [addr], A.typeU = Just A.Addr, A.prop = []}
          iEnvRecordDeclUsedVar name pType
          return (n + 1, narg:as, us ++ [nuni])
        _ -> do
          iEnvRecordDeclUsedVar name aType
          return (n, A.Arg (A.Both name aType) : as, us)
    go (n, as, us) (C.CDecl specs [] _) =
      return (n, A.Arg (A.Second (baseTypeOf specs)) : as, us)
    -- xxx Should fix language-ats?
    -- xxx Following is reversed?
    sortA :: [AArg] -> [AArg] -> [AArg] -> [AArg]
    sortA pArgs args (A.PrfArg px x:xs) = sortA (pArgs ++ px) (args ++ [x]) xs
    sortA pArgs args (x:xs) = sortA pArgs (args ++ [x]) xs
    sortA pArgs args [] = case length pArgs of
      0 -> args
      _ -> A.PrfArg pArgs (last args) : reverse (init args)
interpretCDerivedDeclrArgs dDeclr =
  traceShow dDeclr undefined

-- | Convert C function definition to ATS declaration.
interpretFunction :: C.CFunDef -> St.State IEnv ADecl
interpretFunction (C.CFunDef specs (C.CDeclr (Just ident) [derived] _ _ _) _ body _) = do
  let fname = applyRenames ident
  args <- interpretCDerivedDeclrArgs derived
  body' <- interpretStatementExp body
  s <- St.get
  if fname `Set.member` iEnvDeclFuns s then
      -- Use `implement`, if the function already declared.
      makeImpl fname args body'
    else
      -- Use `fun`, if the function not yet declared.
      makeFunc fname args (Just body') (Just $ baseTypeOf specs)
interpretFunction funDef =
  traceShow funDef undefined

-- | Convert C external C declaration to ATS declaration.
perDecl :: C.CExtDecl -> St.State IEnv ADecl
perDecl (C.CFDefExt f) = do
  iEnvClearDVDVUV
  interpretFunction f
perDecl (C.CDeclExt d) = do
  iEnvClearDVDVUV
  d' <- interpretDeclarations d
  return $ A.Extern dummyPos $ head d'
perDecl eDecl =
  traceShow eDecl undefined
-- xxx `perDecl` may return `State IEnv [ADecl]`.

-- | Inject AGPLv3 comment to every output. (So bad joke?)
copyleftComment :: [String]
copyleftComment =
  ["(*"
  ," * Copyright (c) 2019 YOUR NAME"
  ," * All rights reserved."
  ," *"
  ," * This program is free software: you can redistribute it and/or modify"
  ," * it under the terms of the GNU Affero General Public License as"
  ," * published by the Free Software Foundation, either version 3 of the"
  ," * License, or (at your option) any later version."
  ," *"
  ," * This program is distributed in the hope that it will be useful,"
  ," * but WITHOUT ANY WARRANTY; without even the implied warranty of"
  ," * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
  ," * GNU Affero General Public License for more details."
  ," *"
  ," * You should have received a copy of the GNU Affero General Public"
  ," * License along with this program."
  ," * If not, see <http://www.gnu.org/licenses/>."
  ," *)"
  ,""]

-- | Convert C tranlsation unit to ATS file.
interpretTranslationUnit :: C.CTranslUnit -> AAts
interpretTranslationUnit (C.CTranslUnit cDecls _) =
  A.ATS $ fmap A.Comment copyleftComment
    ++ A.Include "\"share/atspre_staload.hats\""
     : A.Load { A.static = True
              , A.withOctothorpe = False
              , A.qualName = Just "UN"
              , A.fileName = "\"prelude/SATS/unsafe.sats\""
              }
     : St.evalState (mapM perDecl cDecls) defaultIEnv
