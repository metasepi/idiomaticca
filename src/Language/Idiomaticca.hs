-- | Library to translate IDIOMATIC C into readable ATS.
module Language.Idiomaticca
    ( interpretTranslationUnit
    ) where

import Control.Applicative
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

justE :: Show a => Show b => ([a], b, [a]) -> b
justE ([], r, []) = r
justE e = traceShow e undefined

catPreJustPost :: ([ADecl], AExpr, [ADecl]) -> [ADecl]
catPreJustPost (preD, justE, postD) =
  preD ++ [makeVal (Just (A.PName (A.Unqualified "_") [])) justE] ++ postD

-- | Prefix name for internal usage
prefixI :: String -> String
prefixI name = "i9a_" ++ name

-- | State to keep defined Functions and Vars.
data IEnv = IEnv { iEnvDeclFuns :: Set.Set String -- declared function names
                 , iEnvDeclVars :: [(String, AType)] -- defined var names and types
                 , iEnvUsedVars :: Set.Set String -- used var names
                 }
  deriving (Show)

-- | State. `main` function already defined.
defaultIEnv :: IEnv
defaultIEnv = IEnv { iEnvDeclFuns = Set.singleton "main"
                   , iEnvDeclVars = []
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
  St.modify $ \s -> s { iEnvDeclVars = (name, aType) : iEnvDeclVars s }
  iEnvRecordUsedVar name

-- | Clear {Decl,Used} vars in `IEnv`.
iEnvClearDeclUsedVar :: St.State IEnv ()
iEnvClearDeclUsedVar =
  St.modify $ \s -> s { iEnvDeclVars = [], iEnvUsedVars = Set.empty }

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
    go (A.Arg (A.Both name aType)) =
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
  name -> name
-- | xxx Get the rename rule.

singleSpec :: C.CTypeSpec -> AType
singleSpec (C.CIntType _) = A.Named $ A.Unqualified "int"
singleSpec (C.CCharType _) = A.Named $ A.Unqualified "char"
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

-- | Make ATS function.
makeFunc :: String -> AArgs -> Maybe AExpr -> Maybe AType -> St.State IEnv ADecl
makeFunc fname args body ret = do
  iEnvRecordFun fname
  -- Introduce `var` on args
  let body' = case body of Nothing -> Nothing
                           b -> fmap (A.Let dummyPos (A.ATS $ atsArgsVars args)) (Just b)
  return $ A.Func dummyPos
    (A.Fun A.PreF { A.fname = A.Unqualified fname
                  , A.sig = Just ""
                  , A.preUniversals = []
                  , A.universals = []
                  , A.args = fmap reverse args
                  , A.returnType = ret
                  , A.termetric = Nothing
                  , A._expression = body'
                  })

-- | Implement ATS function.
makeImpl :: String -> AArgs -> AExpr -> St.State IEnv ADecl
makeImpl fname args body = do
  -- Introduce `var` on args
  let body' = A.Let dummyPos (A.ATS $ atsArgsVars args) (Just body)
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
makeCall :: String -> [AExpr] -> AExpr
makeCall fname args =
  A.Call { A.callName = A.Unqualified fname
         , A.callImplicits = []
         , A.callUniversals = []
         , A.callProofs = Nothing
         , A.callArgs = reverse args
         }

-- | Make `while` or `for` loop using a recursion function
makeLoop :: String -> Either (Maybe C.CExpr) C.CDecl -> Maybe C.CExpr -> Maybe C.CExpr -> C.CStat -> St.State IEnv [ADecl]
makeLoop nameBase (Left initA) cond incr stat = do
  vars <- usedTypedVars (catMaybes [initA, cond, incr]) [stat]
  -- Make recursion function
  decls <- interpretStatementDecl stat
  let loopName = nameBase -- xxx Should be unique function name
  let callLoop = makeCall loopName $ iEnvDeclVarsCallArgs vars
  incr' <- mapM interpretExpr incr
  let incr'' = fmap catPreJustPost incr'
  (preCondE, justCondE, postCondE) <- makeCond $ fromJust cond
  -- xxx Should use preCondE
  let body = A.Let dummyPos (A.ATS $ postCondE ++ decls ++ fromMaybe [] incr'') (Just callLoop)
  let ifte = A.If justCondE body (Just $ iEnvDeclVarsTupleEx vars)
  let args = iEnvDeclVarsArgs vars
  func <- makeFunc loopName args (Just ifte)
            (Just (A.Tuple dummyPos $ reverse $ fmap snd vars))
  -- Initialize
  initA' <- mapM interpretExpr initA
  let initA'' = fmap justE initA'
  let initA''' = fmap (makeVal patVoid) (maybeToList initA'')
  -- Call the recursion function
  let varsPat = iEnvDeclVarsTuplePat $ fmap (\(n,t) -> (prefixI n,t)) vars
  let callPat = makeVal (Just varsPat) (makeCall loopName $ iEnvDeclVarsCallArgs vars)
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
  return ([], A.Binary A.Mutate expr1' expr2', [])
interpretExpr (C.CCall (C.CVar ident _) args _) = do
  args' <- mapM interpretExpr args
  let args'' = fmap justE args'
  return ([], makeCall (applyRenames ident) args'', [])
interpretExpr expr =
  traceShow expr undefined

-- | Convert C declaration to ATS declarations. C can multiple-define vars.
interpretDeclarations :: C.CDecl -> St.State IEnv [ADecl]
interpretDeclarations (C.CDecl specs [(Just (C.CDeclr (Just ident) [derived] _ _ _), _, _)] _) = do
  let fname = applyRenames ident
  args <- interpretCDerivedDeclr derived
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

-- | Convert C statement to ATS declaration.
interpretStatementDecl :: C.CStat -> St.State IEnv [ADecl]
interpretStatementDecl (C.CExpr (Just expr) _) = do
  expr' <- justE <$> interpretExpr expr
  return [makeVal patVoid expr']
interpretStatementDecl cIf@C.CIf{} = do
  cIf' <- interpretStatementExp cIf
  return [makeVal patVoid cIf']
interpretStatementDecl (C.CWhile cond stat False _) =
  makeLoop "loop_while" (Left Nothing) (Just cond) Nothing stat
interpretStatementDecl (C.CFor initA cond incr stat _) =
  makeLoop "loop_for" initA cond incr stat
interpretStatementDecl (C.CCompound [] items _) =
  concat <$> mapM interpretBlockItemDecl items
interpretStatementDecl stat =
  traceShow stat undefined

-- | Convert C statement to ATS expression.
interpretStatementExp :: C.CStat -> St.State IEnv AExpr
interpretStatementExp (C.CCompound [] items _) = do
  let items' = takeReturn items -- Items before return
  let ret = pickReturn items -- A item may be return
  decls <- concat <$> mapM interpretBlockItemDecl items'
  exp <- mapM interpretBlockItemExp ret
  return $ A.Let dummyPos (A.ATS decls) exp
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
interpretCDerivedDeclr :: C.CDerivedDeclr -> St.State IEnv AArgs
interpretCDerivedDeclr (C.CFunDeclr (Right (decls, _)) _ _) = do
  args <- mapM go decls
  return $ Just args
  where
    go :: C.CDecl -> St.State IEnv AArg
    go (C.CDecl specs [(Just (C.CDeclr (Just ident) _ _ _ _), _, _)] _) = do
      let name = applyRenames ident
      let aType = baseTypeOf specs
      iEnvRecordDeclUsedVar name aType
      return $ A.Arg (A.Both name aType)
    go (C.CDecl specs [] _) =
      return $ A.Arg (A.Second (baseTypeOf specs))
interpretCDerivedDeclr dDeclr =
  traceShow dDeclr undefined

-- | Convert C function definition to ATS declaration.
interpretFunction :: C.CFunDef -> St.State IEnv ADecl
interpretFunction (C.CFunDef specs (C.CDeclr (Just ident) [derived] _ _ _) _ body _) = do
  let fname = applyRenames ident
  args <- interpretCDerivedDeclr derived
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
  iEnvClearDeclUsedVar
  interpretFunction f
perDecl (C.CDeclExt d) = do
  iEnvClearDeclUsedVar
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
