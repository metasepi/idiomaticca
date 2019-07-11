-- | Library to translate IDIOMATIC C into readable ATS.
module Language.Idiomaticca
    ( interpretTranslationUnit
    ) where

import Control.Applicative
import Control.Monad.State
import Debug.Trace
import qualified Data.Set as Set
import qualified Language.ATS as A
import qualified Language.C as C

type Pos = A.AlexPosn

dummyPos :: Pos
dummyPos = A.AlexPn 0 0 0

data IEnv = IEnv { iEnvDeclFuns :: Set.Set String
                 , iEnvDeclVars :: [(String, A.Type Pos)]
                 , iEnvUsedVars :: Set.Set String
                 }
  deriving (Show)

defaultIEnv :: IEnv
defaultIEnv = IEnv { iEnvDeclFuns = Set.singleton "main"
                   , iEnvDeclVars = []
                   , iEnvUsedVars = Set.empty
                   }

binop :: C.CBinaryOp -> A.Expression Pos -> A.Expression Pos -> State IEnv (A.Expression Pos)
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
  in return $ A.Binary op' lhs rhs

applyRenames :: C.Ident -> String
applyRenames ident = case C.identToString ident of
  name -> name

singleSpec :: C.CTypeSpec -> A.Type Pos
singleSpec (C.CIntType _) = A.Named $ A.Unqualified "int"

baseTypeOf :: [C.CDeclSpec] -> A.Type Pos
baseTypeOf (C.CStorageSpec _:ss) = baseTypeOf ss
baseTypeOf [C.CTypeSpec spec] = singleSpec spec

makeVal :: A.Expression Pos -> A.Declaration Pos
makeVal aExpr = A.Val { A.add = A.None
                      , A.valT = Nothing
                      , A.valPat = Just (A.PLiteral (A.VoidLiteral dummyPos))
                      , A._valExpression = Just aExpr
                      }

makeCond :: C.CExpr -> State IEnv (A.Expression Pos)
makeCond cond@(C.CBinary C.CLeOp  _ _ _) = interpretExpr cond
makeCond cond@(C.CBinary C.CGrOp  _ _ _) = interpretExpr cond
makeCond cond@(C.CBinary C.CLeqOp _ _ _) = interpretExpr cond
makeCond cond@(C.CBinary C.CGeqOp _ _ _) = interpretExpr cond
makeCond cond@(C.CBinary C.CEqOp  _ _ _) = interpretExpr cond
makeCond cond@(C.CBinary C.CNeqOp _ _ _) = interpretExpr cond
makeCond cond = do
  cond' <- interpretExpr cond
  return $ A.Binary A.NotEq cond' (A.IntLit 0)

interpretExpr :: C.CExpr -> State IEnv (A.Expression Pos)
interpretExpr (C.CConst c) = case c of
  C.CIntConst int _ -> return $ A.IntLit $ fromInteger $ C.getCInteger int
interpretExpr (C.CVar ident _) = do
  let name = applyRenames ident
  modify $ \s -> s { iEnvUsedVars = Set.insert name (iEnvUsedVars s) }
  return $ A.NamedVal $ A.Unqualified name
interpretExpr (C.CBinary op lhs rhs _) = do
  lhs' <- interpretExpr lhs
  rhs' <- interpretExpr rhs
  binop op lhs' rhs'
interpretExpr (C.CAssign C.CAssignOp expr1 expr2 _) = do
  expr1' <- interpretExpr expr1
  expr2' <- interpretExpr expr2
  return $ A.Binary A.Mutate expr1' expr2'
interpretExpr (C.CCall (C.CVar ident _) args _) = do
  args' <- mapM interpretExpr args
  return $ A.Call { A.callName = A.Unqualified $ applyRenames ident
                  , A.callImplicits = []
                  , A.callUniversals = []
                  , A.callProofs = Nothing
                  , A.callArgs = args'
                  }

interpretDeclarations :: C.CDecl -> State IEnv [A.Declaration Pos]
interpretDeclarations (C.CDecl specs declrs _) =
  mapM go declrs
  where
    go :: (Maybe C.CDeclr, Maybe C.CInit, Maybe C.CExpr) -> State IEnv (A.Declaration Pos)
    go (Just (C.CDeclr (Just ident) [] Nothing [] _), initi, _) = do
      let name = applyRenames ident
      let aType = baseTypeOf specs
      modify $ \s -> s { iEnvDeclVars = (name, aType) : iEnvDeclVars s }
      modify $ \s -> s { iEnvUsedVars = Set.insert name (iEnvUsedVars s) }
      initi' <- mapM cInit initi
      return $ A.Var { A.varT = Just aType
                     , A.varPat = A.UniversalPattern dummyPos name [] Nothing
                     , A._varExpr1 = initi'
                     , A._varExpr2 = Nothing
                     }
    cInit :: C.CInit -> State IEnv (A.Expression Pos)
    cInit (C.CInitExpr expr _) = interpretExpr expr

interpretDeclarationsFunc :: C.CDecl -> State IEnv (A.Declaration Pos)
interpretDeclarationsFunc (C.CDecl specs [(Just (C.CDeclr (Just ident) [derived] _ _ _), _, _)] _) = do
  let fname = applyRenames ident
  args <- interpretCDerivedDeclr derived
  modify $ \s -> s { iEnvDeclFuns = Set.insert fname (iEnvDeclFuns s) }
  return $ A.Func dummyPos
    (A.Fun A.PreF { A.fname = A.Unqualified fname
                  , A.sig = Just ""
                  , A.preUniversals = []
                  , A.universals = []
                  , A.args = args
                  , A.returnType = Just $ baseTypeOf specs
                  , A.termetric = Nothing
                  , A._expression = Nothing
                  })

interpretBlockItemDecl :: C.CBlockItem -> State IEnv [A.Declaration Pos]
interpretBlockItemDecl (C.CBlockDecl decl) =
  interpretDeclarations decl
interpretBlockItemDecl (C.CBlockStmt statement) = do
  statement' <- interpretStatementDecl statement
  return [statement']

interpretBlockItemExp :: C.CBlockItem -> State IEnv (A.Expression Pos)
interpretBlockItemExp (C.CBlockStmt statement) =
  interpretStatementExp statement

interpretStatementDecl :: C.CStat -> State IEnv (A.Declaration Pos)
interpretStatementDecl (C.CExpr (Just expr) _) = do
  expr' <- interpretExpr expr
  return $ makeVal expr'
interpretStatementDecl cIf@C.CIf{} = do
  cIf' <- interpretStatementExp cIf
  return $ makeVal cIf'
interpretStatementDecl stat =
  traceShow stat undefined

interpretStatementExp :: C.CStat -> State IEnv (A.Expression Pos)
interpretStatementExp (C.CCompound [] items _) = do
  -- xxx Need to support return
  decls' <- fmap concat $ mapM interpretBlockItemDecl $ init items
  exp' <- interpretBlockItemExp $ last items
  return $ A.Let dummyPos (A.ATS decls') (Just exp')
interpretStatementExp (C.CReturn (Just expr) _) =
  interpretExpr expr
interpretStatementExp (C.CExpr (Just expr) _) =
  interpretExpr expr
interpretStatementExp (C.CIf cond sthen selse _) = do
  cond' <- makeCond cond
  sthen' <- interpretStatementExp sthen
  selse' <- mapM interpretStatementExp selse
  return $ A.If cond' sthen' selse'

interpretCDerivedDeclr :: C.CDerivedDeclr -> State IEnv (A.Args Pos)
interpretCDerivedDeclr (C.CFunDeclr (Right (decls, _)) _ _) = do
  args <- mapM go decls
  return $ Just args
  where
    go :: C.CDecl -> State IEnv (A.Arg Pos)
    go (C.CDecl specs [(Just (C.CDeclr (Just ident) _ _ _ _), _, _)] _) = do
      let name = applyRenames ident
      let aType = baseTypeOf specs
      modify $ \s -> s { iEnvDeclVars = (name, aType) : iEnvDeclVars s }
      modify $ \s -> s { iEnvUsedVars = Set.insert name (iEnvUsedVars s) }
      return $ A.Arg (A.Both name aType)
    go (C.CDecl specs [] _) = do
      return $ A.Arg (A.Second (baseTypeOf specs))

interpretFunction :: C.CFunDef -> State IEnv (A.Declaration Pos)
interpretFunction (C.CFunDef specs (C.CDeclr (Just ident) [derived] _ _ _) _ body _) = do
  let fname = applyRenames ident
  args <- interpretCDerivedDeclr derived
  s <- get
  body' <- interpretStatementExp body
  if fname `Set.member` iEnvDeclFuns s then
    return A.Impl { A.implArgs = Nothing
                  , A._impl = A.Implement
                      dummyPos -- pos
                      [] -- preUniversalsI
                      [] -- implicits
                      [] -- universalsI
                      (A.Unqualified fname) -- nameI
                      args -- iArgs
                      (Right body') -- _iExpression
                  }
    else do
      modify $ \s -> s { iEnvDeclFuns = Set.insert fname (iEnvDeclFuns s) }
      return $ A.Func dummyPos
        (A.Fun A.PreF { A.fname = A.Unqualified fname
                      , A.sig = Just ""
                      , A.preUniversals = []
                      , A.universals = []
                      , A.args = args
                      , A.returnType = Just $ baseTypeOf specs
                      , A.termetric = Nothing
                      , A._expression = Just body'
                      })

perDecl :: C.CExtDecl -> State IEnv (A.Declaration Pos)
perDecl (C.CFDefExt f) = interpretFunction f
perDecl (C.CDeclExt d) =
  A.Extern dummyPos <$> interpretDeclarationsFunc d

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

-- | convert C tranlsation unit to ATS declarations.
interpretTranslationUnit :: C.CTranslUnit -> A.ATS Pos
interpretTranslationUnit (C.CTranslUnit cDecls _) =
  A.ATS $ fmap A.Comment copyleftComment
    ++ A.Include "\"share/atspre_staload.hats\""
     : A.Load { A.static = True
              , A.withOctothorpe = False
              , A.qualName = Just "UN"
              , A.fileName = "\"prelude/SATS/unsafe.sats\""
              }
     : evalState (mapM perDecl cDecls) defaultIEnv
