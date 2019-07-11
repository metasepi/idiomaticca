-- | Library to translate IDIOMATIC C into readable ATS.
module Language.Idiomaticca
    ( interpretTranslationUnit
    ) where

import Control.Applicative
import Control.Monad.State
import Debug.Trace
import qualified Language.ATS as A
import qualified Language.C as C

type Pos = A.AlexPosn

dummyPos :: Pos
dummyPos = A.AlexPn 0 0 0

type PreDecls = [String]

binop :: C.CBinaryOp -> A.Expression Pos -> A.Expression Pos -> A.Expression Pos
binop op lhs rhs = case op of
  C.CMulOp -> A.Binary A.Mult lhs rhs
  C.CDivOp -> A.Binary A.Div lhs rhs
  C.CAddOp -> A.Binary A.Add lhs rhs
  C.CSubOp -> A.Binary A.Sub lhs rhs
  C.CLeOp  -> A.Binary A.LessThan lhs rhs
  C.CGrOp  -> A.Binary A.GreaterThan lhs rhs
  C.CLeqOp -> A.Binary A.LessThanEq lhs rhs
  C.CGeqOp -> A.Binary A.GreaterThanEq lhs rhs
  C.CEqOp  -> A.Binary A.StaticEq lhs rhs
  C.CNeqOp -> A.Binary A.NotEq lhs rhs

applyRenames :: C.Ident -> String
applyRenames ident = case C.identToString ident of
  name -> name

singleSpec :: C.CTypeSpec -> A.Type Pos
singleSpec (C.CIntType _) = A.Named $ A.Unqualified "int"

baseTypeOf :: [C.CDeclSpec] -> A.Type Pos
baseTypeOf (C.CStorageSpec _:ss) = baseTypeOf ss
baseTypeOf [C.CTypeSpec spec] = singleSpec spec

interpretExpr :: C.CExpr -> A.Expression Pos
interpretExpr (C.CConst c) = case c of
  C.CIntConst int _ -> A.IntLit $ fromInteger $ C.getCInteger int
interpretExpr (C.CVar ident _) =
  A.NamedVal $ A.Unqualified $ applyRenames ident
interpretExpr (C.CBinary op lhs rhs _) =
  binop op (interpretExpr lhs) (interpretExpr rhs)
interpretExpr (C.CAssign C.CAssignOp expr1 expr2 _) =
  A.Binary A.Mutate (interpretExpr expr1) (interpretExpr expr2)
interpretExpr (C.CCall (C.CVar ident _) args _) =
  A.Call { A.callName = A.Unqualified $ applyRenames ident
         , A.callImplicits = []
         , A.callUniversals = []
         , A.callProofs = Nothing
         , A.callArgs = fmap interpretExpr args
         }

interpretDeclarations :: C.CDecl -> [A.Declaration Pos]
interpretDeclarations (C.CDecl specs declrs _) =
  fmap go declrs
  where
    go :: (Maybe C.CDeclr, Maybe C.CInit, Maybe C.CExpr) -> A.Declaration Pos
    go (Just (C.CDeclr (Just ident) [] Nothing [] _), initi, _) =
      A.Var { A.varT = Just $ baseTypeOf specs
            , A.varPat = A.UniversalPattern dummyPos (applyRenames ident) [] Nothing
            , A._varExpr1 = fmap cInit initi
            , A._varExpr2 = Nothing
            }
    cInit :: C.CInit -> A.Expression Pos
    cInit (C.CInitExpr expr _) = interpretExpr expr

interpretDeclarationsFunc :: C.CDecl -> State PreDecls (A.Declaration Pos)
interpretDeclarationsFunc (C.CDecl specs [(Just (C.CDeclr (Just ident) [derived] _ _ _), _, _)] _) = do
  let fname = applyRenames ident
  let args = interpretCDerivedDeclr derived
  modify (fname :)
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

interpretBlockItemDecl :: C.CBlockItem -> [A.Declaration Pos]
interpretBlockItemDecl (C.CBlockDecl decl) =
  interpretDeclarations decl
interpretBlockItemDecl (C.CBlockStmt statement) =
  [interpretStatementDecl statement]

interpretBlockItemExp :: C.CBlockItem -> A.Expression Pos
interpretBlockItemExp (C.CBlockStmt statement) =
  interpretStatementExp statement

makeVal :: A.Expression Pos -> A.Declaration Pos
makeVal aExpr = A.Val { A.add = A.None
                      , A.valT = Nothing
                      , A.valPat = Just (A.PLiteral (A.VoidLiteral dummyPos))
                      , A._valExpression = Just aExpr
                      }

interpretStatementDecl :: C.CStat -> A.Declaration Pos
interpretStatementDecl (C.CExpr (Just expr) _) =
  makeVal $ interpretExpr expr
interpretStatementDecl cIf@(C.CIf _ _ _ _) =
  makeVal $ interpretStatementExp cIf
interpretStatementDecl stat =
  traceShow stat undefined

makeCond :: C.CExpr -> A.Expression Pos
makeCond cond@(C.CBinary C.CLeOp  _ _ _) = interpretExpr cond
makeCond cond@(C.CBinary C.CGrOp  _ _ _) = interpretExpr cond
makeCond cond@(C.CBinary C.CLeqOp _ _ _) = interpretExpr cond
makeCond cond@(C.CBinary C.CGeqOp _ _ _) = interpretExpr cond
makeCond cond@(C.CBinary C.CEqOp  _ _ _) = interpretExpr cond
makeCond cond@(C.CBinary C.CNeqOp _ _ _) = interpretExpr cond
makeCond cond =
  A.Binary A.NotEq (interpretExpr cond) (A.IntLit 0)

interpretStatementExp :: C.CStat -> A.Expression Pos
interpretStatementExp (C.CCompound [] items _) =
  A.Let dummyPos
    (A.ATS $ concatMap interpretBlockItemDecl $ init items) -- xxx Need to support return
    (Just $ interpretBlockItemExp $ last items)
interpretStatementExp (C.CReturn (Just expr) _) =
  interpretExpr expr
interpretStatementExp (C.CExpr (Just expr) _) =
  interpretExpr expr
interpretStatementExp (C.CIf cond sthen selse _) =
  A.If (makeCond cond)
    (interpretStatementExp sthen)
    (fmap interpretStatementExp selse)

interpretCDerivedDeclr :: C.CDerivedDeclr -> A.Args Pos
interpretCDerivedDeclr (C.CFunDeclr (Right (decls, _)) _ _) =
  Just $ fmap go decls
  where
    go :: C.CDecl -> A.Arg Pos
    go (C.CDecl specs [(Just (C.CDeclr (Just ident) _ _ _ _), _, _)] _) =
      A.Arg (A.Both (applyRenames ident) (baseTypeOf specs))
    go (C.CDecl specs [] _) =
      A.Arg (A.Second (baseTypeOf specs))

interpretFunction :: C.CFunDef -> State PreDecls (A.Declaration Pos)
interpretFunction (C.CFunDef specs (C.CDeclr (Just ident) [derived] _ _ _) _ body _) = do
  let fname = applyRenames ident
  let args = interpretCDerivedDeclr derived
  s <- get
  if fname `elem` s then
    return A.Impl { A.implArgs = Nothing
                  , A._impl = A.Implement
                      dummyPos -- pos
                      [] -- preUniversalsI
                      [] -- implicits
                      [] -- universalsI
                      (A.Unqualified fname) -- nameI
                      args -- iArgs
                      (Right $ interpretStatementExp body) -- _iExpression
                  }
    else do
      modify (fname :)
      return $ A.Func dummyPos
        (A.Fun A.PreF { A.fname = A.Unqualified fname
                      , A.sig = Just ""
                      , A.preUniversals = []
                      , A.universals = []
                      , A.args = args
                      , A.returnType = Just $ baseTypeOf specs
                      , A.termetric = Nothing
                      , A._expression = Just $ interpretStatementExp body
                      })

perDecl :: C.CExtDecl -> State PreDecls (A.Declaration Pos)
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
     : evalState (mapM perDecl cDecls) ["main"]
