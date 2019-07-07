-- | Library to translate IDIOMATIC C into readable ATS.
module Language.Idiomaticca
    ( interpretTranslationUnit
    ) where

import qualified Language.ATS as A
import qualified Language.C as C

type Pos = A.AlexPosn

dummyPos :: Pos
dummyPos = A.AlexPn 0 0 0

binop :: C.CBinaryOp -> A.Expression Pos -> A.Expression Pos -> A.Expression Pos
binop op lhs rhs = case op of
  C.CMulOp -> A.Binary A.Mult lhs rhs
  C.CDivOp -> A.Binary A.Div lhs rhs
  C.CAddOp -> A.Binary A.Add lhs rhs
  C.CSubOp -> A.Binary A.Sub lhs rhs

applyRenames :: C.Ident -> String
applyRenames ident = case C.identToString ident of
  name -> name

singleSpec :: C.CTypeSpec -> A.Type Pos
singleSpec (C.CIntType _) = A.Named $ A.Unqualified "int"

baseTypeOf :: [C.CDeclSpec] -> A.Type Pos
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
    cInit (C.CInitExpr e _) = interpretExpr e

interpretBlockItemDecl :: C.CBlockItem -> [A.Declaration Pos]
interpretBlockItemDecl (C.CBlockDecl decl) =
  interpretDeclarations decl
interpretBlockItemDecl (C.CBlockStmt statement) =
  [interpretStatementDecl statement]

interpretBlockItemExp :: C.CBlockItem -> A.Expression Pos
interpretBlockItemExp (C.CBlockStmt statement) =
  interpretStatementExp statement

interpretStatementDecl :: C.CStat -> A.Declaration Pos
interpretStatementDecl (C.CExpr (Just expr) _) =
  A.Val { A.add = A.None
        , A.valT = Nothing
        , A.valPat = Just (A.PLiteral (A.VoidLiteral dummyPos))
        , A._valExpression = Just $ interpretExpr expr
        }

interpretStatementExp :: C.CStat -> A.Expression Pos
interpretStatementExp (C.CCompound [] items _) =
  A.Let dummyPos
    (A.ATS $ concat $ fmap interpretBlockItemDecl $ init items)
    (Just $ interpretBlockItemExp $ last items)
interpretStatementExp (C.CReturn (Just expr) _) =
  interpretExpr expr

interpretFunction :: C.CFunDef -> A.Declaration Pos
interpretFunction (C.CFunDef _ (C.CDeclr (Just ident) _ _ _ _) _ body _) =
  A.Impl Nothing -- implArgs
           (A.Implement -- _impl
             dummyPos -- pos
             [] -- preUniversalsI
             [] -- implicits
             [] -- universalsI
             (A.Unqualified $ C.identToString ident) -- nameI
             (Just []) -- iArgs
             (Right $ interpretStatementExp body)) -- _iExpression

perDecl :: C.CExtDecl -> A.Declaration Pos
perDecl (C.CFDefExt f) = interpretFunction f

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
  A.ATS $ fmap A.Comment copyleftComment ++
    A.Include "\"share/atspre_staload.hats\"" : fmap perDecl cDecls
