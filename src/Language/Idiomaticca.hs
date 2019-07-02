-- | Library to translate IDIOMATIC C into readable ATS.
module Language.Idiomaticca
    ( interpretTranslationUnit
    ) where

import qualified Language.ATS as A
import qualified Language.C as C

binop :: C.CBinaryOp -> A.Expression A.AlexPosn -> A.Expression A.AlexPosn -> A.Expression A.AlexPosn
binop op lhs rhs = case op of
  C.CMulOp -> A.Binary A.Mult lhs rhs
  C.CDivOp -> A.Binary A.Div lhs rhs
  -- xxx C.CRmdOp ...
  C.CAddOp -> A.Binary A.Add lhs rhs
  C.CSubOp -> A.Binary A.Sub lhs rhs
  _ -> undefined

interpretExpr :: C.CExpr -> A.Expression A.AlexPosn
interpretExpr (C.CConst c) = case c of
  C.CIntConst int _ -> A.IntLit $ fromInteger $ C.getCInteger int
  _ -> undefined
interpretExpr (C.CBinary op lhs rhs _) =
  binop op (interpretExpr lhs) (interpretExpr rhs)
interpretExpr _ = undefined

interpretStatement :: C.CStat -> A.Expression A.AlexPosn
interpretStatement (C.CCompound [] [C.CBlockStmt statement] _) =
  interpretStatement statement
interpretStatement (C.CReturn (Just expr) _) =
  interpretExpr expr
interpretStatement _ = undefined

interpretFunction :: C.CFunDef -> A.Declaration A.AlexPosn
interpretFunction (C.CFunDef _ (C.CDeclr (Just ident) _ _ _ _) _ body _) =
  A.Impl Nothing -- implArgs
           (A.Implement -- _impl
             undefined -- pos
             [] -- preUniversalsI
             [] -- implicits
             [] -- universalsI
             (A.Unqualified $ C.identToString ident) -- nameI
             (Just []) -- iArgs
             (Right $ interpretStatement body)) -- _iExpression
interpretFunction _ =
  undefined

perDecl :: C.CExtDecl -> A.Declaration A.AlexPosn
perDecl (C.CFDefExt f) = interpretFunction f
perDecl _ = undefined

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
interpretTranslationUnit :: C.CTranslUnit -> A.ATS A.AlexPosn
interpretTranslationUnit (C.CTranslUnit cDecls _) =
  A.ATS $ fmap A.Comment copyleftComment ++
    A.Include "\"share/atspre_staload.hats\"" : fmap perDecl cDecls
