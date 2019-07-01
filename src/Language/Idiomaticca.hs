module Language.Idiomaticca
    ( interpretTranslationUnit
    ) where

import Language.ATS as A
import Language.C as C

binop :: C.CBinaryOp -> A.Expression AlexPosn -> A.Expression AlexPosn -> A.Expression AlexPosn
binop op lhs rhs = case op of
  C.CMulOp -> A.Binary A.Mult lhs rhs
  C.CDivOp -> A.Binary A.Div lhs rhs
  -- xxx C.CRmdOp ...
  C.CAddOp -> A.Binary A.Add lhs rhs
  C.CSubOp -> A.Binary A.Sub lhs rhs
  _ -> undefined

interpretExpr :: C.CExpr -> A.Expression AlexPosn
interpretExpr (C.CConst const) = case const of
  C.CIntConst int _ -> A.IntLit $ fromInteger $ C.getCInteger int
  _ -> undefined
interpretExpr (C.CBinary op lhs rhs _) =
  binop op (interpretExpr lhs) (interpretExpr rhs)
interpretExpr _ = undefined

interpretStatement :: C.CStat -> A.Expression AlexPosn
interpretStatement (C.CCompound [] [C.CBlockStmt statement] _) =
  interpretStatement statement
interpretStatement (C.CReturn (Just expr) _) =
  interpretExpr expr
interpretStatement _ = undefined

interpretFunction :: CFunDef -> A.Declaration AlexPosn
interpretFunction (C.CFunDef _ (C.CDeclr (Just ident) _ _ _ _) _ body _) =
  A.Impl Nothing -- implArgs
           (A.Implement -- _impl
             undefined -- pos
             [] -- preUniversalsI
             [] -- implicits
             [] -- universalsI
             (A.Unqualified $ identToString ident) -- nameI
             (Just []) -- iArgs
             (Right $ interpretStatement body)) -- _iExpression
interpretFunction _ =
  undefined

perDecl :: C.CExtDecl -> A.Declaration AlexPosn
perDecl (C.CFDefExt f) = interpretFunction f
perDecl _ = undefined

interpretTranslationUnit :: C.CTranslUnit -> A.ATS AlexPosn
interpretTranslationUnit (C.CTranslUnit cDecls _) =
  A.ATS $ A.Include "\"share/atspre_staload.hats\"" : fmap perDecl cDecls
