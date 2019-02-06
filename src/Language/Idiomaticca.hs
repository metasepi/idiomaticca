module Language.Idiomaticca
    ( interpretTranslationUnit
    ) where

import Language.ATS as ATS
import Language.C as C

binop :: C.CBinaryOp -> ATS.Expression AlexPosn -> ATS.Expression AlexPosn -> ATS.Expression AlexPosn
binop op lhs rhs = case op of
  C.CMulOp -> ATS.Binary ATS.Mult lhs rhs
  C.CDivOp -> ATS.Binary ATS.Div lhs rhs
  -- xxx C.CRmdOp ...
  C.CAddOp -> ATS.Binary ATS.Add lhs rhs
  C.CSubOp -> ATS.Binary ATS.Sub lhs rhs
  _ -> undefined

interpretExpr :: C.CExpr -> ATS.Expression AlexPosn
interpretExpr (C.CConst const) = case const of
  C.CIntConst int _ -> ATS.IntLit $ fromInteger $ C.getCInteger int
  _ -> undefined
interpretExpr (C.CBinary op lhs rhs _) =
  binop op (interpretExpr lhs) (interpretExpr rhs)
interpretExpr _ = undefined

interpretStatement :: C.CStat -> ATS.Expression AlexPosn
interpretStatement (C.CCompound [] [C.CBlockStmt statement] _) =
  interpretStatement statement
interpretStatement (C.CReturn (Just expr) _) =
  interpretExpr expr
interpretStatement _ = undefined

interpretFunction :: CFunDef -> ATS.Declaration AlexPosn
interpretFunction (C.CFunDef _ (C.CDeclr (Just ident) _ _ _ _) _ body _) =
  ATS.Impl Nothing -- implArgs
           (ATS.Implement -- _impl
             undefined -- pos
             [] -- preUniversalsI
             [] -- implicits
             [] -- universalsI
             (ATS.Unqualified $ identToString ident) -- nameI
             (Just []) -- iArgs
             (Right $ interpretStatement body)) -- _iExpression
interpretFunction _ =
  undefined

perDecl :: C.CExtDecl -> ATS.Declaration AlexPosn
perDecl (C.CFDefExt f) = interpretFunction f
perDecl _ = undefined

interpretTranslationUnit :: C.CTranslUnit -> ATS.ATS AlexPosn
interpretTranslationUnit (C.CTranslUnit cDecls _) =
  ATS.ATS $ ATS.Include "\"share/atspre_staload.hats\"" : fmap perDecl cDecls
