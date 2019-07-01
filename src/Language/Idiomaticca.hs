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
interpretExpr (C.CConst const) = case const of
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

-- | convert C tranlsation unit to ATS declarations.
interpretTranslationUnit :: C.CTranslUnit -> A.ATS A.AlexPosn
interpretTranslationUnit (C.CTranslUnit cDecls _) =
  A.ATS $ A.Include "\"share/atspre_staload.hats\"" : fmap perDecl cDecls
