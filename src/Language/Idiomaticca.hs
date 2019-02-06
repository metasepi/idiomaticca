module Language.Idiomaticca
    ( interpretTranslationUnit
    ) where

import Language.ATS as ATS
import Language.C as C

interpretExpr :: C.CExpr -> ATS.Expression AlexPosn
interpretExpr (C.CConst const) = case const of
  C.CIntConst int _ -> ATS.IntLit $ fromInteger $ C.getCInteger int
  _ -> undefined
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
  ATS.ATS $ fmap perDecl cDecls
