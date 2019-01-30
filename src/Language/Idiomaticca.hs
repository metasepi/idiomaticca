module Language.Idiomaticca
    ( interpretTranslationUnit
    ) where

import Language.ATS as ATS
import Language.C as C

statementTrans :: C.CStat -> ATS.Expression AlexPosn
statementTrans (C.CCompound [] [C.CBlockStmt statement] _) = statementTrans statement
statementTrans (C.CReturn (Just (C.CConst (C.CIntConst ret _))) _) = ATS.IntLit $ fromInteger $ C.getCInteger ret
statementTrans _ = undefined

extDeclTrans :: C.CExtDecl -> ATS.Declaration AlexPosn
extDeclTrans (C.CFDefExt (C.CFunDef _ (C.CDeclr (Just funcIdent) _ _ _ _) _ statement _)) =
  ATS.Impl Nothing (ATS.Implement
                    undefined -- pos
                    [] -- preUniversalsI
                    [] -- implicits
                    [] -- universalsI
                    (ATS.Unqualified $ identToString funcIdent) -- nameI
                    (Just []) -- iArgs
                    (Right $ statementTrans statement)) -- _iExpression
extDeclTrans _ = undefined

interpretTranslationUnit :: C.CTranslUnit -> ATS.ATS AlexPosn
interpretTranslationUnit (C.CTranslUnit cDecls _) =
  ATS.ATS $ fmap extDeclTrans cDecls
