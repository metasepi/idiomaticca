module Language.Idiomaticca
    ( interpretTranslationUnit
    ) where

import Language.ATS as ATS
import Language.C as C

interpretTranslationUnit :: C.CTranslUnit -> ATS.ATS AlexPosn
interpretTranslationUnit cAst =
  ATS.ATS [ATS.Impl
             Nothing
             (ATS.Implement
              undefined -- pos
              [] -- preUniversalsI
              [] -- implicits
              [] -- universalsI
              (ATS.Unqualified "main") -- nameI
              (Just []) -- iArgs
              (Right $ ATS.IntLit 0))] -- _iExpression
