module Main where

import Control.Monad
import System.FilePath
import System.Environment
import Language.C as C
import Language.ATS as ATS
import Language.Idiomaticca

main :: IO ()
main = do
  args <- getArgs
  when (length args < 1) undefined -- xxx
  let file = head args

  Right cAst <- C.parseCFilePre file
  let atsAst = interpretTranslationUnit cAst
  putStrLn $ ATS.printATS atsAst
