module Main where

import Control.Monad
import System.FilePath
import System.Environment
import Data.Version hiding (Version (..))
import Paths_idiomaticca
import Options.Applicative
import qualified Language.C as C
import qualified Language.ATS as A
import Language.Idiomaticca

wrapper :: ParserInfo Command
wrapper = info (helper <*> versionInfo <*> command')
  (fullDesc
   <> progDesc "The idiomaticca translates IDIOMATIC C into readable ATS."
   <> header "idiomaticca - a tool to translate IDIOMATIC C into readable ATS")

versionInfo :: Parser (a -> a)
versionInfo = infoOption ("idiomaticca version: " ++ showVersion version) (short 'V' <> long "version" <> help "Show version")

data Command = Trans { _file :: String }
             | DumpAts { _file :: String }

command' :: Parser Command
command' = hsubparser
  (command "trans" (info trans (progDesc "Translate C into ATS"))
  <> command "dumpats" (info dumpats (progDesc "Dump ATS's AST"))
  )

trans :: Parser Command
trans = Trans
  <$> targetP mempty id "C filename"

dumpats :: Parser Command
dumpats = DumpAts
  <$> targetP mempty id "ATS filename"

targetP :: Mod ArgumentFields String -> (Parser String -> a) -> String -> a
targetP completions' f s = f
  (argument str
   (metavar "TARGET"
    <> help ("Targets to " <> s)
    <> completions'))

main :: IO ()
main = execParser wrapper >>= run

run :: Command -> IO ()
run (Trans file) = do
  Right cAst <- C.parseCFilePre file
  let atsAst = interpretTranslationUnit cAst
  putStrLn $ A.printATS atsAst
run (DumpAts file) = do
  atsSrc <- readFile file
  let Right atsAst = A.parse atsSrc
  putStrLn $ show atsAst
