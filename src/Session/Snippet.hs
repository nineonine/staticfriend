module Session.Snippet where

import Prelude
import System.FilePath.Posix ((</>), (<.>))

data SourceLang = Haskell | C deriving (Show, Read)

srcLangPath :: SourceLang -> FilePath
srcLangPath Haskell = "./hs"
srcLangPath C = "./c"

data TargetLang = X86 | LLVM deriving (Show, Read)

targetLangPath :: FilePath
targetLangPath = "./build"

data OptLevel = O0 | O1 | O2 | O3 deriving (Show, Read)

data SampleProgram
  = Enum
  | HelloWorld
  | Fib
  | Loop
  deriving (Show, Read)

data SnippetRequest = SnippetRequest {
    source_lang :: SourceLang
  , target_lang :: TargetLang
  , sample_prog :: SampleProgram
  , opt_level   :: OptLevel
  } deriving Show

getLangFiles :: SnippetRequest -> (FilePath, FilePath)
getLangFiles SnippetRequest{..} =
    (srcFile,targetFile)
    where
    srcFileExt = case source_lang of
        Haskell -> "hs"
        C -> "c"
    targetFileExt = case target_lang of
        X86 -> case source_lang of
            Haskell -> "dump-asm"
            C -> "s"
        LLVM -> "ll"

    srcFile = (srcLangPath source_lang) </> (show sample_prog) <.> srcFileExt
    targetFile = targetLangPath </> (show sample_prog) <.> targetFileExt
