module Session.Snippet where

import System.FilePath.Posix ((</>), (<.>))

data SourceLang = Haskell | C deriving Show

srcLangPath :: SourceLang -> FilePath
srcLangPath Haskell = "./hs"
srcLangPath C = "./c"

data TargetLang = X86 | LLVM deriving Show

targetLangPath :: FilePath
targetLangPath = "./build"

progFileName :: SampleProgram -> FilePath
progFileName Enum = "enum"
progFileName HelloWorld = "hello_world"
progFileName Fib = "fib"
progFileName Loop = "loop"

data SampleProgram
  = Enum
  | HelloWorld
  | Fib
  | Loop
  deriving Show

data SnippetRequest = SnippetRequest SourceLang TargetLang SampleProgram

getLangFiles :: SnippetRequest -> (FilePath, FilePath)
getLangFiles (SnippetRequest srcLang targetLang prog) =
    (srcFile,targetFile)
    where
    srcFileExt = case srcLang of
        Haskell -> "hs"
        C -> "c"
    targetFileExt = case srcLang of
        Haskell -> "dump-asm"
        C -> "s"
    srcFile = (srcLangPath srcLang) </> (progFileName prog) <.> srcFileExt
    targetFile = targetLangPath </> (progFileName prog) <.> targetFileExt
