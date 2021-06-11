module Session.Service where

import Session.Snippet
import Session.State

loadSourceFiles :: SnippetRequest -> IO SessionState
loadSourceFiles snipReq = do
    let (inFile,outFile) = getLangFiles snipReq
    srcContents <- readFile inFile
    targetContents  <- readFile outFile
    return (mkSessionState srcContents targetContents)

dummyLoad :: IO SessionState
dummyLoad = do
    loadSourceFiles (SnippetRequest Haskell X86 HelloWorld)
