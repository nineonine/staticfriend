module Session.Service where

import Session.Request
import Session.Snippet
import Session.State

loadSourceFiles :: SnippetRequest -> IO SessionState
loadSourceFiles snipReq = do
    let (inFile,outFile) = getLangFiles snipReq
    srcContents <- readFile inFile
    targetContents  <- readFile outFile
    return (mkSessionState srcContents targetContents)

parseSessionRequest :: SessionRequest -> SnippetRequest
parseSessionRequest SessionRequest{..} = SnippetRequest src target prog opt
    where
    src    :: SourceLang    = read session_source_in
    target :: TargetLang    = read session_source_out
    prog   :: SampleProgram = read session_program
    opt    :: OptLevel      = read session_optimization
