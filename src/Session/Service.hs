module Session.Service where

import qualified Data.Map.Strict as Map

import Session.Request
import Session.Snippet
import Session.State

loadSourceFiles :: SnippetRequest -> IO SessionState
loadSourceFiles snipReq = do
    let (inFile,outFile) = getLangFiles snipReq
    srcContents <- readFile inFile
    targetContents  <- readFile outFile
    let targetWithMeta = Map.fromList (zip [1..] (lines targetContents))
    return (mkSessionState srcContents targetContents targetWithMeta)

parseSessionRequest :: SessionRequest -> SnippetRequest
parseSessionRequest SessionRequest{..} = SnippetRequest src target prog opt
    where
    src    :: SourceLang    = read session_source
    target :: TargetLang    = read session_target
    prog   :: SampleProgram = read session_program
    opt    :: OptLevel      = read session_optimization
