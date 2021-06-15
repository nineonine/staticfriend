module Session.Service where

import Prelude hiding (readFile, lines)
import Protolude (readFile, lines)
import qualified Data.Text as T
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
    src    :: SourceLang    = read (T.unpack session_source)
    target :: TargetLang    = read (T.unpack session_target)
    prog   :: SampleProgram = read (T.unpack session_program)
    opt    :: OptLevel      = read (T.unpack session_optimization)
