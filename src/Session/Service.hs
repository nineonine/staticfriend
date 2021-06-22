module Session.Service where

import Prelude hiding (readFile, lines)
import Protolude (readFile, lines, when)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map


import Analysis.InfoItem
import Session.Request
import Session.Snippet
import Session.Response
import X86.Parser

loadSourceFiles :: SnippetRequest -> InfoItemLookup -> IO SessionState
loadSourceFiles snipReq lkp = do
    let (inFile,outFile) = getLangFiles snipReq
    srcContents <- readFile inFile
    targetContents  <- readFile outFile
    let (ast,errs) = parseX86Source targetContents
    when (not $ null errs) (putStrLn ("[WARN] Parsing Errors: " <> show errs))
    let targetWithMeta = Map.fromList (zip [1..] (lines targetContents))
    return (mkSessionState srcContents targetContents targetWithMeta lkp)

parseSessionRequest :: SessionRequest -> SnippetRequest
parseSessionRequest SessionRequest{..} = SnippetRequest src target prog opt
    where
    src    :: SourceLang    = read (T.unpack session_source)
    target :: TargetLang    = read (T.unpack session_target)
    prog   :: SampleProgram = read (T.unpack session_program)
    opt    :: OptLevel      = read (T.unpack session_optimization)
