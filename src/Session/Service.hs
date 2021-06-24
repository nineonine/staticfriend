module Session.Service where

import Prelude hiding (readFile, lines)
import Protolude (readFile, lines, when)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V


import Analysis.InfoItem
import Analysis.Glean
import Session.Logger
import Session.Request
import Session.Response
import Session.Snippet
import X86.Parser

loadSourceFiles :: SnippetRequest -> InfoItemLookup -> IO SessionState
loadSourceFiles snipReq lkp = do
    let (inFile,outFile) = getLangFiles snipReq
    srcContents <- readFile inFile
    targetContents <- readFile outFile
    let (ast,errs) = parseX86Source targetContents
    when (not $ null errs) $
        doLog Warn $ "Parsing Errors: " <> (T.pack $ show errs)
    let infoItemAnalysis =
            InfoItemAnalysis ( V.fromList (zip (lines targetContents)
                                               (gleanInfoItems ast lkp))
                             )
    return (mkSessionState srcContents targetContents infoItemAnalysis lkp)

parseSessionRequest :: SessionRequest -> SnippetRequest
parseSessionRequest SessionRequest{..} = SnippetRequest src target prog opt
    where
    src    :: SourceLang    = read (T.unpack session_source)
    target :: TargetLang    = read (T.unpack session_target)
    prog   :: SampleProgram = read (T.unpack session_program)
    opt    :: OptLevel      = read (T.unpack session_optimization)
