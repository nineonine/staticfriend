module Session.Logger where

import Protolude
import qualified Data.Text as T

data LogLevel = Info | Warn | Error | Fatal deriving (Show, Eq)

doLog :: LogLevel -> Text -> IO ()
doLog logLvl msg = putText (mconcat [
    "[", T.pack $ map toUpper (show logLvl), "] ", msg
    ])
