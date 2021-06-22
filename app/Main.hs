{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import qualified Data.Text as T
import Network.Wai.Middleware.Static
import Web.Scotty

import StaticFriend

indexHTML :: String
indexHTML = "static/index.html"

main :: IO ()
main = do
    doLog Info "Starting staticfriend"
    doLog Info "Loading InfoItem Database"
    infoItemLookup <- getInfoItemLookup

    scotty 9999 $ do
        middleware $ staticPolicy (noDots >-> addBase "static")

        get "/" $ do
            file indexHTML

        get "/session" $ do
            file indexHTML

        post "/api/session" $ do
            req :: SnippetRequest <- parseSessionRequest <$> jsonData
            sessionState <- liftAndCatchIO $ do
                doLog Info ("Processing Session Request: " <> (T.pack $ show req))
                res :: SessionState <- loadSourceFiles req infoItemLookup
                return res
            json sessionState
