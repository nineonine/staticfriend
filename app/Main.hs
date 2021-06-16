{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Network.Wai.Middleware.Static
import Web.Scotty

import StaticFriend

main :: IO ()
main = do
    let indexHTML = "static/index.html"
    scotty 9999 $ do
        middleware $ staticPolicy (noDots >-> addBase "static")

        get "/" $ do
            file indexHTML

        get "/session" $ do
            file indexHTML

        post "/api/session" $ do
            req :: SnippetRequest <- parseSessionRequest <$> jsonData
            sessionState <- liftAndCatchIO $ do
                putStrLn ("[INFO] Processing Session Request: " <> show req)
                res :: SessionState <- loadSourceFiles req
                return res
            json sessionState
