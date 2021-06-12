{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as L

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
            sessionState <- liftAndCatchIO $ do
                putStrLn "[INFO] Loading snippets from file system"
                dummyLoad
            json sessionState
