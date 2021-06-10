{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.Lazy as L

import Network.Wai.Middleware.Static
import Web.Scotty

import Data.Monoid (mconcat)

main :: IO ()
main = do
    let indexHTML = "static/index.html"
    scotty 9999 $ do
        middleware $ staticPolicy (noDots >-> addBase "static")

        get "/" $ do
            file indexHTML

        get "/session" $ do
            file indexHTML
