{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Middleware.Cors
import Web.Scotty

main :: IO ()
main = do
  scotty 9000 $ do
    middleware simpleCors
    get "/hello/:name" $ do
      name <- param "name"
      html $ mconcat [ "<h1>Hello ", name, " from Scotty!</h1><hr/>"]
