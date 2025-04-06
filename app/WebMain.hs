module Main where

import qualified Network.Wai.Handler.Warp as Warp

import Web

main :: IO ()
main = do
  let port = 8080
  putStrLn $ "Server starting on port " ++ show port
  Warp.run port application
