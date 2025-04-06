module Main where

import qualified Network.Wai.Handler.Warp as Warp

import Web

main :: IO ()
main = do
  let port = 8080
  putStrLn $ "Starting server on http://localhost:" ++ show port ++ "/hello"
  putStrLn $ "Starting server on http://localhost:" ++ show port ++ "/bye"
  Warp.run port application
