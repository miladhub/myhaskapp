module Main where

import Lib
import Control.Monad.Reader

main :: IO ()
main = do
  run (Env "myhost" 100) app

app :: App Int
app = do
  e <- ask
  liftIO $ putStrLn (host e)
  return 42
