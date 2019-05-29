module Main where

import Lib
import Control.Monad.Reader

main :: IO ()
main = do
  let e = Env "myhost" 100
  run e app

app :: App Int
app = do
  e <- ask
  liftIO $ putStrLn (host e)
  return 42
