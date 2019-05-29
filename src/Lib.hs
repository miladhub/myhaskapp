module Lib
    ( Env(..), App, run
    ) where

import Control.Monad.Reader

data Env =
  Env {
    host :: String
  , port :: Int
  }
  deriving (Eq, Show)

type App = ReaderT Env IO

run :: Show a => Env -> App a -> IO ()
run e app = do
  putStrLn "Running..."
  a <- runReaderT app $ e
  putStrLn $ show a
  return ()

