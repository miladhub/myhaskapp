module Main where

import Lib
import Control.Monad.Reader

data Env =
  Env {
    host :: String
  , port :: Int
  }
  deriving (Eq, Show)

type App a = ReaderT Env IO a

main :: IO ()
main = do
  let e = Env "myhost" 100
  run e dummy
  run e foo

run :: Show a => Env -> App a -> IO ()
run e app = do
  putStrLn "Running..."
  a <- runReaderT app $ e
  putStrLn $ show a
  return ()

dummy :: App Int
dummy = do
  e <- ask
  liftIO $ putStrLn (host e)
  return 42

foo :: App String
foo = return "foo"
