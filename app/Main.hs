module Main where

import Lib
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

main :: IO ()
main = do
  run $ bazz >> bizz >> bizz >> err

data Env =
  Env {
    host :: String
  , port :: Int
  }
  deriving (Eq, Show)

type App =
  ExceptT
    String
    (ReaderT Env
      (StateT [String] IO))

run :: Show a => App a -> IO ()
run app = do
  let env = Env "myhost" 42
      rei = runExceptT app
      r = runReaderT rei $ env
      s = runStateT r $ ["Initializing"]
  (ei, st) <- s -- (Either String a, [String])   
  putStrLn $ "Final state: " ++ show st
  case ei of
    Left err -> putStrLn $ "Error: " ++ err
    Right a  -> putStrLn $ "OK: " ++ (show a)
  return ()

track :: String -> App ()
track t = do
  s <- get
  put $ s ++ [t]

err :: App ()
err = do
  track "Error"
  ExceptT $ return $ Left "foo"

bazz :: App Int
bazz = return 42

bizz :: App Int
bizz = do
  e <- ask
  let p = port e
  track "Bizzing"
  return $ 42 + p

