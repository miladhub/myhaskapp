module App where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

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

run :: Show a => Env -> App a -> IO ()
run env app = do
  let readt          = runExceptT app
      statet         = runReaderT readt $ env
      resultAndState = runStateT statet $ ["Initializing"]
  -- (Either String a, [String])
  (result, finalState) <- resultAndState
  putStrLn $ "Final state: " ++ show finalState
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right a  -> putStrLn $ "OK: " ++ (show a)
  return ()

getEnv :: App Env
getEnv = ask

track :: String -> App ()
track t = do
  s <- get
  put $ s ++ [t]

err :: App ()
err = do
  track "Error"
  ExceptT $ return $ Left "foo"

