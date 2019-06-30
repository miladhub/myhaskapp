module Others where

import App
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

type App' = ReaderT Env IO

run' :: Show a => Env -> App' a -> IO ()
run' e app = do
  putStrLn "Running..."
  a <- runReaderT app $ e
  putStrLn $ show a
  return ()

main' :: IO ()
main' = do
  run' (Env "myhost" 100) app'

app' :: App' Int
app' = do
  e <- ask
  liftIO $ putStrLn (host e)
  return 42

runFoo :: IO ()
runFoo = do
  putStrLn "Running..."
  let e = Env "myhost" 42
      s = runReaderT foo $ e
      ioa = runStateT s []
  (a, xs) <- ioa
  putStrLn $ show a
  return ()

type Foo = ReaderT Env (StateT [String] IO)

foo :: Foo Int
foo = do
  (Env host port) <- ask
  put ["foo" ++ host]
  liftIO $ putStrLn "fooing"
  return port

type Baz = ReaderT Env (StateT [String] (ExceptT String IO))

runBaz :: Show a => Baz a -> IO ()
runBaz baz = do
  putStrLn "Running..."
  let e = Env "myhost" 42
      s = runReaderT baz $ e
      exc = runStateT s []
      ioa = runExceptT exc
  es <- ioa
  case es of
    Left err -> putStrLn $ "Error: " ++ err
    Right (i, xs) -> putStrLn $ "OK: " ++ (show i)
  return ()

baz :: Baz Int
baz = do
  e <- ask
  put ["baz", host e]
  return 42

err :: Baz ()
err =
  let et = ExceptT $ return $ Left "foo"
      st = StateT  $ \_ -> et
      rt = ReaderT $ \_ -> st
  in rt

type Buz = ReaderT Env (ExceptT String (StateT [String] IO))

runBuz :: Show a => Buz a -> IO ()
runBuz buz = do
  putStrLn "Running..."
  let e = Env "myhost" 42
      exct = runReaderT buz $ e
      st = runExceptT exct
      ei = runStateT st []
  (e, s) <- ei
  putStrLn $ "Final state: " ++ show s
  case e of
    Left err -> putStrLn $ "Error: " ++ err
    Right i -> putStrLn $ "OK: " ++ (show i)
  return ()

buz :: Buz Int
buz = do
  e <- ask
  put ["buz", host e]
  err'
  return 42

err' :: Buz ()
err' = do
  let et = ExceptT $ return $ Left "foo"
      rt = ReaderT $ \_ -> et
  s <- get
  put $ s ++ ["error..."]
  rt

