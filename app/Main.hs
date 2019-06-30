module Main where

import App

main :: IO ()
main = do
  run (Env "myhost" 42) $ do
    bazz
    bizz
    bizz
    err

bazz :: App Int
bazz = return 42

bizz :: App Int
bizz = do
  e <- getEnv
  let p = port e
  track "Bizzing"
  return $ 42 + p

