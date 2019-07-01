module Main where

import App

main :: IO ()
main = do
  run (Env "myhost" 42) $ do
    n <- num 6
    m <- num 7
    e <- getEnv
    let p = port e
    track "Multiplying"
    return $ n * m
    track "Failing"
    err
    track "Finally"

num :: Int -> App Int
num n = do
  track $ "Yielding " ++ (show n)
  return n

