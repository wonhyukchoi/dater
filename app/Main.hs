{-
 - REPL adapted from Stephen Diehl
 - https://www.stephendiehl.com/llvm/#full-source-1
 -}
module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import System.Exit(die)

import Parser
import Eval

main :: IO ()
main = do
  putStrLn "Welcome to dater, date calculations done easy!"
  runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "$>"
      case minput of
        Nothing    -> return ()
        Just ":q"  -> outputStrLn "Goodbye."
        Just input -> (liftIO $ process input) >> loop

process :: String -> IO ()
process line = do
  let res = eval <$> (parse line)
  case res of 
    Left err -> print err
    Right ex -> mapM_ print ex
