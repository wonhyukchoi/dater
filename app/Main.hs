module Main where

import System.Environment(getArgs)
import System.IO(hFlush, stdout)
import System.Exit(die)
import Parser
import Eval

main :: IO ()
main = do
  args <- getArgs
  if null args then do
    putStrLn "Welcome to dater, date calculations done easy!"
    until_ (readPrompt "dater$>") putStrLn (== ":q")
  else
    die "Unrecognized arguments"

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
  where flushStr str = putStr str >> hFlush stdout

until_ :: (Monad m) => m a -> (a -> m()) -> (a -> Bool) -> m ()
until_ prompt action isQuit = do
  input <- prompt
  case isQuit input of
    True -> return ()
    _    -> action input >> until_ prompt action isQuit
