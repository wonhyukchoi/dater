module Main where

import Control.Monad.Trans
import System.Console.Haskeline

import System.IO (hPutStrLn, stderr, stdout)
import System.Console.ANSI
  ( Color (Red, Blue)
  , ColorIntensity (Vivid)
  , ConsoleLayer (Foreground)
  , SGR (Reset, SetColor)
  , hSetSGR
  )

import Parser (parseDater)
import Eval (eval)

main :: IO ()
main = do
  printInfo
  runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "dater $> "
      case minput of
        Nothing    -> return ()
        Just ":q"  -> outputStrLn "Goodbye."
        Just input -> (liftIO $ process input) >> loop

process :: String -> IO ()
process line = case parseDater line of
  Left err   -> printErr err
  Right expr -> do
    result <- eval expr
    case result of
      Left err   -> printErr err
      Right date -> print date

printInfo :: IO ()
printInfo = do
  hSetSGR   stdout [ SetColor Foreground Vivid Blue ]
  hPutStrLn stdout info
  hSetSGR   stdout [ Reset ] 
  where info = unlines [ "Welcome to dater, date calculations done easy!"
                       , "Example Usages:"
                       , "dater $> today +  week 9"
                       , "dater $> 2022/11/22 - 0/11/1"
                       , "dater $> today <> 1997 / 4 / 10"
                       ]


printErr :: (Show a) => a -> IO ()
printErr s = do
  hSetSGR   stderr [ SetColor Foreground Vivid Red ]
  hPutStrLn stderr $ show s
  hSetSGR   stderr [ Reset ] 
