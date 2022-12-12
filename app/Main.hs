module Main where

import Control.Monad.Trans (liftIO)
import System.Console.Haskeline ( runInputT
                                , defaultSettings
                                , getInputLine
                                , outputStrLn
                                )

import System.IO (hPutStrLn, stderr, stdout)
import System.Console.ANSI
  ( Color (Red, Blue)
  , ColorIntensity (Vivid)
  , ConsoleLayer (Foreground)
  , SGR (Reset, SetColor)
  , hSetSGR
  )

import Dater (process)

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
        Just input -> liftIO (printResult input) >> loop

printResult :: String -> IO ()
printResult line = do
  result <- process line
  case result of
    Left  err  -> printErr err
    Right expr -> putStrLn expr

printInfo :: IO ()
printInfo = do
  hSetSGR  stdout [ SetColor Foreground Vivid Blue ]
  putStrLn info
  hSetSGR  stdout [ Reset ]
  where info = unlines [ "Welcome to dater, date calculations done easy!"
                       , "Example Usages:"
                       , "dater $> today +  week 9"
                       , "dater $> 2022/11/22 - 0/11/1"
                       , "dater $> today <> 1997 / 4 / 10"
                       ]

printErr :: String -> IO ()
printErr s = do
  hSetSGR   stderr [ SetColor Foreground Vivid Red ]
  hPutStrLn stderr s
  hSetSGR   stderr [ Reset ]
