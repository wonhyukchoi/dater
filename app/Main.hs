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
                       , ""
                       , "Use + and - to add/subtrace from a date,"
                       , "and <> to find the difference between two dates."
                       , ""
                       , "Grammar:"
                       , "DATER := DATE (+/-) DIFF"
                       , "       | DATE  <>   DATE"
                       , "DATE  := year/month/day | today"
                       , "DIFF  := year/month/day"
                       , "       |  year  (n)"
                       , "       |  month (n)"
                       , "       |  day   (n)"
                       , ""
                       , "Example Usages:"
                       , "dater $> today +  week 9"
                       , "2023/8/5"
                       , "dater $> 2022/11/22 - 0/11/1"
                       , "2021/12/21"
                       , "dater $> today <> 1997 / 4 / 10"
                       , "26/1/24"
                       ]

printErr :: String -> IO ()
printErr s = do
  hSetSGR   stderr [ SetColor Foreground Vivid Red ]
  hPutStrLn stderr s
  hSetSGR   stderr [ Reset ]
