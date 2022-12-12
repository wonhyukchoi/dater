
module Lib (process) where

-----------------------------------------------------------------------------

import Language (YMD)

import Parser (parseDater)

import Eval (eval)

-----------------------------------------------------------------------------

process :: String -> IO (Either String String)
process line = case parseDater line of
  Left err   -> return $ Left $ show err
  Right expr -> do
    result <- eval expr
    return $ case result of
      Left err   -> Left  $ show err
      Right date -> Right $ show date