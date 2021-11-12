module Lexer
    ( tokenize,
      Op(..)
    ) where

data Op = Plus | Minus

tokenize :: IO ()
tokenize = putStrLn "foo"
