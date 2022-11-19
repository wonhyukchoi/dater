module Expr (Expr(..), Operator(..), Date(..)) where

-----------------------------------------------------------------------------

data Expr = Expr Operator Date Date deriving (Show)

data Operator = Add | Sub deriving (Show)

data Date = YMD { year  :: Integer
                , month :: Integer
                , day   :: Integer
                }
          | Now
          deriving (Show)
