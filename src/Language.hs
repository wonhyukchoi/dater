module Language ( Expr (..)
                , NumOp (..)
                , DiffOp (..)
                , Date(..)
                , YMD(..)
                ) where

-----------------------------------------------------------------------------

import Data.List (intercalate)

-----------------------------------------------------------------------------

data Expr =
    DateExpr Date
  | NumOp NumOp Date YMD
  | DiffOp DiffOp Date Date

data NumOp = Add | Sub deriving (Show, Eq)

data DiffOp = Diff deriving (Show, Eq)

data Date = 
    Date YMD
  | Today
  deriving (Show, Eq)

data YMD = YMD { year  :: Integer
               , month :: Integer
               , day   :: Integer
               }
          deriving (Eq)

instance Show YMD where
  show (YMD y m d) = intercalate "/" $ map show [y, m, d]
