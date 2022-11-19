module Language (Operator(..), DateExpr(..), YMD(..)) where

-----------------------------------------------------------------------------

import Data.List (intercalate)

-----------------------------------------------------------------------------

data Operator = Add
              | Sub
              | Diff
              deriving (Show, Eq)

data DateExpr = 
            Date YMD
          | Today
          | Expr Operator DateExpr DateExpr
          deriving (Show, Eq)

data YMD = YMD { year  :: Integer
               , month :: Integer
               , day   :: Integer
               }
          deriving (Eq)

instance Show YMD where
  show (YMD y m d) = intercalate "/" $ map show [y, m, d]
