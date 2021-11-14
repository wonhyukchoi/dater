module Parser
    ( parse
    , Date(..)
    , Op(..)
    ) where

import Data.List(intercalate)

import Text.Parsec.String (Parser)

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer

data Op = Plus | Minus deriving (Eq, Show)

data Date = Date { year  :: Int
                 , month :: Int
                 , day   :: Int
                 }
instance Show Date where
  show (Date year month day) = intercalate "/" $ map show [year, month, day]

tokenizeOp :: Parser Op
tokenizeOp = do
  operand <- Parsec.oneOf "+-"
  return $ adtify operand
  where adtify '+' = Plus
        adtify _   = Minus

tokenizeInt :: Parser Integer
tokenizeInt = Tok.integer lexer

dateParse :: Parser Date
dateParse = do
  Parsec.char '('
  year <- tokenizeInt
  Parsec.char ','
  month <- tokenizeInt
  Parsec.char ','
  Parsec.char ')'
  day <- tokenizeInt
  return $ Date (fromIntegral year) (fromIntegral month) (fromIntegral day)

parse :: Parser (Date, Op, Date)
parse = do
  date1 <- dateParse
  operand <- tokenizeOp
  date2 <- dateParse
  return $ (date1, operand, date2)
