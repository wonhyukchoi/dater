{-# LANGUAGE FlexibleContexts #-}

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
        adtify '-' = Minus

tokenizeInt :: Parser Int
tokenizeInt = do
  n <- Parsec.many1 Parsec.digit
  return $ read n

-- TODO: a sane way to do lexing
parseDate :: Parser Date
parseDate = do
  year <- tokenizeInt
  Parsec.spaces
  month <- tokenizeInt
  Parsec.spaces
  day <- tokenizeInt
  return $ Date (fromIntegral year) (fromIntegral month) (fromIntegral day)

parseExpr :: Parser (Op, Date, Date)
parseExpr = do
  Parsec.spaces
  date1 <- parseDate
  Parsec.spaces
  operand <- tokenizeOp
  Parsec.spaces
  date2 <- parseDate
  Parsec.spaces
  return $ (operand, date1, date2)

basicParser :: Parser a -> String -> Either Parsec.ParseError a
basicParser p = Parsec.parse p ""

parse :: String -> Either Parsec.ParseError (Op, Date, Date)
parse = basicParser parseExpr
