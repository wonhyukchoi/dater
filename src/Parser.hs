{-# LANGUAGE FlexibleContexts #-}

module Parser
    ( parse
    , Date(..)
    , Op(..)
    ) where

import Data.List(intercalate)
import Control.Applicative((<|>))

import Text.Parsec.String(Parser)
import Text.Parsec(try)

import Data.Time

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

-- FIXME: More sane way to do lexing
parseDate :: Parser Date
parseDate = try parseDateLiteral
            <|> parseDay
            <|> parseWeek
            <|> parseMonth
            <|> parseYear

parseDateLiteral :: Parser Date
parseDateLiteral = do
  year <- tokenizeInt
  Parsec.spaces
  month <- tokenizeInt
  Parsec.spaces
  day <- tokenizeInt
  return $ Date (fromIntegral year) (fromIntegral month) (fromIntegral day)

-- FIXME: DRY
parseDay :: Parser Date
parseDay = do
  Parsec.string "day"
  Parsec.spaces
  num <- tokenizeInt
  return $ Date 0 0 num

parseWeek :: Parser Date
parseWeek = do
  Parsec.string "week"
  Parsec.spaces
  num <- tokenizeInt
  return $ Date 0 0 (7 * num)

parseMonth :: Parser Date
parseMonth = do
  Parsec.string "month"
  Parsec.spaces
  num <- tokenizeInt
  return $ Date 0 num 0

parseYear :: Parser Date
parseYear = do
  Parsec.string "year"
  Parsec.spaces
  num <- tokenizeInt
  return $ Date num 0 0

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
