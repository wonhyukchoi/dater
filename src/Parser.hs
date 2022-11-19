module Parser (parseDater) where

-----------------------------------------------------------------------------

import qualified Text.Parsec as Parsec

import qualified Text.Parsec.Token as Token

import Control.Applicative ((<|>))

import Data.Functor.Identity (Identity)

import Text.Parsec.Language (emptyDef)

import Text.Parsec.String (Parser)

import Expr (Expr(..), Operator(..), Date(..))

-----------------------------------------------------------------------------

daterDef :: Token.LanguageDef a
daterDef = emptyDef { Token.opStart         = Parsec.oneOf "+-/"
                    , Token.opLetter        = Parsec.oneOf "+-/"
                    , Token.caseSensitive   = False
                    , Token.reservedNames   = [ "now"
                                              ]
                    , Token.reservedOpNames = ["+", "-", "/"]
                    }

lexer :: Token.GenTokenParser String p Identity
lexer = Token.makeTokenParser daterDef

integer :: Parser Integer
integer = Token.integer lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

operatorParser :: Parser Operator
operatorParser =  (reservedOp "+" >> return Add)
              <|> (reservedOp "-" >> return Sub)

ymdParser :: Parser Date
ymdParser = do
  year  <- integer
  _     <- reservedOp "/"
  month <- integer
  _     <- reservedOp "/"
  day   <- integer
  return $ YMD year month day

dayParser :: Parser Date
dayParser = do
  _   <- reserved "day"
  num <- integer
  return $ YMD 0 0 num

weekParser :: Parser Date
weekParser = do
  _   <- reserved "week"
  num <- integer
  return $ YMD 0 0 $ 7 * num

monthParser :: Parser Date
monthParser = do
  _   <- reserved "month"
  num <- integer
  return $ YMD 0 1 0

yearParser :: Parser Date
yearParser = do
  _   <- reserved "year"
  num <- integer
  return $ YMD 1 0 0

sugarParser :: Parser Date
sugarParser = dayParser <|> weekParser <|> monthParser <|> yearParser

nowParser :: Parser Date
nowParser = reserved "now" >> return Now

dateParser :: Parser Date
dateParser = nowParser <|> ymdParser <|> sugarParser

exprParser :: Parser Expr
exprParser = (Parsec.spaces >>) $ do
  lhs <- dateParser
  op  <- operatorParser
  rhs <- dateParser
  return $ Expr op lhs rhs

parseDater :: String -> Either Parsec.ParseError Expr
parseDater input = Parsec.parse (exprParser <* Parsec.eof) errMsg input
  where errMsg = "Parser failed for " ++ input
