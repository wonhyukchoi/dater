module Parser (parseDater) where

-----------------------------------------------------------------------------

import qualified Text.Parsec as Parsec

import qualified Text.Parsec.Token as Token

import qualified Text.Parsec.Expr as Expr

import Control.Applicative ((<|>))

import Control.Monad (liftM)

import Data.Functor.Identity (Identity)

import Text.Parsec.Language (emptyDef)

import Text.Parsec.String (Parser)

import Language (Operator(..), DateExpr(..), YMD(..))

-----------------------------------------------------------------------------

daterDef :: Token.LanguageDef a
daterDef = emptyDef { Token.opStart         = Parsec.oneOf "+-/<"
                    , Token.opLetter        = Parsec.oneOf "+-/>"
                    , Token.caseSensitive   = False
                    , Token.reservedNames   = [ "today"
                                              , "day"
                                              , "month"
                                              , "week"
                                              , "year"
                                              , "_"
                                              ]
                    , Token.reservedOpNames = ["+", "-", "/", "<>"]
                    }

lexer :: Token.GenTokenParser String p Identity
lexer = Token.makeTokenParser daterDef

integer :: Parser Integer
integer = Token.integer lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

ymdParser :: Parser YMD
ymdParser = do
  year  <- integer
  _     <- reservedOp "/"
  month <- integer
  _     <- reservedOp "/"
  day   <- integer
  return $ YMD year month day

dayParser :: Parser YMD
dayParser = do
  _   <- reserved "day"
  num <- integer
  return $ YMD 0 0 num

weekParser :: Parser YMD
weekParser = do
  _   <- reserved "week"
  num <- integer
  return $ YMD 0 0 $ 7 * num

monthParser :: Parser YMD
monthParser = do
  _   <- reserved "month"
  num <- integer
  return $ YMD 0 1 0

yearParser :: Parser YMD
yearParser = do
  _   <- reserved "year"
  num <- integer
  return $ YMD 1 0 0

sugarParser :: Parser YMD
sugarParser = dayParser <|> weekParser <|> monthParser <|> yearParser

todayParser :: Parser DateExpr
todayParser = reserved "today" >> return Today

dateTerm :: Parser DateExpr
dateTerm = todayParser <|> dateParser'
  where dateParser' = liftM Date (ymdParser <|> sugarParser)

dateOperators :: [[Expr.Operator String () Identity DateExpr]]
dateOperators =
  [
    [ Expr.Infix  (reservedOp "+"  >> return (Expr Add )) Expr.AssocLeft
    , Expr.Infix  (reservedOp "-"  >> return (Expr Sub )) Expr.AssocLeft
    , Expr.Infix  (reservedOp "<>" >> return (Expr Diff)) Expr.AssocLeft
    ]
  ]

dateParser :: Parser DateExpr
dateParser = Expr.buildExpressionParser dateOperators dateTerm

parseDater :: String -> Either Parsec.ParseError DateExpr
parseDater input = Parsec.parse (dateParser <* Parsec.eof) errMsg input
  where errMsg = "Parser failed for " ++ input
