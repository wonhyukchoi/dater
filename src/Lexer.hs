module Lexer
    ( lexer
    , integer
    , parens
    , commaSep
    , reservedOp
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+","-"]
    style = emptyDef {
               Tok.reservedOpNames = ops
             }

integer :: Parser Integer
integer = Tok.integer lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
