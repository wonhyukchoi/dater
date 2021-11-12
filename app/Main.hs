module Main where

import Lexer
import Parser
import SemanticChecking
import Eval

greeting :: IO ()
greeting = putStrLn "Welcome to dater, date calculations done easy!"

main :: IO ()
main = greeting
