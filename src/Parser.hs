module Parser
    ( parse
    , Date(..)
    ) where

data Date = Date { year  :: Integer
                 , month :: Int
                 , day   :: Int
                 }

instance Show Date where
  show (Date year month day) = show year ++ "/" ++ show month ++ "/" ++ show day

parse :: IO ()
parse = putStrLn "foo"
