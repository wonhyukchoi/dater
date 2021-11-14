module Eval
    ( eval
    ) where

import Lexer
import Parser
import Data.Time

type EvalResult = Either ErrorType Day
data ErrorType = DateError String

eval :: Op -> Date -> Date -> EvalResult
eval op fstDate sndDate = (eval' op) <$> checkValid fstDate <*> checkValid sndDate
  where
    checkValid date@(Date y m d) = case fromGregorianValid (fromIntegral y) m d of 
        Nothing  -> Left $ DateError $ show date ++ " is invalid"
        Just day -> Right $ day

eval' :: Op -> Day -> Day -> Day
eval' Minus fstDate sndDate = fromGregorian y (fromIntegral m') (fromIntegral d)
  where
    (CalendarDiffDays m d) = diffGregorianDurationRollOver fstDate sndDate
    (y, m') = divMod m 12
eval' Plus refDate toAdd = addGregorianYearsRollOver y monthDayAdded
  where
    (y, m, d)     = toGregorian toAdd
    monthDayToAdd = CalendarDiffDays (fromIntegral m) (fromIntegral d)
    monthDayAdded = addGregorianDurationRollOver monthDayToAdd refDate
