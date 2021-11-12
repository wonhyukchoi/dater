module Eval
    ( wrapper
    ) where

import Lexer
import Parser
import Data.Time

type EvalResult = Either ErrorType String
data ErrorType = DateError String

wrapper op fstDate sndDate
  | checkValid fstDate == Nothing = Left $ dateError fstDate
  | checkValid sndDate == Nothing = Left $ dateError sndDate
  | otherwise                     = Right $ show $ eval op fstDate' sndDate'
  where
    fstDate' = let (Date y m d) = fstDate in fromGregorian y m d
    sndDate' = let (Date y m d) = sndDate in fromGregorian y m d
    checkValid (Date y m d) = fromGregorianValid y m d
    dateError x = DateError $ show x ++ " is invalid"

eval :: Op -> Day -> Day -> Day
eval Minus fstDate sndDate = fromGregorian y (fromIntegral m') (fromIntegral d)
  where
    (CalendarDiffDays m d) = diffGregorianDurationRollOver fstDate sndDate
    (y, m') = divMod m 12
eval Plus refDate toAdd = addGregorianYearsRollOver y monthDayAdded
  where
    (y, m, d)     = toGregorian toAdd
    monthDayToAdd = CalendarDiffDays (fromIntegral m) (fromIntegral d)
    monthDayAdded = addGregorianDurationRollOver monthDayToAdd refDate
