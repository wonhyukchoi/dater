module Eval
    ( eval
    ) where

import Parser
import Data.Time

-- TODO: shouldn't do fromGregorian check for things like (0,1,0)...

type EvalResult = Either ErrorType Date
data ErrorType = DateError String deriving Show

eval :: (Op, Date, Date) -> EvalResult
eval (op,fstDate,sndDate) = (eval' op) <$> checkValid fstDate <*> checkValid sndDate
  where
    checkValid date@(Date y m d) = case fromGregorianValid (fromIntegral y) m d of 
        Nothing  -> Left $ DateError $ show date ++ " is invalid"
        Just day -> Right $ day

eval' :: Op -> Day -> Day -> Date
eval' Minus fstDate sndDate = Date y' m' d'
  where
    (CalendarDiffDays m d) = diffGregorianDurationRollOver fstDate sndDate
    (y, month)    = divMod m 12
    [y', m', d']  = map fromIntegral [y, month, d]
eval' Plus refDate toAdd = Date (fromIntegral y') m' d'
  where
    (y, m, d)     = toGregorian toAdd
    monthDayToAdd = CalendarDiffDays (fromIntegral m) (fromIntegral d)
    monthDayAdded = addGregorianDurationRollOver monthDayToAdd refDate
    (y', m', d')  = toGregorian  $addGregorianYearsRollOver y monthDayAdded
