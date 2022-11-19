module Eval
    ( eval
    ) where

import Parser
import Data.Time

eval = undefined

-- data ErrorType = DateError String deriving Show

-- getDayIfValid :: Date -> Either ErrorType Day
-- getDayIfValid date@(Date y m d) = case fromGregorianValid (fromIntegral y) m d of 
--     Nothing  -> Left $ DateError $ show date ++ " is invalid"
--     Just day -> Right $ day

-- eval :: (Op, Date, Date) -> Either ErrorType Date
-- eval (Plus, fstDate, sndDate)  = eval' <$> getDayIfValid fstDate <*> (Right sndDate)
--   where
--     eval' refDate (Date y m d) = Date (fromIntegral y') m' d'
--       where
--         monthDayToAdd = CalendarDiffDays (fromIntegral m) (fromIntegral d)
--         monthDayAdded = addGregorianDurationRollOver monthDayToAdd refDate
--         (y', m', d')  = toGregorian $ addGregorianYearsRollOver (fromIntegral y) monthDayAdded
-- eval (Minus, fstDate, sndDate) = eval' <$> getDayIfValid fstDate <*> getDayIfValid sndDate
--   where 
--     eval' fstDate sndDate = Date y' m' d'
--       where
--         (CalendarDiffDays m d) = diffGregorianDurationRollOver fstDate sndDate
--         (y, month)    = divMod m 12
--         [y', m', d']  = map fromIntegral [y, month, d]
