{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------

module Eval (eval) where

-----------------------------------------------------------------------------

import Control.Monad (liftM, liftM2)

import Data.Time ( Day
                 , CalendarDiffDays(..)
                 , fromGregorianValid
                 , addGregorianDurationRollOver
                 , addGregorianYearsRollOver
                 , diffGregorianDurationRollOver
                 , toGregorian
                 , fromGregorian
                 , zonedTimeToLocalTime
                 , getZonedTime
                 , localDay
                 )

import Language (DateExpr(..), Operator (..), YMD(..))

-----------------------------------------------------------------------------

data DateError = DateError String

instance Show DateError where
  show (DateError val) = unwords ["Error:", "Date", val, "is invalid!"]

eval :: DateExpr -> IO (Either DateError YMD)
eval = \case
  Date ymd        -> return $ return ymd
  Today           -> do
    let zonedTime2Gregorian = toGregorian . localDay . zonedTimeToLocalTime
    (y, m, d) <- liftM zonedTime2Gregorian getZonedTime
    return $ return $ YMD y (fromIntegral m) (fromIntegral d)
  Expr op lhs rhs -> do
   originalYMD <- eval lhs
   diffYMD     <- eval rhs
   let originalDay = originalYMD >>= ymd2Day
       addYMD diff = liftM day2Date $ addDays <$> originalDay <*> diff
   return $ case op of
     Add  -> addYMD diffYMD
     Sub  -> addYMD $ negateYMD <$> diffYMD
     Diff -> liftM2 diffDays originalDay $ (diffYMD >>= ymd2Day)

negateYMD :: YMD -> YMD
negateYMD (YMD y m d) = YMD (-y) (-m) (-d)

addDays :: Day -> YMD -> Day
addDays original (YMD y m d) = addMonthsAndDays $ addYears original
  where
    monthsToAdd      = fromIntegral m
    daysToAdd        = fromIntegral d
    addMonthsAndDays =
      addGregorianDurationRollOver $ (CalendarDiffDays monthsToAdd daysToAdd)
    addYears         = addGregorianYearsRollOver y

diffDays :: Day -> Day -> YMD
diffDays original diffDate = YMD year month d
  where
    const_NUM_MONTHS_IN_YR = 12
    (CalendarDiffDays m d) = diffGregorianDurationRollOver original diffDate
    (year, month)          = divMod m const_NUM_MONTHS_IN_YR

ymd2Day :: YMD -> Either DateError Day
ymd2Day ymd = let year'  = fromIntegral $ year  ymd
                  month' = fromIntegral $ month ymd
                  day'   = fromIntegral $ day   ymd
              in case fromGregorianValid year' month' day' of
                Nothing  -> Left $ DateError $ show ymd
                Just day -> return day

day2Date :: Day -> YMD
day2Date day = let (y, m, d) = toGregorian day 
               in YMD y (fromIntegral m) (fromIntegral d)
