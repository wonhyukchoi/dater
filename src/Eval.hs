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

import Language ( Expr (..)
                , NumOp (..)
                , DiffOp (..)
                , Date(..)
                , YMD(..)
                )
  
import Debug.Trace (trace)

-----------------------------------------------------------------------------

newtype DateError = DateError String

instance Show DateError where
  show (DateError val) = unwords ["Error:", "Date", val, "is invalid!"]

eval :: Expr -> IO (Either DateError YMD)
eval = \case
  DateExpr date     -> evalDate date
  DiffOp _ lhs rhs  -> do
    lhs' <- evalDate lhs
    rhs' <- evalDate rhs
    return $ liftM2 diffDays (lhs' >>= ymd2Day) (rhs' >>= ymd2Day)
  NumOp op date ymd -> do
    originalYMD <- evalDate date
    let diffYMD  = case op of
                     Add -> ymd
                     Sub -> negateYMD ymd
        original = originalYMD >>= ymd2Day
    return $ day2YMD <$> liftM2 addDays original (return diffYMD)

evalDate :: Date -> IO (Either DateError YMD)
evalDate = \case
  Date ymd -> return $ validateYMD ymd
  Today    -> do
    let zonedTime2Gregorian = toGregorian . localDay . zonedTimeToLocalTime
    (y, m, d) <- fmap zonedTime2Gregorian getZonedTime
    return $ return $ YMD y (fromIntegral m) (fromIntegral d)

addDays :: Day -> YMD -> Day
addDays original (YMD y m d) = addMonthsAndDays $ addYears original
  where
    monthsToAdd      = fromIntegral m
    daysToAdd        = fromIntegral d
    addMonthsAndDays =
      addGregorianDurationRollOver $ CalendarDiffDays monthsToAdd daysToAdd
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

day2YMD :: Day -> YMD
day2YMD day = let (y, m, d) = toGregorian day
               in YMD y (fromIntegral m) (fromIntegral d)

negateYMD :: YMD -> YMD
negateYMD (YMD y m d) = YMD (-y) (-m) (-d)

validateYMD :: YMD -> Either DateError YMD
validateYMD ymd = ymd2Day ymd >> return ymd
