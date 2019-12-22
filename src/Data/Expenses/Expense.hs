module Data.Expenses.Expense
  ( DateDirective(DateDir)
  , Direction(..)
  , Expense(..)
  , Money(..)
  , dayOfWeek
  , numDaysAfter
  , nextDate
  )
where

import qualified Data.Time.Calendar as DT

import Data.Expenses.Types
  ( DateDirective(DateDir)
  , Direction(..)
  , Expense(..)
  , Money(..)
  )



dayOfWeek :: DT.DayOfWeek -> Int
dayOfWeek DT.Monday = 0
dayOfWeek DT.Tuesday = 1
dayOfWeek DT.Wednesday = 2
dayOfWeek DT.Thursday = 3
dayOfWeek DT.Friday = 4
dayOfWeek DT.Saturday = 5
dayOfWeek DT.Sunday = 6



-- d1 + (numDaysAfter d1 d2) = d2
-- e.g. "Tue is 1 day after Mon; the next Mon is 6 days after Tue".
numDaysAfter :: DT.DayOfWeek -> DT.DayOfWeek -> Int
numDaysAfter d1 d2 = (7 + dayOfWeek d2 - dayOfWeek d1) `mod` 7



nextDate
  :: ((Int, Int, Int), DT.DayOfWeek)
  -> DateDirective
  -> ((Int, Int, Int), DT.DayOfWeek)
nextDate ((y, m, d), dy) (DateDir Nothing dy') =
  -- Need to calculate how many days dy' is after dy.
  let diff = numDaysAfter dy dy'
      day = DT.fromGregorian (fromIntegral y) m d
      day' = DT.addDays (fromIntegral diff) day
      (y', m', d') = DT.toGregorian day'
  in ((fromIntegral y', m', d'), dy')



nextDate _ (DateDir (Just day') dy') =
  -- Simply just use the new date/day
  ((fromIntegral y', m', d'), dy')
    where
     (y', m', d') = DT.toGregorian day'
