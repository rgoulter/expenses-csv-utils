module Data.Expenses.Expense
  ( DateDirective(DateDir)
  , Day(Mon, Tue, Wed, Thu, Fri, Sat, Sun)
  , Direction(..)
  , Expense(..)
  , Money(..)
  , addDays
  , dayOfWeek
  , numDaysAfter
  , nextDate
  )
where

import qualified Data.Time.Calendar as DT

import Data.Expenses.Types
  ( DateDirective(DateDir)
  , Day(Mon, Tue, Wed, Thu, Fri, Sat, Sun)
  , Direction(..)
  , Expense(..)
  , Money(..)
  )



dayOfWeek :: Day -> Int
dayOfWeek Mon = 0
dayOfWeek Tue = 1
dayOfWeek Wed = 2
dayOfWeek Thu = 3
dayOfWeek Fri = 4
dayOfWeek Sat = 5
dayOfWeek Sun = 6



-- d1 + (numDaysAfter d1 d2) = d2
-- e.g. "Tue is 1 day after Mon; the next Mon is 6 days after Tue".
numDaysAfter :: Day -> Day -> Int
numDaysAfter d1 d2 = (7 + dayOfWeek d2 - dayOfWeek d1) `mod` 7



addDays :: (Int, Int, Int) -> Int -> (Int, Int, Int)
addDays (y, m, d) dd =
  (fromIntegral y', m', d')
  where day = DT.fromGregorian (fromIntegral y) m d
        day' = DT.addDays (fromIntegral dd) day
        (y', m', d') = DT.toGregorian day'



nextDate :: ((Int, Int, Int), Day) -> DateDirective -> ((Int, Int, Int), Day)
nextDate ((y,m,d), dy) (DateDir Nothing dy') =
  -- Need to calculate how many days dy' is after dy.
  let diff = numDaysAfter dy dy'
      (y', m', d') = addDays (y, m, d) diff
  in ((y', m', d'), dy')



nextDate ((y,m,d), dy) (DateDir (Just (y',m',d')) dy') =
  -- Simply just use the new date/day
  ((y', m', d'), dy')
