-- Adapted from
-- https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html
--
-- Parse strings like:
--   "2016-01-01 MON"
--   "TUE"
-- (Maybe yyyy-mm-mm) DAY
--
-- MON, TUE, WED, THURS, FRI, SAT, SUN
--  (also TUES, WEDS)

module ParseDateDirective where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Time.Calendar as DT



data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq)

data DateDirective = DateDir {
                       dateDirDate :: Maybe (Int, Int, Int),
                       dateDirDay  :: Day
                     } deriving (Show, Eq)



dayOfWeek :: Day -> Int
dayOfWeek Mon = 0
dayOfWeek Tue = 1
dayOfWeek Wed = 2
dayOfWeek Thu = 3
dayOfWeek Fri = 4
dayOfWeek Sat = 5
dayOfWeek Sun = 6

-- e.g. "Tue is 1 day after Mon; the next Mon is 6 days after Tue".
numDaysAfter :: Day -> Day -> Int
numDaysAfter d1 d2 = (7 + (dayOfWeek d2) - (dayOfWeek d1)) `mod` 7

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



sc :: Parser ()
sc = hidden . skipMany $ void spaceChar


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc



integer :: Parser Integer
integer = lexeme L.integer

dash :: Parser String
dash = symbol "-"



-- TODO: No need to be case-sensitive here.
day :: Parser Day
day =
  ((      string "MON")  *> pure Mon) <|>
  ((try $ string "TUE" <* skipMany (noneOf "\n\r\0")) *> pure Tue) <|>
  ((      string "WED" <* skipMany (noneOf "\n\r\0"))  *> pure Wed) <|>
  ((try $ string "THU" <* skipMany (noneOf "\n\r\0")) *> pure Thu) <|>
  ((      string "FRI")  *> pure Fri) <|>
  ((try $ string "SAT")  *> pure Sat) <|>
  ((try $ string "SUN")  *> pure Sun)

-- TODO: at the moment, not strict about `yyyy-mm-dd`
date :: Parser (Int, Int, Int)
date =
  do yyyy <- fromIntegral <$> integer
     void  dash
     mm <- fromIntegral <$> integer
     void  dash
     dd <- fromIntegral <$> integer
     return $ (yyyy, mm, dd)

dateDirective :: Parser DateDirective
dateDirective =
  do dt <- optional date
     dy <- day
     return $ DateDir dt dy
