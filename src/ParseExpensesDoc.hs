module ParseExpensesDoc where

import Text.Printf (printf)
import Data.Maybe (fromMaybe, mapMaybe)

import Control.Monad (void, forM_)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.Char as C

import qualified ParseDateDirective as PD
import qualified ParseExpenseDirective as PE
import Expense (DateDirective, Expense(..),
                Day(Mon), nextDate,
                Direction(..))
import qualified Expense as E
import Entry

import Text.CSV as CSV



data LineDirective = DateCmd DateDirective | ExpCmd Expense deriving (Show, Eq)



-- PARSER



sc :: Parser ()
sc = hidden . skipMany $ choice [void spaceChar,
                                 void eol,
                                 L.skipLineComment "#"]



parseExpensesFile :: Parser [LineDirective]
parseExpensesFile =
  some $ (sc *> (DateCmd <$> PD.dateDirective <* sc <?> "Date directive") <|>
                (ExpCmd  <$> PE.expense <* sc <?> "Expense directive"))



signedInteger :: Parser Integer
signedInteger = L.signed sc PD.integer


-- for getting from CSV field,
-- e.g. -1.23 to (-1, 23), 3 to (3, 0)
price :: Parser (Int, Int)
price =
  do dollars <- signedInteger
     cents <- fromMaybe 0 <$> optional (PD.symbol "." *> signedInteger)
     return (fromIntegral dollars, fromIntegral cents)



-- UTILITY FUNCTIONS



recordFromEntry :: Entry -> CSV.Record
recordFromEntry entry =
  [date, price, cur, remark] ++ map stringFromCategory (entryCategories entry)
  where
    (y, m, d) = entryDate entry
    date   = printf "%4d-%02d-%02d" y m d
    (dollars, cents, cur) = entryPrice entry
    price  = printf "%d.%d" dollars cents
    remark = entryRemark entry



entriesFromDirectives :: [LineDirective] -> [Entry]
entriesFromDirectives directives =
  let init = ((-1, -1, -1), Mon, [])

      -- Fold over a (Date, Day, GatheredRows)
      (_, _, rows) =
        foldl (\(date, day, rows) lineD ->
                 case lineD of
                   -- For ExpenseDirectives, simply add to list of 'rows'.
                   ExpCmd exp ->
                     (date, day, entryFromExpense date exp : rows)

                   -- For DateDirectives, increment/set the date/day.
                   DateCmd dateDir ->
                     let (date', day') = nextDate (date, day) dateDir
                     in  (date', day', rows))
              init
              directives
      rows' = reverse rows
  in  rows'



recordsFromDirectives :: [LineDirective] -> [CSV.Record]
recordsFromDirectives directives =
  map recordFromEntry $ entriesFromDirectives directives



-- Pattern-match the Record's fields, ensures sufficient.
entryFromRecord :: CSV.Record -> Maybe Entry
entryFromRecord (dateStr:priceStr:cur:remark:categories) = do
  (dollars, cents) <- parseMaybe price priceStr
  (y, m, d)        <- parseMaybe PD.date dateStr
  return Entry { entryDate       = (y,m,d)
               , entryPrice      = (dollars, cents, cur)
               , entryRemark     = remark
               -- MAGIC assumption: 2 categories
               , entryCategories = take 2 $
                                   map categoryFromString categories ++ [Uncategorised, Uncategorised]
               }

-- If insufficient number of fields, Nothing.
entryFromRecord _ =
  Nothing



entriesFromCSV :: CSV.CSV -> [Entry]
entriesFromCSV =
  -- If any Record is malformed (for some reason),
  -- discard/ignore it.
  mapMaybe entryFromRecord

