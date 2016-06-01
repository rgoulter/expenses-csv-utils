module ParseExpensesDoc where

import Text.Printf (printf)
import Data.Maybe (fromMaybe)

import Control.Monad (void, forM_)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.Char as C

import qualified ParseDateDirective as D
import qualified ParseExpenseDirective as E

import Text.CSV as CSV



data LineDirective = DateCmd D.DateDirective | ExpCmd E.Expense deriving (Show, Eq)


-- For now, we'll just have categories as strings
data Category = Uncategorised
              | Category String



data Entry = Entry
  { entryDate       :: (Int, Int, Int)    -- (y,m,d)
  , entryPrice      :: (Int, Int, String) -- (dlr,cents,cur)
  , entryRemark     :: String
  , entryCategories :: [Category]
  }



-- PARSER



sc :: Parser ()
sc = hidden . skipMany $ choice [void spaceChar,
                                 void eol,
                                 L.skipLineComment "#"]



parseExpensesFile :: Parser [LineDirective]
parseExpensesFile =
  some $ (sc *> (DateCmd <$> D.dateDirective <* sc <?> "Date directive") <|>
                (ExpCmd  <$> E.expense <* sc <?> "Expense directive"))



signedInteger :: Parser Integer
signedInteger = L.signed sc D.integer


-- for getting from CSV field,
-- e.g. -1.23 to (-1, 23), 3 to (3, 0)
price :: Parser (Int, Int)
price =
  do dollars <- signedInteger
     cents <- fromMaybe 0 <$> optional (D.symbol "." *> signedInteger)
     return (fromIntegral dollars, fromIntegral cents)



-- UTILITY FUNCTIONS



entryFromExpense :: (Int, Int, Int) -> E.Expense -> Entry
entryFromExpense (y,m,d) exp =
  Entry { entryDate       = (y,m,d)
        , entryPrice      = (dollars, cents, cur)
        , entryRemark     = E.expenseRemark exp
        -- MAGIC: 2x categories.
        , entryCategories = [ Uncategorised
                            , Uncategorised
                            ]
        }
  where
    amount  = E.expenseAmount exp
    mult    = case E.expenseDirection exp of
               E.Spent -> (1 *)
               E.Received -> ((-1) *)
    dollars = mult $ E.moneyDollar amount
    cents   = E.moneyCents amount

    -- MAGIC: Implicit currency is SGD if not given.
    cur     = fromMaybe "SGD" (E.moneyCurrency amount)



stringFromCategory :: Category -> String
stringFromCategory Uncategorised = "Uncategorised"
stringFromCategory (Category c) = c



categoryFromString :: String -> Category
categoryFromString "Uncategorised" = Uncategorised
categoryFromString s = Category s



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
  let init = ((-1, -1, -1), D.Mon, [])

      -- Fold over a (Date, Day, GatheredRows)
      (_, _, rows) =
        foldl (\(date, day, rows) lineD ->
                 case lineD of
                   -- For ExpenseDirectives, simply add to list of 'rows'.
                   ExpCmd exp ->
                     (date, day, entryFromExpense date exp : rows)

                   -- For DateDirectives, increment/set the date/day.
                   DateCmd dateDir ->
                     let (date', day') = D.nextDate (date, day) dateDir
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
  (y, m, d)        <- parseMaybe D.date dateStr
  return Entry { entryDate       = (y,m,d)
               , entryPrice      = (dollars, cents, cur)
               , entryRemark     = remark
               -- May-be want to 'pad' this to 2?
               , entryCategories = map categoryFromString categories
               }

-- If insufficient number of fields, Nothing.
entryFromRecord _ =
  Nothing

