module ParseExpensesDoc where

import Text.Printf (printf)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (find, groupBy, concatMap)
import Data.Function (on)
import Data.Hashable (Hashable(..))

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
              deriving (Eq)



data Entry = Entry
  { entryDate       :: (Int, Int, Int)    -- (y,m,d)
  , entryPrice      :: (Int, Int, String) -- (dlr,cents,cur)
  , entryRemark     :: String
  , entryCategories :: [Category]
  } deriving (Show, Eq)



data EntryCmp = Different -- different (date,price,remark)
              | Matched Entry -- Just take the latest/most specific
              | Conflicted Entry Entry



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
categoryFromString "" = Uncategorised
categoryFromString s = Category s



instance Hashable Category where
  hashWithSalt s Uncategorised = s
  hashWithSalt s (Category c) = hashWithSalt s c



instance Show Category where
  show = stringFromCategory



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



compareEntries :: Entry -> Entry -> EntryCmp
compareEntries freshEntry oldEntry =
  let key e = (entryDate e, entryPrice e, entryRemark e)
      isAllUncat e = all (Uncategorised ==) (entryCategories e)
  in
    if key freshEntry == key oldEntry then
      -- Roughly, if freshEntry is all-Uncat, then
      -- just return the oldEntry.
      if isAllUncat freshEntry then
        Matched oldEntry
      -- Otherwise, if oldEntry is all-Uncat, great.
      --            otherwise, conflict!
      else if isAllUncat oldEntry then
        Matched freshEntry
      else
        Conflicted freshEntry oldEntry
    else
      Different



-- TODO: I'd prefer if this were more maintainable
-- `mergeEntryLists` is analogous to DropBox syncing.
--
-- It only handles cases where the (date,price,remark) are unchanged,
-- and only the Categorisation has been changed.
-- i.e. can't handle cases where remark is changed, or entries are
-- removed.
mergeEntryLists :: [Entry] -> [Entry] -> [Entry]
mergeEntryLists freshEntries oldEntries =
  let key e = (entryDate e, entryPrice e, entryRemark e)
      -- Helper assumes the two input lists have the same date.
      helper freshEntries oldEntries result =
        case (freshEntries, oldEntries) of
          ([], xs) -> result ++ xs
          (xs, []) -> result ++ xs
          (x:xs, y:ys) ->
            let -- if item exists in list, return Just of combination of both,
                -- otherwise, return Nothing
                aux it ls =
                  case find (\e -> key e == key it) ls of
                    Just it' ->
                      -- gotta combine `it`, `it'`.
                      case compareEntries it it' of
                        Matched res        -> Just res
                        -- `Conflicted` shouldn't happen,
                        -- but resolve arbitrarily.
                        Conflicted lhs rhs -> Just lhs
                        Different          -> Nothing
                    Nothing  -> Nothing
            in
              case (aux x (y:ys), aux y (x:xs)) of
                (Nothing, _) ->
                  -- Head of LHS doesn't appear in RHS; prefer that.
                  helper xs (y:ys) (result ++ [x])
                (Just x, Nothing) ->
                  -- Head of RHS doesn't appear in LHS; prefer that.
                  helper (x:xs) ys (result ++ [y])
                (Just x, Just y) ->
                  -- Both heads occur in other side,
                  -- if x,y the same, then output both,
                  if key x == key y then
                    helper xs ys (result ++ [x])
                  else
                    -- Could take either LHS or RHS; just pick LHS
                    -- (Need to filter first x from ys)
                    let filterFirst [] = []
                        filterFirst (y:ys) | key x == key y = ys
                        filterFirst ls = ls
                    in
                      helper xs (y:filterFirst ys) (result ++ [x])

      -- Group [Entry] into [(Date,[Entry])],
      -- then 'merge' using the above helper function when both dates equal
      -- (or concat the [Entry] from the earlier Date)
      keyedEntryGroups :: [Entry] -> [((Int, Int, Int), [Entry])]
      keyedEntryGroups entries = map (\es -> (entryDate $ head es, es))
                                     $ groupBy ((==) `on` entryDate) entries
      keyedFreshEntries = keyedEntryGroups freshEntries
      keyedOldEntries   = keyedEntryGroups oldEntries

      mergeAux :: Ord date => [(date, [Entry])] -> [(date, [Entry])] -> [Entry]
      mergeAux freshEntries oldEntries =
        case (freshEntries, oldEntries) of
          (xs, []) -> concatMap snd xs
          ([], xs) -> concatMap snd xs
          ((d1,es1):xs, (d2,es2):ys) | d1 < d2 ->
            es1 ++ mergeAux xs oldEntries
          ((d1,es1):xs, (d2,es2):ys) | d1 > d2 ->
            es2 ++ mergeAux freshEntries ys
          ((d1,es1):xs, (d2,es2):ys) ->
            -- Both the same date; merge using helper
            helper es1 es2 [] ++ mergeAux xs ys
  in
    mergeAux keyedFreshEntries keyedOldEntries
