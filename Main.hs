module Main where

import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import Text.CSV (printCSV)

import Control.Monad (void, forM_)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.Char as C

import ParseDateDirective hiding (sc)
import ParseExpenseDirective hiding (sc)



data LineDirective = DateCmd DateDirective | ExpCmd Expense deriving (Show)



sc :: Parser ()
sc = hidden . skipMany $ choice [void spaceChar,
                                 void eol,
                                 L.skipLineComment "#"]



parseExpensesFile :: Parser [LineDirective]
parseExpensesFile =
  some $ (sc *> (DateCmd <$> try dateDirective <* sc) <|>
                (ExpCmd  <$> try expense <* sc))



main :: IO ()
main = do
  args <- getArgs
  if length args == 2 then
    let [inputF, outputF] = args in
    process inputF outputF
  else
    putStrLn "Please run with <input.txt> <output.csv>"



rowFromExp :: (Int, Int, Int) -> Expense -> [String]
rowFromExp (y,m,d) exp =
  [date, price, cur, remark, "Uncategorised"]
  where
      date   = printf "%4d-%02d-%02d" y m d
      amount = expenseAmount exp
      mult = case (expenseDirection exp) of
               Spent -> (1 *)
               Received -> ((-1) *)
      price  = printf "%d.%d" (mult $ moneyDollar amount) (moneyCents amount)
      cur    = fromMaybe "SGD" (moneyCurrency amount)
      remark = expenseRemark exp



process :: String -> String -> IO ()
process inputF outputF = do
  -- Parse the input file to list of [DateDir | ExpDir]
  result <- parseFromFile parseExpensesFile inputF

  case result of
    Left err -> print err
    Right xs -> do
      let (_, _, rows) = foldl (\(date, day, rows) lineD -> case lineD of
                                  ExpCmd exp -> (date, day, rowFromExp date exp : rows)
                                  DateCmd dateDir ->
                                    let (date', day') = nextDate (date, day) dateDir
                                    in (date', day', rows))
                               ((-1, -1, -1), Mon, [])
                               xs
          rows' = reverse rows
          outp = printCSV rows'
      writeFile outputF outp


