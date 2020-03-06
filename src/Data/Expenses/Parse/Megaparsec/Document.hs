module Data.Expenses.Parse.Megaparsec.Document
  ( modelFromAst
  , parseExpensesFile
  , withFile
  ) where

import Control.Monad (forM_, void)

import Data.Void (Void)

import Data.Either (Either(..), partitionEithers)

import qualified Data.Time.Calendar.Compat as DT

import Text.Megaparsec
  ( ParseError
  , anySingle
  , between
  , choice
  , eof
  , errorBundlePretty
  , hidden
  , manyTill
  , parseErrorPretty
  , runParser
  , sepEndBy
  , skipMany
  , someTill
  , try
  , withRecovery
  , (<?>)
  , (<|>)
  )
import Text.Megaparsec.Char (eol, spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Expenses.Parse.Megaparsec.Entry
import Data.Expenses.Parse.Megaparsec.Types
  (AST(..), LineDirective(..), Parser, RawLineDirective)
import qualified Data.Expenses.Parse.Megaparsec.DateDirective as PD
import qualified Data.Expenses.Parse.Megaparsec.ExpenseDirective as PE
import Data.Expenses.Expense
  (nextDate)
import Data.Expenses.Types (Model, modelFromEntries)




-- PARSER



scn :: Parser ()
scn = hidden . skipMany $ choice [ void spaceChar
                                 , L.skipLineComment "#"
                                 ]



lineDirective :: Parser LineDirective
lineDirective =
  (DateCmd
   <$> PD.dateDirective <* scn
   <?> "Date directive") <|>
  (ExpCmd
   <$> PE.expense <* scn
   <?> "Expense directive")



recover :: ParseError String Void -> Parser RawLineDirective
recover err =
  Left err <$ (try restOfLine <|> lastLine)
    where
      restOfLine = void $ manyTill anySingle eol
      lastLine = void $ someTill (void anySingle) eof



parseExpensesFile :: Parser AST
parseExpensesFile =
  AST <$> between scn eof (sepEndBy rawLine scn)
   where
     rawLine = withRecovery recover (Right <$> lineDirective)



-- UTILITY FUNCTIONS



modelFromAst :: AST -> Either [ParseError String Void] Model
modelFromAst (AST eitherLineDirectives) =
  f partitionedEitherDirectives
    where
  f ([], lineDirectives) =
    Right (modelFromEntries $ entriesFromDirectives lineDirectives)
  f (parseErrors, _) =
    Left parseErrors
  partitionedEitherDirectives = partitionEithers eitherLineDirectives



entriesFromDirectives :: [LineDirective] -> [Entry]
entriesFromDirectives directives =
  let
    initial = ((-1, -1, -1), DT.Monday, [])

    -- Fold over a (Date, Day, GatheredRows)
    (_, _, directiveRows) =
      foldl (\(date, day, rows) lineD ->
               case lineD of
                 -- For ExpenseDirectives, simply add to list of 'rows'.
                 ExpCmd expense ->
                   (date, day, entryFromExpense date expense : rows)

                 -- For DateDirectives, increment/set the date/day.
                 DateCmd dateDir ->
                   let (date', day') = nextDate (date, day) dateDir
                   in  (date', day', rows))
            initial
            directives
    rows' = reverse directiveRows
  in
  rows'



withFile :: String -> (Model -> IO()) -> IO ()
withFile inputF f = do
  -- Parse the input file to list of [DateDir | ExpDir]
  rawResult <- runParser parseExpensesFile inputF <$> readFile inputF

  case rawResult of
    Left err ->
      putStrLn $ errorBundlePretty err

    Right result ->
      case modelFromAst result of
        Left errors ->
          forM_ errors $ putStrLn . parseErrorPretty
        Right directives ->
          f directives
