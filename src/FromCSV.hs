module FromCSV where

import Text.CSV as CSV

import Entry



signedInteger :: Parser Integer
signedInteger = L.signed sc PD.integer


-- for getting from CSV field,
-- e.g. -1.23 to (-1, 23), 3 to (3, 0)
price :: Parser (Int, Int)
price =
  do dollars <- signedInteger
     cents <- fromMaybe 0 <$> optional (PD.symbol "." *> signedInteger)
     return (fromIntegral dollars, fromIntegral cents)



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
