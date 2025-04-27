module RobinHood.Date where

import RobinHood.CellParser
import RobinHood.Char8
import RobinHood.Prelude


newtype Date = Date Day deriving (Show, Eq, Ord)

-- > 2/10/2025
parseDate :: CellParser Date
parseDate = go <?> "date"
  where
    go = do
      month <- decimal
      void (word8 slash)
      day <- decimal
      void (word8 slash)
      year <- decimal
      endOfInput
      case fromGregorianValid year month day of
        Nothing -> fail $ "Wrong Date " <> show month <> "/" <> show day <> "/" <> show year
        Just d -> pure $ Date d

instance PrintfArg Date where
  formatArg (Date d) ff =
    case ff.fmtWidth of
      Nothing -> (show d <>)
      Just w ->
        let s :: String = show d in
          ((replicate (w - length s) ' ' <> s) <>)

instance PrintfArg (Maybe (Min Date)) where
  formatArg Nothing ff =
    case ff.fmtWidth of
      Nothing -> ("" <>)
      Just w -> ((replicate w ' ') <>)
  formatArg (Just (Min d)) ff = formatArg d ff
