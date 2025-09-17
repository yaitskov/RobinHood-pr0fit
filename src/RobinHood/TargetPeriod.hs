module RobinHood.TargetPeriod where

import Data.Attoparsec.Text qualified as A
import RobinHood.Compactable
import RobinHood.Prelude
import RobinHood.Date

newtype Quarter = Quarter Int deriving (Show, Eq, Ord)

data TargetPeriod
  = QuarterPeriod Year Quarter
  | YearPeriod Year
  | StarPeriod
  deriving (Show, Eq, Ord)

instance PrintfArg TargetPeriod where
  formatArg StarPeriod ff =
    case ff.fmtWidth of
      Nothing -> ("*" <>)
      Just w -> ((replicate (w - 1) ' ' <> "*") <>)
  formatArg (YearPeriod y) ff =
    let sy :: String = show y in
      case ff.fmtWidth of
        Nothing -> (sy <>)
        Just w -> ((replicate (w - length sy) ' ' <> sy) <>)
  formatArg (QuarterPeriod y (Quarter q)) ff =
    let sy :: String = show y
        sq :: String = show q
        s = "Q" <> sq <> "/" <> sy
    in
      case ff.fmtWidth of
        Nothing -> (s <>)
        Just w -> ((replicate (w - length s) ' ' <> s) <>)

instance Compactable TargetPeriod where
  compact = id
  {-# INLINE compact #-}

monthToQuater :: Int -> Quarter
monthToQuater m
  | m > 0 && m < 13 = Quarter $ (m `div` 4)  + 1
  | otherwise = error $ "Month out of range: " <> show m

targetPeriodsByDate :: Date -> Set TargetPeriod -> [TargetPeriod]
targetPeriodsByDate (Date dt) tps = filter p $ toList tps
  where
    (dtY, dtM, _d) = toGregorian dt
    p = \case
      StarPeriod -> True
      YearPeriod y -> y == dtY
      QuarterPeriod y q -> y == dtY && q == monthToQuater dtM

targetPeriodParser :: A.Parser TargetPeriod
targetPeriodParser = do
  qPeriod <|> yPeriod <|> star
  where
    yPeriod = YearPeriod <$> A.decimal
    star = A.char '*' >> pure StarPeriod
    qPeriod = do
      q <- (A.char 'q' <|> A.char 'Q') >> A.decimal
      y <- A.char '/' >> A.decimal
      if q > 0 && q < 5
        then pure $ QuarterPeriod y (Quarter q)
        else fail $ "Quater out of range: [" <> show q <> "]"

defaultTargetPeriodsFor :: Day -> [TargetPeriod]
defaultTargetPeriodsFor now = [StarPeriod, YearPeriod yPeriod, qPeriod]
  where
    (y, moy, dom) = toGregorian now
    qPeriod
      | (moy, dom) < (January, 15) = QuarterPeriod (y - 1) $ Quarter 4
      | (moy, dom) < (April, 15) = QuarterPeriod y $ Quarter 1
      | (moy, dom) < (July, 15) = QuarterPeriod y $ Quarter 2
      | (moy, dom) < (October, 15) = QuarterPeriod y $ Quarter 3
      | otherwise = QuarterPeriod y $ Quarter 4
    yPeriod
      | (moy, dom) < (April, 15) = y - 1
      | otherwise = y
