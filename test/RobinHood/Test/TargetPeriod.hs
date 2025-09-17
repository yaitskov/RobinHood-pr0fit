module RobinHood.Test.TargetPeriod where

import Data.Attoparsec.Text (parseOnly)
import RobinHood.TargetPeriod
import RobinHood.Prelude
import Test.Tasty.QuickCheck

prop_monthToQuarter_less_4 :: Positive Int -> Bool
prop_monthToQuarter_less_4 (Positive n) = monthToQuater ((n `mod` 12) + 1) < Quarter 5

prop_monthToQuarter_positive :: Positive Int -> Bool
prop_monthToQuarter_positive (Positive n) = monthToQuater ((n `mod` 12) + 1) > Quarter 0

prop_targetPeriodParser_q_year :: Positive Int -> Positive Int -> Bool
prop_targetPeriodParser_q_year (Positive q) (Positive y) = got == pure expected
  where
    got = parseOnly targetPeriodParser ("Q" <> show q' <> "/202" <> show y')
    expected = QuarterPeriod (fromIntegral $ 2020 + y') (Quarter q')
    q' = q `mod` 4 + 1
    y' = y `mod` 10
