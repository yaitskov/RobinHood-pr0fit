module RobinHood.Test.Money where

import Data.Attoparsec.ByteString (parseOnly)
import Data.Text.Encoding as DTE
import RobinHood.Money
import RobinHood.Prelude
import Test.Tasty.QuickCheck

prop_parseMoney_showed_positive_int :: Positive Int -> Bool
prop_parseMoney_showed_positive_int (Positive n) =
  parseOnly parseMoney (DTE.encodeUtf8 $ "$" <> show n <> ".00") == pure (Money $ fromIntegral n)

prop_parseMoney_showed_negative_int :: Positive Int -> Bool
prop_parseMoney_showed_negative_int (Positive n) =
  parseOnly parseMoney (DTE.encodeUtf8 $ "-$" <> show n <> ".00") == pure (Money $ fromIntegral (-n))

prop_parseMoney_zero :: Bool
prop_parseMoney_zero =
  parseOnly parseMoney "$0.00" == pure 0

prop_parseMoney_showed_positive_cents :: Positive Int -> Bool
prop_parseMoney_showed_positive_cents (Positive n) =
  parseOnly parseMoney (DTE.encodeUtf8 $ toText centsStr) == pure (Money $ fromIntegral n / 100)
  where
    cents = n `mod` 100
    centsStr :: String = printf "$0.%02d" cents

prop_parseMoney_thousands :: Positive Int -> Bool
prop_parseMoney_thousands (Positive n) =
  parseOnly parseMoney (DTE.encodeUtf8 $ "$" <> show n <> ",000.00") == pure (Money $ fromIntegral n * 1_000)

prop_parseMoney_parentheses :: Positive Int -> Bool
prop_parseMoney_parentheses (Positive n) =
  parseOnly parseMoney (DTE.encodeUtf8 $ "($" <> show n <> ".00)") == pure (Money $ fromIntegral n)

prop_parseMoney_parentheses_negative :: Positive Int -> Bool
prop_parseMoney_parentheses_negative (Positive n) =
  parseOnly parseMoney (DTE.encodeUtf8 $ "(-$" <> show n <> ".00)") == pure (Money $ fromIntegral n * (-1))
