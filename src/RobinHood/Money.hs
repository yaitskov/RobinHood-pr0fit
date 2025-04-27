{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RobinHood.Money where

import Data.ByteString qualified as B8
import Data.Decimal qualified as D
import RobinHood.CellParser
import RobinHood.Char8
import RobinHood.Prelude

newtype Money
  = Money
  { unMoney :: D.Decimal }
  deriving newtype (Show, Eq, Ord, Num)
  deriving stock (Generic)

instance Semigroup Money where
  Money a <> Money b = Money $ a + b

divide :: Money -> Int -> [(Int, Money)]
divide (Money d) n = (_2 %~ Money) <$> d `D.divide` n

totalSum :: [(Int, Money)] -> Money
totalSum = sum . fmap (\(a, b) -> fromIntegral a * b)

partSum :: Int -> [(Int, Money)] -> (Money, [(Int, Money)])
partSum nToTake = go nToTake 0
  where
    go 0 !finalSum restShares =
      (finalSum, restShares)
    go !nLeft !interSum ((shares, price):t)
      | nLeft < shares = (interSum + fromIntegral nLeft * price, (shares - nLeft, price):t)
      | otherwise = go (nLeft - shares) (interSum + fromIntegral shares * price) t
    go _ !finalSum [] = (finalSum, [])


-- > -$38,877.00
parseMoney :: CellParser Money
parseMoney = go <?> "money"
  where
    go = do
      sign <- (void (char8 '-') >> pure (-1)) <|> pure 1
      void (char8 '$' <?> "dollar sign")
      n <- B8.foldl' step 0 <$> takeWhile1 isDigitOrComma
      void (char8 '.' <?> "comma")
      fraq  <- takeWhile1 isDigit
      let l = B8.length fraq
      if l > 22
        then fail $ "Too many digits after comma" <> show n <> "." <> show fraq
        else pure . Money . D.Decimal (fromIntegral l) $ sign * B8.foldl' step n fraq
    step a w
      | isDigit w = digitStep a w
      | w == comma = a
      | otherwise = error $ "Dead code when w = "  <> show w

    isDigit w = w >= zero && w <= nine
    isDigitOrComma w = isDigit w || w == comma
