{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module RobinHood.Instrument where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.Semigroup.Generic
import RobinHood.Compactable (Compactable)
import RobinHood.Date
import RobinHood.Money
import RobinHood.Prelude
import RobinHood.TargetPeriod

newtype RhInstrumentName = RhInstrumentName ByteString
  deriving newtype (Show, Eq, Ord, Compactable)

instance PrintfArg RhInstrumentName where
  formatArg (RhInstrumentName n) ff =
    case ff.fmtWidth of
      Nothing -> (C8.unpack n <>)
      Just w ->
        ((replicate (w - B.length n) ' ' <> C8.unpack n) <>)

parseInstrumentName :: ByteString -> Either String RhInstrumentName
parseInstrumentName "" = fail "Empty Instrument name"
parseInstrumentName o = pure $ RhInstrumentName o

data InstrumentProfit
  = InstrumentProfit
    { capitalGain :: !Money
    , oversell :: !(Maybe (Min Date))
    , dividend :: !Money
    , fee :: !Money
    , tax :: !Money
    } deriving (Show, Generic)

instance Semigroup InstrumentProfit where
  (<>) = gmappend

instance Monoid InstrumentProfit where
  mempty = InstrumentProfit 0 mempty 0 0 0

data RhInstrument
  = RhInstrument
  { name :: !RhInstrumentName
  , cumQuantity :: !Int
  , avgCost :: ![(Int, Money)]
  , previousDay :: !Date
  , profit :: !(Map TargetPeriod InstrumentProfit)
  } deriving (Show, Generic)
