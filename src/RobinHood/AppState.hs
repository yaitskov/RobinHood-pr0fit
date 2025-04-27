{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RobinHood.AppState where

import RobinHood.Cmd
import RobinHood.Instrument
import RobinHood.Money
import RobinHood.Date
import RobinHood.Prelude
import RobinHood.TargetPeriod (TargetPeriod)

newtype BlockId = BlockId { unBlockId :: Integer } deriving (Show, Eq, Num, Ord)


data RhProfit
  = RhProfit
  { currentBlock :: !BlockId
  , instruments :: !(Map RhInstrumentName RhInstrument)
  , interest :: !(Map TargetPeriod Money)
  , balance :: !Money
  , lastRowDate :: !(Maybe Date)
  } deriving (Show, Generic)

type ProfitM a = StateT RhProfit (ReaderT CmdArgs IO) a
