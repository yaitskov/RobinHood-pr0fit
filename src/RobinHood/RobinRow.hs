{-# LANGUAGE OverloadedStrings #-}
module RobinHood.RobinRow where

import Data.Set
import RobinHood.CellParser
import RobinHood.Date
import RobinHood.Instrument
import RobinHood.Money
import RobinHood.Prelude


data RobinRow
  = RobinTradeRow -- change balance ande capital gain
    { date :: Date
    , instrument :: RhInstrumentName
    , quantity:: Int
    , price:: Money
    , amount :: Money
    }
  | RobinFeeRow -- change balance but not capital gain
    { date :: Date
    , amount :: Money
    }
  | RobinAgencyFeeRow -- change balance but not capital gain
    { instrument :: RhInstrumentName
    , date :: Date
    , amount :: Money
    }
  | RobinForeignTaxRow -- change balance and capital gain
    { instrument :: RhInstrumentName
    , date :: Date
    , amount :: Money
    }
  | RobinDividendRow -- change balance count and dividen
    { instrument :: RhInstrumentName
    , date :: Date
    , amount :: Money
    }
  | RobinInterestRow -- change balance and interest
    { date :: Date
    , amount :: Money
    }
  | RobinBonusRow -- change balance
    { date :: Date
    , amount :: Money
    }
  | RobinMoneyMoveRow -- change balance
    { date :: Date
    , amount :: Money
    }
  | RobinHeaderRow -- signal end of file
  deriving (Show, Eq, Generic)

parseRobinRow :: Set ByteString -> [ByteString] -> Either String (Maybe RobinRow)
parseRobinRow codesToIgnore rowCells =
  case rowCells of
   [_activityDate, rawProcessDate, _settleDate, rawInstrument, _description,
    rawTransCode, rawQuantity, rawPrice, rawAmount, _unnamedEmpty] ->
     case rawTransCode of
       "Trans Code" -> pure $ Just RobinHeaderRow
       "ACH" ->  parseDateMoney RobinMoneyMoveRow rawProcessDate rawAmount
       "GOLD" -> parseDateMoney RobinFeeRow rawProcessDate rawAmount
       "GMPC" -> parseDateMoney RobinFeeRow rawProcessDate rawAmount
       "GDBP" -> parseDateMoney RobinBonusRow rawProcessDate rawAmount
       "DFEE" -> parseInstrDateMoney RobinAgencyFeeRow rawInstrument rawProcessDate rawAmount
       "AFEE" -> parseInstrDateMoney RobinAgencyFeeRow rawInstrument rawProcessDate rawAmount
       "DTAX" -> parseInstrDateMoney RobinForeignTaxRow rawInstrument rawProcessDate rawAmount
       "INT" ->  parseDateMoney RobinInterestRow rawProcessDate rawAmount
       "CDIV" -> parseInstrDateMoney RobinDividendRow rawInstrument rawProcessDate rawAmount
       "Sell" -> parseTrade rawProcessDate rawInstrument rawQuantity rawPrice rawAmount
       "Buy" ->  parseTrade rawProcessDate rawInstrument rawQuantity rawPrice rawAmount
       "SPL" ->  pure Nothing -- ?
       "SPR" ->  pure Nothing -- ?
       "" ->     pure Nothing
       oCode | oCode `member` codesToIgnore -> pure Nothing
             | otherwise -> fail $ "Unknown Trans Code: " <> show oCode
   o -> fail $ "Wrong number of columns in " <> show o
  where
    parseCell :: (CellParser a) -> ByteString -> Either String a
    parseCell p bs =
      case parseOnly p bs of
         Left e -> fail $ e <> "\nOn the row " <> show rowCells
         Right v -> pure v
    parseInstrDateMoney dc ins dt am =
      Just <$> (dc <$> parseInstrumentName ins
                 <*> parseCell parseDate dt
                 <*> parseCell parseMoney am)
    parseDateMoney dc dt am =
      Just <$> (dc <$> parseCell parseDate dt <*> parseCell parseMoney am)
    parseTrade rawProcessDate rawInstrumentName rawQuantity rawPrice rawAmount =
      Just <$> (RobinTradeRow
        <$> parseCell parseDate rawProcessDate
        <*> parseInstrumentName rawInstrumentName
        <*> parseCell decimal rawQuantity
        <*> parseCell parseMoney rawPrice
        <*> parseCell parseMoney rawAmount)
