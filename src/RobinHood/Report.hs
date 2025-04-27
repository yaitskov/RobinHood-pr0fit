module RobinHood.Report where

import Data.Map qualified as M
import RobinHood.AppState
import RobinHood.Money
import RobinHood.Prelude
import RobinHood.Instrument


printReport :: ProfitM ()
printReport = do
  st <- get
  case preview traverseMax ((^. #previousDay) <$> st.instruments) of
    Nothing -> fail "No trades"
    Just _lastTradeDay -> do
      printPerInstrument st
      putStrLn "--------------------------------------------------------------"
      printAggByPeriod st
      putStrLn "--------------------------------------------------------------"
      putStrLn $ "Balance: " <> show ((round $ unMoney st.balance) :: Integer)

  where
    printAggByPeriod st = do
      tax' <- asks (^. #taxBraket)
      putStrLn " Period|      Gain|  Interest|  Dividend|    Profit|Tax Prepay|  Oversell"
      forM_ (toAscList . unionsWith (<>) . fmap (^. #profit) $ elems st.instruments) $ \(p, ip) -> do
        let inter = fromMaybe 0 $ st.interest ^. at p
            profit' = m2s $ ip.capitalGain + inter + ip.dividend - ip.tax
          in
          putStrLn $
            printf "%7s|%10d|%10d|%10d|%10d|%10.2f|%10s"
             p
             (m2s ip.capitalGain) (m2s inter) (m2s ip.dividend)
             profit'
             (tax' * fromIntegral profit')
             ip.oversell

    printPerInstrument st = do
      putStrLn "Instrument| Period|      Gain|  Dividend|    Profit|  Oversell"
      forM_ (elems st.instruments) $ \i -> do
        forM_ (M.toList i.profit) $ \(p, ip) -> do
          putStrLn $
            printf "%10s|%7s|%10d|%10d|%10d|%10s"
             i.name
             p
             (m2s ip.capitalGain) (m2s ip.dividend)
             (m2s $ ip.capitalGain + ip.dividend - ip.tax)
             ip.oversell

    m2s :: Money -> Int
    m2s (Money d) = round d
