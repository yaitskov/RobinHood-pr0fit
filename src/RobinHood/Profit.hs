{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module RobinHood.Profit where


import RobinHood.AppState
import RobinHood.Compactable
import RobinHood.Money
import RobinHood.TargetPeriod
import RobinHood.Instrument
import RobinHood.Prelude
import RobinHood.RobinRow


aggRow :: RobinRow -> ProfitM ()
aggRow = \case
  rtr@RobinTradeRow{} -> do
    tps <- targetPeriodsFor rtr.date
    modify' ( updateBalance rtr.date (rtr.amount +)
             . (#instruments %~ merge rtr.instrument
                 (newInstrumentByTrade rtr) (tradeUpdate tps rtr))
            )
  RobinBonusRow dt a ->
    modify' (updateBalance dt (a +))
  RobinFeeRow dt a ->
    modify' (updateBalance dt (a +))
  RobinAgencyFeeRow ins dt a ->
    updateProfitField #fee ins dt a
  RobinForeignTaxRow ins dt a ->
    updateProfitField #tax ins dt a
  RobinDividendRow ins dt a ->
    updateProfitField #dividend ins dt a
  RobinInterestRow dt a -> do
    tps <- targetPeriodsFor dt
    modify' ( updateBalance dt (a +)
              . (#interest %~ flip (foldl' (\p' tp -> merge tp a (a +) p')) tps)
            )

  RobinMoneyMoveRow dt a ->
    modify' (updateBalance dt (a +))
  RobinHeaderRow -> pure ()
  where
    updateProfitField (fieldLens :: Lens' InstrumentProfit Money) ins dt a = do
      tps <- targetPeriodsFor dt
      modify' ( updateBalance dt (a +)
              . (#instruments %~ merge ins
                                  (newInstrumentByNoTrade fieldLens a tps ins dt)
                                  (#profit %~ mergeInstProfitField fieldLens a tps)
                )
              )

    targetPeriodsFor dt = targetPeriodsByDate dt <$> asks (^. #targetPeriod)
    newInstrumentByNoTrade (fieldLens :: Lens' InstrumentProfit Money) dv tps insName dt =
      RhInstrument
      { name = insName
      , cumQuantity = 0
      , avgCost = []
      , previousDay = dt
      , profit = mergeInstProfitField fieldLens dv tps mempty
      }

    newInstrumentByTrade :: RobinRow -> RhInstrument
    newInstrumentByTrade rtr =
      RhInstrument
      { name = rtr.instrument
      , cumQuantity = rtr.quantity
      , avgCost = [(rtr.quantity, rtr.price)]
      , previousDay = rtr.date
      , profit = mempty
      }

    updateBalance dt df =
      (#balance %~ \cb ->
                     let nb = df cb in
                       if nb < 0
                       then error $
                            "Negative balance - history must be too short.\n" <>
                            "Hint: set init balance via -b option if history is not complete."
                       else nb)
      . (#lastRowDate %~ \case
            Nothing -> Just dt
            Just odt
              | odt > dt -> error $ "Unordered row " <> show odt <> " > " <> show dt
              | otherwise -> Just dt)

    mergeInstProfitField (fieldLens :: Lens' InstrumentProfit Money)  dv tps p =
      foldl' (\p' tp -> merge tp (mempty & (fieldLens .~ dv)) (fieldLens %~ (dv +)) p') p tps

    tradeUpdate tps rtr (ins :: RhInstrument)
      | ins.previousDay > rtr.date =
          error $ "Rows of " <> show ins.name <> " out of order"
      | rtr.amount < 0  =
          ins -- buy
          & #previousDay .~ rtr.date
          & #avgCost .~ (abs rtr.amount + totalSum ins.avgCost) `divide` (rtr.quantity + ins.cumQuantity)
          & #cumQuantity %~ (rtr.quantity +)
      | ins.cumQuantity < rtr.quantity = -- over sell
          ins
          & #profit %~ (mergeInstProfitFieldF #oversell rtr.date)
          & #cumQuantity .~ 0
          & #avgCost .~ []
          & #previousDay .~ rtr.date
      | otherwise =
          let (cost', avgCost') = partSum rtr.quantity ins.avgCost
              dGain = rtr.price * fromIntegral rtr.quantity - cost'
          in
            ins -- sell - avg cost does not change
            & #previousDay .~ rtr.date
            & #avgCost .~ avgCost'
            & #profit %~ mergeInstProfitField #capitalGain dGain tps
            & #cumQuantity %~ (-rtr.quantity +)
      where
        mergeInstProfitFieldF fieldLens d p =
          foldl' (\p' tp -> merge tp (mempty & (fieldLens .~ Just (Min d)))
                            (fieldLens %~ (Just (Min d) <>)) p')
          p tps
