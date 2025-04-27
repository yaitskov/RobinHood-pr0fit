module RobinHood.Run where

import Control.Monad.Catch
import RobinHood.AppState
import RobinHood.Cmd
import RobinHood.CsvParser
import RobinHood.Date
import RobinHood.Prelude
import RobinHood.Profit
import RobinHood.Report
import RobinHood.RobinRow
import System.Directory
import System.FilePath ((</>))
import System.IO hiding (char8, putStrLn)


sortedCsvFilesInTheDir :: FilePath -> ProfitM [FilePath]
sortedCsvFilesInTheDir d = do
  allFiles <- liftIO $ listDirectory d
  datedFiles <- mapM go (filter (isSuffixOf ".csv" . fmap toLower) allFiles)
  pure $ (fmap snd $ sort datedFiles)
 where
   go f =
     let fp = d </> f in
       (,fp) <$> readLastRowDate fp

readLastRowDate :: FilePath -> ProfitM Date
readLastRowDate csv = do
  bracket (liftIO (openFile csv ReadMode))
          (\h -> liftIO (hClose h)) $ \h -> do
    blkSz <- asks (^. #blockSize)
    lastBlock <- findLastBlock blkSz h
    modify' (#currentBlock .~ lastBlock)
    consumeFileUntil h >>= \case
      Nothing -> fail $ "File [" <> csv <> "] is empty"
      Just RobinHeaderRow -> fail $ "File [" <> csv <> "] is empty"
      Just row -> pure row.date

runRhProfit :: CmdArgs -> IO ()
runRhProfit args = void $ runReaderT (execStateT go st0) args
  where
    st0 = RhProfit 0 mempty mempty (args.initBalance) Nothing
    go = do
      sortedCsvFilesInTheDir args.csvInputDir >>= \case
        [] -> fail $ "Directory " <> show args.csvInputDir <> " does not have CSV files"
        ordCsvs ->
          goInM ordCsvs >> printReport

    goInM :: [FilePath] -> ProfitM ()
    goInM ordCsvs =
      forM_ ordCsvs $ \csv ->
        bracket (liftIO (openFile csv ReadMode))
                (\h -> liftIO (hClose h)) $ \h -> do
          blkSz <- asks (^. #blockSize)
          lastBlock <- findLastBlock blkSz h
          lastRowDate' <- gets (^. #lastRowDate)
          modify' (#currentBlock .~ lastBlock)
          let
            checkFileOrder = do
              lastRowDate'' <- gets (^. #lastRowDate)
              case (lastRowDate', lastRowDate'') of
                (Just j', Just j'')
                  | j' < j'' -> pure ()
                  | otherwise ->
                      error $ "Previous file and current file have overlapping dates in range: [" <>
                             show j'' <> ", " <> show j' <> "]"
                _ -> pure ()
          consumeFile h (\row -> aggRow row >> checkFileOrder)
      where
