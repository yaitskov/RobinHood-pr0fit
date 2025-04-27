{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLabels #-}

module RobinHood.CsvParser where

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 hiding (takeWhile1)
import Data.ByteString.Char8 (hGet)
import Data.Char qualified as C
import GHC.IO.Handle (HandlePosn(HandlePosn))
import RobinHood.Prelude
import RobinHood.Char8
import RobinHood.AppState
import RobinHood.RobinRow
import System.IO hiding (char8, putStrLn)


parseUnquotedCell :: BackParser ByteString
parseUnquotedCell = takeWhile1 p <|> pure ""
  where
    p c = c /= comma && c /= 10 && c /= 13

parseDoubleQuotedCell :: BackParser ByteString
parseDoubleQuotedCell =
  char8 '"' *> (takeWhile1 (/= dQuote) <|> pure "") <*  char8 '"'
  where
    dQuote = fromIntegral (C.ord '"')

parseCell :: BackParser ByteString
parseCell = parseDoubleQuotedCell <|> parseUnquotedCell

parseRow :: BackParser [ByteString]
parseRow = do
  skipSpace
  (endOfInput >> pure []) <|> go
  where
    go = do
      cells <- many' (parseCell <* char8 ',')
      firstCell <- parseCell
      -- reverse is a workaround due many' bug for Backward mode
      -- many' should be in a class
      pure $ firstCell : reverse cells

readBlock :: Handle -> ProfitM ByteString
readBlock h = do
  iBlock <- gets currentBlock
  if iBlock < 0
    then pure mempty
    else do
      bs <- asks (^. #blockSize)
      r <- liftIO $ do
        hSeek h AbsoluteSeek $ unBlockId iBlock * fromIntegral bs
        hGet h bs
      modify (#currentBlock .~ iBlock - 1)
      pure r

findLastBlock :: MonadIO m => Int -> Handle -> m BlockId
findLastBlock blockSize' h = liftIO $ do
  hSeek h SeekFromEnd 0
  HandlePosn _ hSize <- hGetPosn h
  let bs = fromIntegral blockSize'
  pure . BlockId .  max 0 $ (hSize `div` bs) - if hSize `mod` bs == 0 then 1 else 0

consumeFile :: Handle -> (RobinRow -> ProfitM ()) -> ProfitM ()
consumeFile h handleRow = do
  input <- readBlock h
  go Nothing input
  where
    go !loopDetector input = do
      iBlock <- gets (^. #currentBlock)
      if iBlock < 0 && input == mempty
        then pure ()
        else do
          parseBackWith (readBlock h) parseRow input >>= \case
            Fail _unconsumed ctx er -> do
              erpos <- liftIO $ hTell h
              fail $ "Failed to parse CSV file around " <> show erpos <> " byte; due: "
                <> show er <> "; context: " <> show ctx
            Partial _ -> fail "CSV file is partial"
            Done (unconsumed :: ByteString) (rawRow :: [ByteString]) -> do
              iBlock' <- gets (^. #currentBlock)
              if loopDetector == Just (unconsumed, iBlock')
                then
                  fail $ "Loop detected. Unconsumed input: " <> show unconsumed
                else do
                  trashCodes <- asks (^. #codesToSkip)
                  case parseRobinRow trashCodes rawRow of
                    Left e -> fail e
                    Right row -> do
                      forM_ row handleRow
                      go (Just (unconsumed, iBlock')) unconsumed


consumeFileUntil :: Handle -> ProfitM (Maybe RobinRow)
consumeFileUntil h  = do
  input <- readBlock h
  go Nothing input
  where
    go !loopDetector input = do
      iBlock <- gets (^. #currentBlock)
      if iBlock < 0 && input == mempty
        then pure $ Nothing
        else do
          parseBackWith (readBlock h) parseRow input >>= \case
            Fail _unconsumed ctx er -> do
              erpos <- liftIO $ hTell h
              fail $ "Failed to parse CSV file around " <> show erpos <> " byte; due: "
                <> show er <> "; context: " <> show ctx
            Partial _ -> fail "CSV file is partial"
            Done (unconsumed :: ByteString) (rawRow :: [ByteString]) -> do
              iBlock' <- gets (^. #currentBlock)
              if loopDetector == Just (unconsumed, iBlock')
                then
                  fail $ "Loop detected. Unconsumed input: " <> show unconsumed
                else do
                  trashCodes <- asks (^. #codesToSkip)
                  case parseRobinRow trashCodes rawRow of
                    Left e -> fail e
                    Right mayRow ->
                      case mayRow of
                        Nothing -> go (Just (unconsumed, iBlock')) unconsumed
                        Just justRow -> pure $ (Just justRow)
