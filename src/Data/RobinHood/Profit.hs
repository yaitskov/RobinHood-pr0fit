{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.RobinHood.Profit where

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 hiding (takeWhile1)
import Data.ByteString.Char8 (hGet)
import Data.Char qualified as C
import Debug.TraceEmbrace
import GHC.Generics
import GHC.IO.Handle (HandlePosn(HandlePosn))
import Relude
import System.IO hiding (char8, putStrLn)

data RobinRow
  = RobinRow
    { activityDate :: String
    , processDate :: String
    , settleDate :: String
    , instrument :: String
    , description :: String
    , transCode :: String
    , quantity:: String
    , price:: String
    , amount :: String
    , unnamed :: String
    } deriving (Show, Eq, Generic)

comma :: Word8
comma = fromIntegral (C.ord ',')

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

newtype BlockId = BlockId { unBlockId :: Integer } deriving (Show, Eq, Num, Ord)

blockSize :: Int
blockSize = 32

readBlock :: Handle -> StateT BlockId IO ByteString
readBlock h = do
  iBlock <- get

  if $(tw "/") iBlock < 0
    then pure mempty
    else do
      r <- liftIO $ do
        hSeek h AbsoluteSeek $ unBlockId iBlock * fromIntegral blockSize
        hGet h blockSize
      put $ iBlock - 1
      pure $ $(tw "got/") r

parseFile :: Handle -> IO [[ByteString]]
parseFile h = do
  hSeek h SeekFromEnd 0
  HandlePosn _ hSize <- hGetPosn h
  let bs = fromIntegral blockSize
  let lastBlock = max 0 $ (hSize `div` bs) - if hSize `mod` bs == 0 then 1 else 0
  evalStateT goWithBlockId $ BlockId lastBlock
  where
    goWithBlockId = do
      input <- readBlock h
      go Nothing input []
    go !loopDetector input !rows = do
      iBlock <- get
      if iBlock < 0 && input == mempty
        then pure rows
        else do
          putStrLn $ "go input " <> show input <> "; iBlock = " <> show iBlock
          parseBackWith (readBlock h) parseRow input >>= \case
            Fail _unconsumed ctx er -> do
              erpos <- liftIO $ hTell h
              fail $ "Failed to parse CSV file around " <> show erpos <> " byte; due: "
                <> show er <> "; context: " <> show ctx
            Partial _ -> fail "CSV file is partial"
            Done (unconsumed :: ByteString) (row :: [ByteString]) -> do
              iBlock' <- get
              if loopDetector == Just (unconsumed, iBlock')
                then do
                  putStrLn $ "Loop detected. Unconsumed input: " <> show unconsumed
                  pure rows
                else
                  go (Just (unconsumed, iBlock')) unconsumed (row : rows)

main :: IO ()
main = do
  h <- openFile "data/2025-01.csv" ReadMode
  rows <- parseFile h
  putStrLn $ show rows
  hClose h

    -- Right v -> V.forM_ v $ \ (activity_date :: String, process_date :: String, settle_date :: String,instrument :: String, description :: String, trans_code :: String, quantity:: String, price:: String, amount :: String, unnamed :: String) ->
    --   putStrLn $ instrument ++ " earns " ++ amount ++ " dollars"
