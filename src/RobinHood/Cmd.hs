module RobinHood.Cmd where

import Data.Attoparsec.Text qualified as A
import Data.ByteString.Char8 qualified as C8
import Data.Text qualified as T
import Data.String qualified as S
import Options.Applicative
import Options.Applicative.Help (vsep)
import RobinHood.Money
import RobinHood.Prelude
import RobinHood.TargetPeriod
import System.Directory

attoparsecReader :: A.Parser a -> ReadM a
attoparsecReader p = eitherReader (A.parseOnly (p <* A.endOfInput) . T.pack)


data CmdArgs
  = CmdArgs
    { targetPeriod :: Set TargetPeriod -- ^ by default last year and last complete quater (for estimate prepayment)
    , codesToSkip :: Set ByteString -- ^ trans codes to skip
    , blockSize :: Int
    , initBalance :: Money
    , taxBraket :: Double
    -- argument
    , csvInputDir :: FilePath -- ^ dir with CSV files from RobinHood
    } deriving (Show, Eq, Generic)


cmdArgsParser :: Set TargetPeriod -> Parser CmdArgs
cmdArgsParser defaultTargetPeriods =
  CmdArgs
    <$> option (fromList <$> (attoparsecReader (A.many1' (A.skipSpace >> targetPeriodParser))))
        (  long "target-period"
        <> short 'p'
        <> value defaultTargetPeriods
        <> helpDoc (pure $ vsep [ "Tax year or quarter (eg '* 2025 Q2/2026')."
                                , "Multiple values are supported separated with a space."
                                , "By default max year, quater and \"star\"."
                                ]))
    <*> option (fromList <$> (maybeReader (pure . fmap C8.pack . S.words)))
        (  long "skip-codes"
        <> short 'c'
        <> value mempty
        <> help "Rows to be skipped with values in Trans Code column")
    <*> option (auto >>= validateBlockSize)
        (  long "block-size"
        <> help "Size of CSV chunk in bytes loaded in RAM"
        <> value 32)
     <*> option (attoparsecReader (Money . fromInteger <$> A.decimal))
                (  long "init-balance"
                  <> short 'b'
                  <> value 0
                  <> showDefault
                  <> help "account balance by the time of first row in the activity report"
                )
    <*> option auto
        (  long "tax"
        <> short 't'
        <> help "Tax percent"
        <> showDefault
        <> value 0.24)
    <*> strArgument
        (  help "Path to directory with RobinHood activity reports (CSV files)"
        <> showDefault
        <> metavar "CSVDIR"
        <> value ".")
  where
    validateBlockSize i
      | i < 16    = fail $ "Block size is less than 16 bytes: " <> show i
      | otherwise = pure i


parseCmdArgs :: IO CmdArgs
parseCmdArgs = do
  defTargets <- fromList . defaultTargetPeriodsFor . utctDay <$> getCurrentTime
  a <- execParser $ opts defTargets
  whenM (not <$> doesDirectoryExist a.csvInputDir) $
    fail $ "Directory " <> show a.csvInputDir <> " does not exist"
  pure a
  where
    opts defTargets =
      info
        (cmdArgsParser defTargets <**> helper)
        (  fullDesc
        <> progDesc "Calc year and quarter profit based on RobinHood activity reports in CSV format"
        <> header "header"
        )
