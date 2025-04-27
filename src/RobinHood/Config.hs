module RobinHood.Config where

import RobinHood.Prelude
import RobinHood.Money

data AppConfig
  = AppConfig
    { taxYear :: Year
    , codesToIgnore :: Set ByteString
    , ignoreUnknownCodes :: Bool
    , blockSize :: Int
    , balance :: Money
    } deriving (Show, Generic)


defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
  { taxYear = 2024
  , codesToIgnore = mempty
  , ignoreUnknownCodes = False
  , blockSize = 32
  , balance = 0
  }
