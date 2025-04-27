module RobinHood.CellParser (module RobinHood.CellParser, module X) where

import Data.Attoparsec.ByteString as X
import Data.Attoparsec.ByteString.Char8 as X (decimal)

type CellParser a = Parser a
