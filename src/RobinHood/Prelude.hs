module RobinHood.Prelude (module X) where

import Control.Lens as X (Lens', (.~), (^?), (^.), (%~), (^..), preview, traverseMax, at, ix)
import Control.Lens.Tuple as X
import Data.Char as X (toLower)
import Data.List as X (isSuffixOf)
import Data.Map.Strict as X (insert, lookup, elems, unionsWith, toAscList)
import Data.Semigroup as X (Min (..), Max (..))
import Data.Time as X
import Debug.TraceEmbrace as X
import GHC.Generics as X
import Relude as X
import Text.Printf as X
