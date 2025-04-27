module RobinHood.Char8 (module RobinHood.Char8, module X) where

import Data.Attoparsec.ByteString.Char8 as X (char8)
import RobinHood.Prelude

comma :: Word8
comma = fromIntegral $ ord ','

zero :: Word8
zero = fromIntegral $ ord '0'

slash :: Word8
slash = fromIntegral $ ord '/'

nine :: Word8
nine = fromIntegral $ ord '9'

digitStep :: Num a => a -> Word8 -> a
digitStep a w = a * 10 + fromIntegral (w - zero)
{-# INLINE digitStep #-}
