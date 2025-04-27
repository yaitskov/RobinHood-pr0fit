module RobinHood.Compactable where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import RobinHood.Prelude

class Compactable k where
  compact :: k -> k

instance Compactable String where
  compact = id
  {-# INLINE compact #-}
instance Compactable Int where
  compact = id
  {-# INLINE compact #-}
instance Compactable BS.ByteString where
  compact = BS.copy
  {-# INLINE compact #-}
instance Compactable LBS.ByteString where
  compact = LBS.copy
  {-# INLINE compact #-}

merge :: (Ord k, Compactable k) => k -> v -> (v -> v) -> Map k v -> Map k v
merge k newV mergeV m =
  case lookup k m of
    Nothing ->
      insert (compact k) newV m
    Just e ->
      insert k (mergeV e) m
{-# INLINE merge #-}
