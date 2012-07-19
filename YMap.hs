{-# LANGUAGE DeriveDataTypeable #-}
module YMap where

{-
YMap is a Map from a key type to a value type
which supports efficient search for keys that
have a certain value. Basically it lets you lookup
both ways.

Make sure you use different types for the key and value.
newtype if necessary.
-}

import Data.Data
import Data.IxSet
import Data.Typeable
import Data.SafeCopy
import qualified Data.IxSet as X

data YEntry a b = YEntry a b deriving (Eq,Ord,Show,Typeable)
type YMap a b = IxSet (YEntry a b)

instance (Ord a, Ord b, Typeable a, Typeable b) => Indexable (YEntry a b) where
  empty = ixSet [
      ixFun (\(YEntry x _) -> [x]),
      ixFun (\(YEntry _ y) -> [y])
    ]

instance (SafeCopy a, SafeCopy b) => SafeCopy (YEntry a b) where
  getCopy = contain $ do
    x <- safeGet
    y <- safeGet
    return (YEntry x y)
  putCopy (YEntry x y) = contain $ do
    safePut x
    safePut y

getK :: YEntry a b -> a
getK (YEntry k _) = k

getV :: YEntry a b -> b
getV (YEntry _ v) = v

-- empty :: (Ord a, Ord b, Typeable a, Typeable b) => YMap a b
empty :: (Ord a, Ord b, Typeable a, Typeable b) => YMap a b
empty = X.empty

lookup :: (Ord a, Ord b, Typeable a, Typeable b) => a -> YMap a b -> Maybe b
lookup k = fmap getV . getOne . getEQ k

search :: (Ord a, Ord b, Typeable a, Typeable b) => b -> YMap a b -> [a]
search v = map getK . X.toList . getEQ v

write :: (Ord a, Ord b, Typeable a, Typeable b) => a -> b -> YMap a b -> YMap a b
write k v = updateIx k (YEntry k v)

delete :: (Ord a, Typeable a, Ord b, Typeable b) => a -> YMap a b -> YMap a b
delete k = deleteIx k

elem :: (Ord a, Typeable a, Ord b, Typeable b) => b -> YMap a b -> Bool
elem v = X.null . getEQ v

null :: YMap a b -> Bool
null = X.null
