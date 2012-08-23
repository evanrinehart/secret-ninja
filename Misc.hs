{-# LANGUAGE OverloadedStrings #-}
module Misc where

import Control.Monad
import Data.Maybe
import Data.Time
import Debug.Trace

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM g f = do
  m <- g
  maybe (return ()) f m

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust x f = maybe (return ()) f x

timeUntil :: UTCTime -> IO NominalDiffTime
timeUntil t = do
  now <- getCurrentTime
  return (diffUTCTime t now)

tracee :: Show a => a -> a
tracee x = trace ("TRACE: "++show x) x


