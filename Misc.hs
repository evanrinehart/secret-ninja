{-# LANGUAGE OverloadedStrings #-}
module Misc where

import Control.Monad
import Data.Maybe

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM g f = do
  m <- g
  maybe (return ()) f m

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust x f = maybe (return ()) f x
