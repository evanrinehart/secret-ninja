{-# LANGUAGE OverloadedStrings #-}
module Misc where

import Control.Monad
import Data.Maybe

whenJust :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJust g f = do
  m <- g
  maybe (return ()) f m

