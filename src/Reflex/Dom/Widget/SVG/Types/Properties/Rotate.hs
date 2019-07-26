{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | 
module Reflex.Dom.Widget.SVG.Types.Properties.Rotate
  (
    Rotate (..)
  ) where

import           Control.Lens (Rewrapped, Wrapped (..), iso)


-- | Wrap the @Float@ value with something more meaningful.
newtype Rotate = Rotate Float deriving (Eq, Show)

instance Rotate ~ t => Rewrapped Rotate t
instance Wrapped Rotate where
  type Unwrapped Rotate = Float
  _Wrapped' = iso (\ (Rotate x) -> x) Rotate
