{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
module Reflex.Dom.Widget.SVG.Types.Properties.TextLength
  (
    TextLength (..)
  ) where

import           Control.Lens                                              (Rewrapped,
                                                                            Wrapped (..),
                                                                            iso)

import           Reflex.Dom.Widget.CSS.DataTypes.Dimensions.LengthOrPercentage (LengthOrPercentage)

-- | Wrap the @LengthOrPercentage@ value.
newtype TextLength = TextLength LengthOrPercentage deriving (Eq, Show)

instance TextLength ~ t => Rewrapped TextLength t
instance Wrapped TextLength where
  type Unwrapped TextLength = LengthOrPercentage
  _Wrapped' = iso (\ (TextLength x) -> x) TextLength
