{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and functions for a corner radius
module Reflex.Dom.Widget.SVG.Types.Properties.CornerRadius
  ( CornerRadius
  , _CornerRadiusX
  , _CornerRadiusY
  ) where

import           Control.Lens                               (Iso', Rewrapped,
                                                             Wrapped (..), iso,
                                                             _Wrapped)

import           Reflex.Dom.Widget.CSS.DataTypes.Dimensions (LengthOrPercentage)
import           Reflex.Dom.Widget.SVG.Types.Internal.Pos   (X, Y)

-- | Corner Radius is effectively just a @LengthOrPercentage@, but we can do better.
newtype CornerRadius p =
  CornerRadius LengthOrPercentage
  deriving (Eq, Show)

instance (CornerRadius p) ~ t => Rewrapped (CornerRadius p) t

instance Wrapped (CornerRadius p) where
  type Unwrapped (CornerRadius p) = LengthOrPercentage
  _Wrapped' = iso (\(CornerRadius x) -> x) CornerRadius

-- | @Iso@ for the CornerRadius on the X axis
_CornerRadiusX :: Iso' (CornerRadius X) LengthOrPercentage
_CornerRadiusX = _Wrapped

-- | @Iso@ for the CornerRadius on the Y axis
_CornerRadiusY :: Iso' (CornerRadius Y) LengthOrPercentage
_CornerRadiusY = _Wrapped
