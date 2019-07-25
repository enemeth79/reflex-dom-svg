{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and function for the @Radius@ wrapper.
module Reflex.Dom.Widget.SVG.Types.Properties.Radius
  ( Radius
  , _Radius
  , _RadiusX
  , _RadiusY
  ) where

import           Control.Lens                               (Iso', Rewrapped,
                                                             Wrapped (..), iso,
                                                             _Wrapped)

import           Reflex.Dom.Widget.CSS.DataTypes.Dimensions (LengthOrPercentage)
import           Reflex.Dom.Widget.SVG.Types.Internal.Pos   (X, Y)


-- | The value of a @Radius@ is just a @\<length-percentage\>@. But we can do better so we wrap
-- it up in a @newtype@ to keep things organised.
newtype Radius p =
  Radius LengthOrPercentage
  deriving (Eq, Show)

instance (Radius p) ~ t => Rewrapped (Radius p) t

instance Wrapped (Radius p) where
  type Unwrapped (Radius p) = LengthOrPercentage
  _Wrapped' = iso (\(Radius x) -> x) Radius

-- | Iso for the float value of a radius
_Radius :: Iso' (Radius ()) LengthOrPercentage
_Radius = _Wrapped

-- | Iso for the float value of an X position radius
_RadiusX :: Iso' (Radius X) LengthOrPercentage
_RadiusX = _Wrapped

-- | Iso for the float value of an Y position radius
_RadiusY :: Iso' (Radius Y) LengthOrPercentage
_RadiusY = _Wrapped
