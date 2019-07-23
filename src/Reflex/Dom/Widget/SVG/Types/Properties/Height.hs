{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
module Reflex.Dom.Widget.SVG.Types.Properties.Height
  ( Height (..)
  , _Height
  ) where

import           Control.Lens                                                  (Iso',
                                                                                Rewrapped,
                                                                                Wrapped (..),
                                                                                iso,
                                                                                _Wrapped)
import           Reflex.Dom.Widget.CSS.DataTypes.Dimensions.LengthOrPercentage

-- | Wrap the @Float@ or @LengthOrPercentage@ value with something more meaningful.
newtype Height w =
  Height w
  deriving (Eq, Show)

instance (Height Float) ~ t => Rewrapped (Height Float) t
instance (Height LengthOrPercentage) ~ t => Rewrapped (Height LengthOrPercentage) t


instance Wrapped (Height w) where
  type Unwrapped (Height w) = w
  _Wrapped' = iso (\(Height x) -> x) Height

_HeightLOP :: Iso' (Height LengthOrPercentage) LengthOrPercentage
_HeightLOP = _Wrapped

_HeightFloat :: Iso' (Height Float) Float
_HeightFloat = _Wrapped

_Height :: Iso' (Height w) w
_Height = _Wrapped'
