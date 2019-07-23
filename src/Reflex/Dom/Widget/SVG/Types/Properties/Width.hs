{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
module Reflex.Dom.Widget.SVG.Types.Properties.Width
  ( Width (..)
  , _Width
  ) where

import           Control.Lens                                                  (Iso',
                                                                                Rewrapped,
                                                                                Wrapped (..),
                                                                                iso,
                                                                                _Wrapped)
import           Reflex.Dom.Widget.CSS.DataTypes.Dimensions.LengthOrPercentage

-- | Wrap the @Float@ and @LengthOrPercentage@ value with something more meaningful.
newtype Width w =
  Width w
  deriving (Eq, Show)

instance (Width Float) ~ t => Rewrapped (Width Float) t
instance (Width LengthOrPercentage) ~ t => Rewrapped (Width LengthOrPercentage) t


instance Wrapped (Width w) where
  type Unwrapped (Width w) = w
  _Wrapped' = iso (\(Width x) -> x) Width


_WidthLOP :: Iso' (Width LengthOrPercentage) LengthOrPercentage
_WidthLOP = _Wrapped

_WidthFloat :: Iso' (Width Float) Float
_WidthFloat = _Wrapped

_Width :: Iso' (Width w) w
_Width = _Wrapped'
