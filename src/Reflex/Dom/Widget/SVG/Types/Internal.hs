{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Miscellaneous types and functions of the API. This module is named Internal
-- as it is possible it will change.
module Reflex.Dom.Widget.SVG.Types.Internal
  ( Width (..)
  , Height (..)
  , AttributeName (..)
  , RepeatCount (..)
  ) where

import           Control.Lens (Rewrapped, Wrapped (..), iso, to, _Wrapped)

import           Data.Text    (Text)


-- | Wrap the @Float@ value with something more meaningful.
newtype Width         = Width Float deriving (Eq, Show)
-- | Wrap the @Float@ value with something more meaningful.
newtype Height        = Height Float deriving (Eq, Show)
-- | Wrap the @Text@ value with something more meaningful.
newtype AttributeName = AttributeName Text deriving (Eq, Show)

instance Width ~ t => Rewrapped Width t
instance Wrapped Width where
  type Unwrapped Width = Float
  _Wrapped' = iso (\(Width x) -> x) Width

instance Height ~ t => Rewrapped Height t
instance Wrapped Height where
  type Unwrapped Height = Float
  _Wrapped' = iso (\(Height x) -> x) Height

instance AttributeName ~ t => Rewrapped AttributeName t
instance Wrapped AttributeName where
  type Unwrapped AttributeName = Text
  _Wrapped' = iso (\ (AttributeName x) -> x) AttributeName

-- | Capture the information about the <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/repeatCount repeatCount> attribute.
data RepeatCount
  = NumOfTimes Word
  | Indefinite
  deriving Eq

instance Show RepeatCount where
  show (NumOfTimes n) = show n
  show Indefinite     = "indefinite"
