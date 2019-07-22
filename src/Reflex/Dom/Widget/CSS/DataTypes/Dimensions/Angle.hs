{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and function for the @Angle@ wrapper.
module Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Angle
  ( Angle (..)
  , AsAngle (..)
  , angleToText
  ) where

import           Control.Lens                                (Prism', prism)

import           Data.Text                                   (Text)

import           Reflex.Dom.Widget.SVG.Types.Internal.Helper (floatToText)

-- | Angle values are \<dimension\>s denoted by \<angle\>. The angle unit identifiers are:  deg, grad, rad, turn units.
data Angle
  = Deg Float
  | Grad Float
  | Rad Float
  | Turn Float
  deriving (Eq, Show)

-- | Classy Prism set for the @Angle@ type
class AsAngle r where
  _Angle :: Prism' r Angle -- ^ General prism for when you have to deal with all of the constructors
  _Deg  :: Prism' r Float  -- ^ Prism for 'deg'
  _Grad :: Prism' r Float  -- ^ Prism for 'grad'
  _Rad  :: Prism' r Float  -- ^ Prism for 'rad'
  _Turn :: Prism' r Float  -- ^ Prism for 'turn'
  _Deg  = _Angle . _Deg
  _Grad = _Angle . _Grad
  _Rad  = _Angle . _Rad
  _Turn = _Angle . _Turn

instance AsAngle Angle where
  _Angle = id
  _Deg = prism Deg
    (\case Deg d -> Right d
           x     -> Left  x
    )
  _Grad = prism Grad
    (\case Grad d -> Right d
           x      -> Left  x
    )
  _Rad = prism Rad
    (\case Rad d -> Right d
           x     -> Left  x
    )
  _Turn = prism Turn
    (\case Turn d -> Right d
           x      -> Left  x
    )

angleToText :: Angle -> Text
angleToText (Deg a)  = floatToText "deg" a
angleToText (Grad a) = floatToText "grad" a
angleToText (Rad a)  = floatToText "rad" a
angleToText (Turn a) = floatToText "turn" a
