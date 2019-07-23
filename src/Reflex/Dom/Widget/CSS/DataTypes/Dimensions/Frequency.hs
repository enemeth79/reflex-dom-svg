{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and function for the @Frequency@ wrapper.
module Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Frequency
  ( Frequency (..)
  , frequencyToText
  ) where

import           Control.Lens                                (Prism', prism)

import           Data.Text                                   (Text, unpack)
import           Reflex.Dom.Widget.SVG.Types.Internal.Helper (floatToText)


-- | Frequency values are dimensions denoted by \<frequency\>. The frequency unit identifiers are: 'Hz' and 'kHz'.
data Frequency
  = Hz Float
  | KHz Float
  deriving Eq

instance Show Frequency where
  show = unpack . frequencyToText

-- | Classy Prism set for the @Frequency@ type
class AsFrequency r where
  _Frequency :: Prism' r Frequency -- ^ General prism for when you have to deal with all of the constructors
  _Hz  :: Prism' r Float -- ^ Prism for Hz
  _KHz  :: Prism' r Float -- ^ Prism for kHz
  _Hz  = _Frequency . _Hz
  _KHz = _Frequency . _KHz

instance AsFrequency Frequency where
  _Frequency = id
  _Hz = prism Hz
    (\case Hz d -> Right d
           x    -> Left x
    )
  _KHz = prism KHz
    (\case KHz d -> Right d
           x     -> Left x
    )

frequencyToText :: Frequency -> Text
frequencyToText (Hz f)  = floatToText "Hz" f
frequencyToText (KHz f) = floatToText "kHz" f
