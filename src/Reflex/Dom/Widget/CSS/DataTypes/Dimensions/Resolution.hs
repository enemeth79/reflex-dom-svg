{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and function for the @Resolution@.
module Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Resolution
  ( Resolution (..)
  , resolutionToText
  ) where

import           Control.Lens                                (Prism', prism)

import           Data.Text                                   (Text, unpack)
import           Reflex.Dom.Widget.SVG.Types.Internal.Helper (floatToText)


-- | Resolution units are dimensions denoted by <resolution>. The resolution unit identifiers are: 'dpi', 'dpcm', 'dppx '.
data Resolution
  = Dpi Float
  | Dpcm Float
  | Dppx Float
  deriving Eq

instance Show Resolution where
  show = unpack . resolutionToText

-- | Classy Prism set for the @Resolution@ type
class AsResolution r where
  _Resolution :: Prism' r Resolution -- ^ General prism for when you have to deal with all of the constructors
  _Dpi :: Prism' r Float -- ^ Prism for dpi
  _Dpcm  :: Prism' r Float -- ^ Prism for dpcm
  _Dppx  :: Prism' r Float -- ^ Prism for dppx
  _Dpi  = _Resolution . _Dpi
  _Dpcm  = _Resolution . _Dpcm
  _Dppx = _Resolution . _Dppx

instance AsResolution Resolution where
  _Resolution = id
  _Dpi = prism Dpi
    (\case Dpi d -> Right d
           x     -> Left x
    )
  _Dpcm = prism Dpcm
    (\case Dpcm d -> Right d
           x      -> Left x
    )
  _Dppx = prism Dppx
    (\case Dppx d -> Right d
           x      -> Left x
    )

resolutionToText :: Resolution -> Text
resolutionToText (Dpi r)  = floatToText "dpi" r
resolutionToText (Dpcm r) = floatToText "dpcm" r
resolutionToText (Dppx r) = floatToText "dppx" r
