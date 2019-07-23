{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and function for the @Time@ wrapper.
module Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Time
  ( Time (..)
  , timeToText
  ) where

import           Control.Lens                                (Prism', prism)

import           Data.Text                                   (Text, unpack)
import           Reflex.Dom.Widget.SVG.Types.Internal.Helper (floatToText)


-- | Currently we're only implemented `Timecount-val`, "h" | "min" | "s" | "ms"
data Time
  = Hours Float
  | Mins Float
  | Secs Float
  | MSecs Float
  deriving Eq

instance Show Time where
  show = unpack . timeToText

-- | Classy Prism set for the @Time@ type
class AsTime r where
  _Time :: Prism' r Time -- ^ General prism for when you have to deal with all of the constructors
  _Hours :: Prism' r Float -- ^ Prism for Hours
  _Mins  :: Prism' r Float -- ^ Prism for Minutes
  _Secs  :: Prism' r Float -- ^ Prism for Seconds
  _MSecs :: Prism' r Float -- ^ Prism for Milliseconds
  _Hours = _Time . _Hours
  _Mins  = _Time . _Mins
  _Secs  = _Time . _Secs
  _MSecs = _Time . _MSecs

instance AsTime Time where
  _Time = id
  _Hours = prism Hours
    (\case Hours d -> Right d
           x       -> Left x
    )
  _Mins = prism Mins
    (\case Mins d -> Right d
           x      -> Left x
    )
  _Secs = prism Secs
    (\case Secs d -> Right d
           x      -> Left x
    )
  _MSecs = prism MSecs
    (\case MSecs d -> Right d
           x       -> Left x
    )

timeToText :: Time -> Text
timeToText (Hours t) = floatToText "h" t
timeToText (Mins t)  = floatToText "min" t
timeToText (Secs t)  = floatToText "s" t
timeToText (MSecs t) = floatToText "ms" t
