-- | Types and function for the @LengthAdjust@ wrapper.
module Reflex.Dom.Widget.SVG.Types.Properties.LengthAdjust
  ( LengthAdjust (..)
  , makeLengthAdjust
  ) where

import           Data.Text (Text, pack)


-- | Capture the information about the @lengthAdjust@ attribute <https://www.w3.org/TR/SVG2/text.html#TextElementLengthAdjustAttribute lengthAdjust> value.
data LengthAdjust
  = LengthAdjust_Spacing
  | LengthAdjust_SpacingAndGlyphs
  deriving Eq

instance Show LengthAdjust where
  show LengthAdjust_Spacing          = "spacing"
  show LengthAdjust_SpacingAndGlyphs = "spacingAndGlyphs"

makeLengthAdjust
  :: LengthAdjust
  -> Text
makeLengthAdjust = pack . show
