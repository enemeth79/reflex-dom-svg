{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Types and lenses for the \<text\> SVG element.
module Reflex.Dom.Widget.SVG.Types.Elements.SVG_Text
  ( SVG_Text (..)
  , svg_text_pos_x
  , svg_text_pos_y
  , svg_text_pos_dx
  , svg_text_pos_dy
  , svg_text_rotate
  , svg_text_textLength
  , svg_text_lengthAdjust
  , makeTextProps
  , text_
  )
  where

import           Control.Lens                                (Lens', at, to,
                                                              (.~), (^?), _Just)

import           Data.Function                               ((&))

import           Data.Map                                    (Map)

import           Data.Text                                   (Text)

import           Reflex                                      (Dynamic)
import qualified Reflex                                      as R
import           Reflex.Dom.Core                             (DomBuilder,
                                                              DomBuilderSpace,
                                                              Element,
                                                              EventResult,
                                                              PostBuild)

import           Reflex.Dom.Widget.SVG.Types.Internal.Helper (wrappedToText)
import           Reflex.Dom.Widget.SVG.Types.Internal.Pos    (Pos, X, Y)
import           Reflex.Dom.Widget.SVG.Types.Properties      (LengthAdjust,
                                                              Rotate,
                                                              TextLength,
                                                              makeLengthAdjust)

import           Reflex.Dom.Widget.SVG.Types.SVGEl           (svgElDynAttr')


-- | SVG <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/text \<text\>> properties
data SVG_Text = SVG_Text
  { _svg_text_pos_x        :: Maybe (Pos X) -- ^ X position
  , _svg_text_pos_y        :: Maybe (Pos Y) -- ^ Y position
  , _svg_text_pos_dx       :: Maybe (Pos X) -- ^ Shift X position
  , _svg_text_pos_dy       :: Maybe (Pos Y) -- ^ Shift Y position
  , _svg_text_rotate       :: Maybe Rotate -- ^ Rotation
  , _svg_text_textLength   :: Maybe TextLength -- ^ A width the text should be scaled to fit
  , _svg_text_lengthAdjust :: Maybe LengthAdjust -- ^ Stretched or compressed
  }
  deriving (Eq, Show)

-- | Lens for the X position
svg_text_pos_x :: Lens' SVG_Text (Maybe (Pos X))
svg_text_pos_x f (SVG_Text x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_Text y1 x2 x3 x4 x5 x6 x7) (f x1)
{-# INLINE svg_text_pos_x #-}

-- | Lens for the Y position
svg_text_pos_y :: Lens' SVG_Text (Maybe (Pos Y))
svg_text_pos_y f (SVG_Text x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_Text x1 y1 x3 x4 x5 x6 x7) (f x2)
{-# INLINE svg_text_pos_y #-}

-- | Lens for the shift X position
svg_text_pos_dx :: Lens' SVG_Text (Maybe (Pos X))
svg_text_pos_dx f (SVG_Text x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_Text x1 x2 y1 x4 x5 x6 x7) (f x3)
{-# INLINE svg_text_pos_dx #-}

-- | Lens for the shift Y position
svg_text_pos_dy :: Lens' SVG_Text (Maybe (Pos Y))
svg_text_pos_dy f (SVG_Text x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_Text x1 x2 x3 y1 x5 x6 x7) (f x4)
{-# INLINE svg_text_pos_dy #-}

-- | Lens for the @rotate@ of a @SVG_Text@
svg_text_rotate :: Lens' SVG_Text (Maybe Rotate)
svg_text_rotate f (SVG_Text x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_Text x1 x2 x3 x4 y1 x6 x7) (f x5)
{-# INLINE svg_text_rotate #-}

-- | Lens for the @textLength@ of a @SVG_Text@
svg_text_textLength :: Lens' SVG_Text (Maybe TextLength)
svg_text_textLength f (SVG_Text x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_Text x1 x2 x3 x4 x5 y1 x7) (f x6)
{-# INLINE svg_text_textLength #-}

-- | Lens for the @lengthAdjust@ of a @SVG_Text@
svg_text_lengthAdjust :: Lens' SVG_Text (Maybe LengthAdjust)
svg_text_lengthAdjust f (SVG_Text x1 x2 x3 x4 x5 x6 x7)
  = fmap (\y1 -> SVG_Text x1 x2 x3 x4 x5 x6 y1) (f x7)
{-# INLINE svg_text_lengthAdjust #-}


-- | Convert the given properties to the correct attributes for a \<text\>.
makeTextProps
  :: SVG_Text
  -> Map Text Text
makeTextProps r = mempty
  & at "x" .~ r ^? svg_text_pos_x . _Just . wrappedToText
  & at "y" .~ r ^? svg_text_pos_y . _Just . wrappedToText
  & at "dx" .~ r ^? svg_text_pos_dx . _Just . wrappedToText
  & at "dy" .~ r ^? svg_text_pos_dy . _Just . wrappedToText
  & at "rotate" .~ r ^? svg_text_rotate . _Just . wrappedToText
  & at "textLength" .~ r ^? svg_text_textLength . _Just . wrappedToText
  & at "lengthAdjust" .~ r ^? svg_text_lengthAdjust . _Just . to makeLengthAdjust


text_
  :: ( DomBuilder t m
     , PostBuild t m
     , R.Reflex t
     )
  => Map Text Text      -- properties
  -> Dynamic t SVG_Text -- dAttributes
  -> m a  -- children
  -> m ( Element EventResult (DomBuilderSpace m) t, a)
text_ attrs dText children =
  svgElDynAttr'
    "text"
    ((mappend attrs . makeTextProps) <$> dText)
    children
