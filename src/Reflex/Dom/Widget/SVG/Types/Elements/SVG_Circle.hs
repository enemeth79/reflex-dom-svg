{-# LANGUAGE OverloadedStrings #-}
-- | Types and functions for the \<circle\> SVG element.
module Reflex.Dom.Widget.SVG.Types.Elements.SVG_Circle
  ( SVG_Circle (..)
  , svg_circle_pos_centerX
  , svg_circle_pos_centerY
  , svg_circle_radius
  , makeCircleProps
  , circle_
  ) where

import           Control.Lens                                (Lens', at, (?~),
                                                              (^.))

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
import           Reflex.Dom.Widget.SVG.Types.Internal.Pos    (CenterX, CenterY,
                                                              Pos)
import           Reflex.Dom.Widget.SVG.Types.Properties      (Radius)
import           Reflex.Dom.Widget.SVG.Types.SVGEl           (svgElDynAttr')

-- | Properties for the <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/circle \<circle\>> element.
data SVG_Circle = SVG_Circle
  { _svg_circle_pos_centerX :: Pos CenterX
  , _svg_circle_pos_centerY :: Pos CenterY
  , _svg_circle_radius      :: Radius ()
  }
  deriving (Eq, Show)


-- | Lens for the Center X position of an @SVG_Circle@
svg_circle_pos_centerX :: Lens' SVG_Circle (Pos CenterX)
svg_circle_pos_centerX f (SVG_Circle x1 x2 x3)
  = fmap (\y1 -> SVG_Circle y1 x2 x3) (f x1)
{-# INLINE svg_circle_pos_centerX #-}

-- | Lens for the Center Y position of an @SVG_Circle@
svg_circle_pos_centerY :: Lens' SVG_Circle (Pos CenterY)
svg_circle_pos_centerY f (SVG_Circle x1 x2 x3)
  = fmap (\y1 -> SVG_Circle x1 y1 x3) (f x2)
{-# INLINE svg_circle_pos_centerY #-}

-- | Lens for the @Radius@ of an @SVG_Circle@
svg_circle_radius :: Lens' SVG_Circle (Radius ())
svg_circle_radius f (SVG_Circle x1 x2 x3)
  = fmap (SVG_Circle x1 x2) (f x3)
{-# INLINE svg_circle_radius #-}

-- | Convert the given properties to the correct attributes for a \<circle\>.
makeCircleProps
  :: SVG_Circle
  -> Map Text Text
makeCircleProps c = mempty
  & at "cx" ?~ c ^. svg_circle_pos_centerX . wrappedToText
  & at "cy" ?~ c ^. svg_circle_pos_centerY . wrappedToText
  & at "r"  ?~ c ^. svg_circle_radius . wrappedToText


circle_
  :: ( DomBuilder t m
     , PostBuild t m
     , R.Reflex t
     )
  => Map Text Text  -- properties
  -> Dynamic t SVG_Circle -- dAttributes
  -> m a  -- children
  -> m ( Element EventResult (DomBuilderSpace m) t, a)
circle_ attrs dCircle children =
  svgElDynAttr'
    "circle"
    ((mappend attrs . makeCircleProps) <$> dCircle)
    children
