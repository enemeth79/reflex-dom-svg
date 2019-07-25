{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Types and functions for the \<polygon\> SVG element.
--
-- The \<polygon\> element defines a closed shape consisting of a set of connected
-- straight line segments. The last point is connected to the first point. For
-- open shapes see the \<polyline\> element.
module Reflex.Dom.Widget.SVG.Types.Elements.SVG_Polygon
  ( SVG_Polygon (..)
  , svg_polygon_path
  , svg_polygon_start
  , makePolygonProps
  , polygon_
  ) where

import           Control.Lens                             (Lens')

import           Data.List.NonEmpty                       (NonEmpty, (<|))
import           Data.Map                                 (Map)
import           Data.Text                                (Text)

import           Reflex                                   (Dynamic)
import qualified Reflex                                   as R
import           Reflex.Dom.Core                          (DomBuilder,
                                                           DomBuilderSpace,
                                                           Element, EventResult,
                                                           PostBuild, (=:))

import           Reflex.Dom.Widget.SVG.Types.Internal.Pos (Pos, X, Y,
                                                           makePointsProp)
import           Reflex.Dom.Widget.SVG.Types.SVGEl        (svgElDynAttr')

-- | Properties for the <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/polygon \<polygon\>> element.
data SVG_Polygon = SVG_Polygon
  { _svg_polygon_start :: (Pos X, Pos Y)
  , _svg_polygon_path  :: NonEmpty (Pos X, Pos Y)
  }
  deriving (Eq, Show)

-- | Lens for the list of @(Pos X, Pos Y)@ of an @SVG_Polygon@ path attribute.
svg_polygon_path :: Lens' SVG_Polygon (NonEmpty (Pos X, Pos Y))
svg_polygon_path f (SVG_Polygon x1 x2) = fmap (SVG_Polygon x1) (f x2)
{-# INLINE svg_polygon_path #-}

-- | Lens for the starting @(Pos X, Pos Y)@ of an @SVG_Polygon@ element.
svg_polygon_start :: Lens' SVG_Polygon (Pos X, Pos Y)
svg_polygon_start f (SVG_Polygon x1 x2) = fmap (`SVG_Polygon` x2) (f x1)
{-# INLINE svg_polygon_start #-}

-- | Convert the given properties to the correct 'points' attribute of a \<polygon\>.
makePolygonProps
  :: SVG_Polygon
  -> Map Text Text
makePolygonProps SVG_Polygon {..} =
  "points" =: makePointsProp (_svg_polygon_start <| _svg_polygon_path)


polygon_
  :: ( DomBuilder t m
     , PostBuild t m
     , R.Reflex t
     )
  => Map Text Text      -- properties
  -> Dynamic t SVG_Polygon -- dAttributes
  -> m a  -- children
  -> m ( Element EventResult (DomBuilderSpace m) t, a)
polygon_ attrs dPolygon children =
  svgElDynAttr'
    "polygon"
    ((mappend attrs . makePolygonProps) <$> dPolygon)
    children
