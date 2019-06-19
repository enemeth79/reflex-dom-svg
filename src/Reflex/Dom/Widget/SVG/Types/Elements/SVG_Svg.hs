{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Types and functions for the \<svg\> SVG element.
module Reflex.Dom.Widget.SVG.Types.Elements.SVG_Svg
  ( SVG_Svg (..)
  , svg_root_width
  , svg_root_height
  , svg_root_viewBox
  , makeSVGProps
  , svg_
  )
  where

import           Control.Lens                                    (Lens', at, to,
                                                                  (.~), (^.),
                                                                  (^?), _Just)

import           Data.Function                                   ((&))

import           Data.Text                                       (Text)

import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map

import           Reflex                                          (Dynamic)
import qualified Reflex                                          as R
import           Reflex.Dom.Core                                 (DomBuilder, DomBuilderSpace,
                                                                  Element,
                                                                  EventResult,
                                                                  PostBuild)

import           Reflex.Dom.Widget.SVG.Types.Internal            (Height, Width)
import           Reflex.Dom.Widget.SVG.Types.Internal.Helper     (wrappedToText)

import           Reflex.Dom.Widget.SVG.Types.PreserveAspectRatio
import           Reflex.Dom.Widget.SVG.Types.SVGEl               (svgElDynAttr')
import           Reflex.Dom.Widget.SVG.Types.ViewBox

-- | Minimum information required for building a SVG root element.
data SVG_Svg = SVG_Svg
  { _svg_root_width               :: Width
  , _svg_root_height              :: Height
  , _svg_root_viewBox             :: Maybe ViewBox
  , _svg_root_preserveAspectRatio :: Maybe PreserveAspectRatio
  }
  deriving (Eq, Show)


-- | Lens for @Height@ attribute on @SVG_Svg@
svg_root_height :: Lens' SVG_Svg Height
svg_root_height f (SVG_Svg x1 x2 v p) = fmap (\x2' -> SVG_Svg x1 x2' v p) (f x2)
{-# INLINE svg_root_height #-}

-- | Lens for @Width@ attribute on @SVG_Svg@
svg_root_width :: Lens' SVG_Svg Width
svg_root_width f (SVG_Svg x1 x2 v p) = fmap (\x1' -> SVG_Svg x1' x2 v p) (f x1)
{-# INLINE svg_root_width #-}

-- | Lens for @ViewBox@ attribute on @SVG_Svg@
svg_root_viewBox :: Lens' SVG_Svg ( Maybe ViewBox )
svg_root_viewBox f (SVG_Svg x1 x2 v p) = fmap (\v' -> SVG_Svg x1 x2 v' p) (f v)
{-# INLINE svg_root_viewBox #-}

-- | Lens for @PreserveAspectRatio@ attribute on @SVG_Svg@
svg_root_preserveAspectRatio :: Lens' SVG_Svg ( Maybe PreserveAspectRatio )
svg_root_preserveAspectRatio f (SVG_Svg x1 x2 v p) = fmap (\p' -> SVG_Svg x1 x2 v p') (f p)
{-# INLINE svg_root_preserveAspectRatio #-}


-- | Convert the record to the correct attribute map for Reflex.
makeSVGProps
  :: SVG_Svg
  -> Map Text Text
makeSVGProps s = Map.fromList
  [ ("width", s ^. svg_root_width . wrappedToText )
  , ("height", s ^. svg_root_height . wrappedToText )
  , ("xmlns", "http://www.w3.org/2000/svg" )
  ]
  & at "viewBox" .~ s ^? svg_root_viewBox . _Just . to makeViewBox
  & at "preserveAspectRatio" .~ s ^? svg_root_preserveAspectRatio . _Just . to makePreserveAspectRatio


-- | Create the Root SVG element.
--
-- Note that there are not restrictions on the inner element, apart from the
-- return type being of @m (SVGEl t m)@. So you are free to place whatever you
-- like in there, but bear in mind that the browser rules for SVG are still in
-- play. So text inputs etc, won't work.
svg_
  :: ( DomBuilder t m
     , PostBuild t m
     , R.Reflex t
     )
  => Dynamic t SVG_Svg -- dAttributes
  -> m a  -- children
  -> m ( Element EventResult (DomBuilderSpace m) t, a)
svg_ dAttrs children =
  svgElDynAttr'
    "svg"
    (makeSVGProps <$> dAttrs)
    children
