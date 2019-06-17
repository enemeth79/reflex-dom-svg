{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Types and functions for the \<g\> SVG element.
module Reflex.Dom.Widget.SVG.Types.Elements.SVG_Group
  ( SVG_Group (..)
  , makeGroupProps
  , group_
  )
  where

import           Data.Text                                       (Text)

import           Data.Map                                        (Map)

import           Reflex                                          (Dynamic)
import qualified Reflex                                          as R
import           Reflex.Dom.Core                                 (DomBuilder, DomBuilderSpace,
                                                                  Element,
                                                                  EventResult,
                                                                  PostBuild)

import           Reflex.Dom.Widget.SVG.Types.SVGEl               (svgElDynAttr')

-- | Properties for the <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/g \<g\>> element.
data SVG_Group = SVG_Group
  deriving (Eq, Show)


-- | Convert the given properties to the correct attributes for a \<g\>.
makeGroupProps
  :: SVG_Group
  -> Map Text Text
makeGroupProps _ = mempty


group_
  :: ( DomBuilder t m
     , PostBuild t m
     , R.Reflex t
     )
  => Map Text Text      -- properties
  -> Dynamic t SVG_Group -- dAttributes
  -> m a  -- children
  -> m ( Element EventResult (DomBuilderSpace m) t, a)
group_ attrs dGroup children =
  svgElDynAttr'
    "g"
    ((mappend attrs . makeGroupProps) <$> dGroup)
    children
