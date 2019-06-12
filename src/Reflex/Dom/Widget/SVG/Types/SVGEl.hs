{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Main functions for creating SVG dom elements via Reflex
module Reflex.Dom.Widget.SVG.Types.SVGEl
  ( SVGEl (..)
  , svgBasicDyn
  , svgBasicDyn_
  , svgElDynAttr'
  , svgElDynAttr_
  ) where

import           Control.Monad.Fix (MonadFix)
import           Data.Text         (Text)

import           Reflex            (Dynamic, MonadHold)
import           Reflex.Dom.Core   (DomBuilder, DomBuilderSpace, Element,
                                    EventResult, PostBuild)
import qualified Reflex.Dom.Core   as RD

import           Data.Map          (Map)


-- | This represents an SVG element, containing both the raw Reflex.Dom @El@ type
-- and a @Dynamic@ of all of the children that are nested in this element.
data SVGEl t m = SVGEl
  { _svgEl_el       :: Element EventResult (DomBuilderSpace m) t
  , _svgEl_children :: Dynamic t
        ( Map Text (Element EventResult (DomBuilderSpace m) t) )
  }

svgXMLNamespace :: Text
svgXMLNamespace = "http://www.w3.org/2000/svg"

-- | This is for creating a SVG element with @Dynamic@ attributes, and ensuring we
-- use the right namespace so the browser actually picks up on it. The name
-- space in use is "http://www.w3.org/2000/svg".
svgElDynAttr'
  :: forall t m a. ( DomBuilder t m
                   , PostBuild t m
                   )
  => Text  -- tag
  -> Dynamic t (Map Text Text)  -- attribute map
  -> m a  -- children
  -> m (Element EventResult (DomBuilderSpace m) t, a)
svgElDynAttr' = RD.elDynAttrNS'
  (Just svgXMLNamespace)


-- | As per @svgElDynAttr'@, but does not have any children.
svgElDynAttr_
  :: forall t m. ( DomBuilder t m
                 , PostBuild t m
                 )
  => Text
  -> Dynamic t (Map Text Text)
  -> m (Element EventResult (DomBuilderSpace m) t)
svgElDynAttr_ tag dAttrs = fst <$> RD.elDynAttrNS'
  (Just svgXMLNamespace)
  tag
  dAttrs
  RD.blank


-- | Create a SVG element that has dynamic attributes and contains children.
-- The SVG element will have some @Dynamic@ properties and a function that
-- allows these properties to be converted into a @Map Text Text@, inline with
-- other Reflex.Dom widgets.
svgBasicDyn
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => Text
  -> ( p -> Map Text Text )
  -> Dynamic t p
  -> Dynamic t ( Map Text (Map Text Text) )
  -> m ( SVGEl t m )
svgBasicDyn tag propFn dProps dInnerElMap =
  fmap
    (uncurry SVGEl)
    (svgElDynAttr'
      tag
      (propFn <$> dProps)
      (RD.listWithKey
        dInnerElMap
        (\innerTag dInnerAttrs ->
          fst <$> svgElDynAttr'
                    innerTag
                    dInnerAttrs
                    RD.blank -- NOTE: TODO: no children yet
        )
      )
    )


-- | As per the @svgBasicDyn@ function, except with no inner elements.
svgBasicDyn_
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => Text
  -> ( p -> Map Text Text )
  -> Dynamic t p
  -> m ( SVGEl t m )
svgBasicDyn_ tag propFn dProps =
  svgBasicDyn tag propFn dProps (pure mempty)
