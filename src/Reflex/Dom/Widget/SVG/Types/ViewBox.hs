{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and function for the @ViewBox@ wrapper.
module Reflex.Dom.Widget.SVG.Types.ViewBox
  ( ViewBox (..)
  , viewBox_height
  , viewBox_width
  , viewBox_min_x
  , viewBox_min_y
  , makeViewBox
  )
  where

import           Control.Lens                           (Lens', (^.), _Wrapped)

import           Data.Text                              (Text)
import qualified Data.Text                              as Text

import           Reflex.Dom.Widget.SVG.Types.Internal   (Height)
import           Reflex.Dom.Widget.SVG.Types.Properties (Width)


-- | SVG @viewBox@ attribute
data ViewBox = ViewBox
  { _viewBox_min_X  :: Float
  , _viewBox_min_Y  :: Float
  , _viewBox_width  :: Width Float
  , _viewBox_height :: Height
  }
  deriving (Eq, Show)

-- | Lens for @_viewBox_min_X@ attribute on @ViewBox@
viewBox_min_x :: Lens' ViewBox Float
viewBox_min_x f (ViewBox minX minY w h) = fmap (\minX' -> ViewBox minX' minY w h) (f minX)
{-# INLINE viewBox_min_x #-}

-- | Lens for @_viewBox_min_Y@ attribute on @ViewBox@
viewBox_min_y :: Lens' ViewBox Float
viewBox_min_y f (ViewBox minX minY w h) = fmap (\minY' -> ViewBox minX minY' w h) (f minY)
{-# INLINE viewBox_min_y #-}

-- | Lens for @_viewBox_width@ attribute on @ViewBox@
viewBox_width :: Lens' ViewBox (Width Float)
viewBox_width f (ViewBox minX minY w h) = fmap (\w' -> ViewBox minX minY w' h) (f w)
{-# INLINE viewBox_width #-}

-- | Lens for @_viewBox_min_X@ attribute on @ViewBox@
viewBox_height :: Lens' ViewBox Height
viewBox_height f (ViewBox minX minY w h) = fmap (ViewBox minX minY w) (f h)
{-# INLINE viewBox_height #-}


makeViewBox
  :: ViewBox
  -> Text
makeViewBox ViewBox {..} = Text.unwords $ Text.pack . show <$>
  [ _viewBox_min_X
  , _viewBox_min_Y
  , _viewBox_width ^. _Wrapped
  , _viewBox_height ^. _Wrapped
  ]
