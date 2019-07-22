{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and function for the @Percentage@ type.
module Reflex.Dom.Widget.CSS.DataTypes.Percentage
  ( Percentage
  , percentageToText
  ) where

import           Data.Text                                   (Text)

import           Reflex.Dom.Widget.SVG.Types.Internal.Helper (floatToText)

-- | Percentage: the \<percentage\> type.
-- A percentage consists of a number immediately followed by a percent sign `%`.
type Percentage = Float

percentageToText :: Percentage -> Text
percentageToText = floatToText "%"
