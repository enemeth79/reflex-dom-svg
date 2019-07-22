{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and function for the @LengthOrPercentage@.
module Reflex.Dom.Widget.CSS.DataTypes.Dimensions.LengthOrPercentage
  ( LengthOrPercentage (..)
  , lengthOrPercentageToText
  ) where

import           Data.Text                                         (Text)

import           Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Length
import           Reflex.Dom.Widget.CSS.DataTypes.Percentage


-- | \<length-percentage\>
--  Equivalent to [ <length> | <percentage> ], where the \<percentage\> will resolve to a \<length\>.
data LengthOrPercentage
  = Length Length -- ^ Relative length units specify a length relative to another length.
  | Percent Percentage
  deriving (Eq, Show)

lengthOrPercentageToText :: LengthOrPercentage -> Text
lengthOrPercentageToText (Length l)  = lengthToText l
lengthOrPercentageToText (Percent p) = percentageToText p
