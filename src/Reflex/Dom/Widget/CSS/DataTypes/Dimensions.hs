-- | Re-exports all of the individual Dimensions Types
module Reflex.Dom.Widget.CSS.DataTypes.Dimensions
  (
    module Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Angle
  , module Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Frequency
  , module Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Length
  , module Reflex.Dom.Widget.CSS.DataTypes.Dimensions.LengthOrPercentage
  , module Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Time
  )
  where

import           Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Angle
import           Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Frequency
import           Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Length
import           Reflex.Dom.Widget.CSS.DataTypes.Dimensions.LengthOrPercentage
import           Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Time


-- | <https://www.w3.org/TR/css-values/#dimensions \<dimension\> >.
-- The general term dimension refers to a number with a unit attached to it; and is denoted by \<dimension\>.
-- When written literally, a dimension is a number immediately followed by a unit identifier, which is an identifier. Unit identifiers are ASCII case-insensitive.
-- CSS uses \<dimension\>s to specify
--   * angles (\<angle\>)
--   * distances (\<length\>),
--   * durations (\<time\>),
--   * frequencies (\<frequency\>),
--   * resolutions (\<resolution\>),
--   * and other quantities.

-- | angles (\<angle\>)

-- | distances (\<length\>)

-- | durations (\<time\>)

-- | frequencies (\<frequency\>)

-- | resolutions (\<resolution\>)
