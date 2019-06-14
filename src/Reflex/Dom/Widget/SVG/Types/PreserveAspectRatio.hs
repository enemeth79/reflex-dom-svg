{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and function for the @PreserveAspectRatio@ wrapper.
module Reflex.Dom.Widget.SVG.Types.PreserveAspectRatio
  ( PreserveAspectRatio (..)
  , Align (..)
  , MeetOrSlice (..)
  , preserveAspectRatio_align
  , preserveAspectRatio_meetOrSlice
  , makePreserveAspectRatio
  ) where

import           Control.Lens (Lens', to, (^.), _Just)

import           Data.Text    (Text)
import qualified Data.Text    as Text


-- | Capture the information about the @preserveAspectRatio@ attribute <https://www.w3.org/TR/SVG2/coords.html#PreserveAspectRatioAttribute align> value.
data Align
  = Align_None
  | Align_XMinYMin
  | Align_XMidYMin
  | Align_XMaxYMin
  | Align_XMinYMid
  | Align_XMidYMid
  | Align_XMaxYMid
  | Align_XMinYMax
  | Align_XMidYMax
  | Align_XMaxYMax
  deriving (Eq)

instance Show Align where
  show Align_None     = "none"
  show Align_XMinYMin = "xMinYMin"
  show Align_XMidYMin = "xMidYMin"
  show Align_XMaxYMin = "xMaxYMin"
  show Align_XMinYMid = "xMinYMid"
  show Align_XMidYMid = "xMidYMid"
  show Align_XMaxYMid = "xMaxYMid"
  show Align_XMinYMax = "xMinYMax"
  show Align_XMidYMax = "xMidYMax"
  show Align_XMaxYMax = "xMaxYMax"


-- | Capture the information about the @preserveAspectRatio@ attribute <https://www.w3.org/TR/SVG2/coords.html#PreserveAspectRatioAttribute meetOrSlice> value.
data MeetOrSlice
  = MeetOrSlice_Meet
  | MeetOrSlice_Slice
  deriving (Eq)

instance Show MeetOrSlice where
  show MeetOrSlice_Meet  = "meet"
  show MeetOrSlice_Slice = "slice"


-- | Properties for the <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAspectRatio \<preserveAspectRatio\>> element.
data PreserveAspectRatio = PreserveAspectRatio
  { _preserveAspectRatio_align       :: Align
  , _preserveAspectRatio_meetOrSlice :: Maybe MeetOrSlice
  }
  deriving (Eq, Show)

-- | Lens for the @Align@ of an @PreserveAspectRatio@ element.
preserveAspectRatio_align :: Lens' PreserveAspectRatio Align
preserveAspectRatio_align f (PreserveAspectRatio x1 x2)
  = fmap (\y -> PreserveAspectRatio y x2) (f x1)
{-# INLINE preserveAspectRatio_align #-}

-- | Lens for the @MeetOrSlice@ of an @PreserveAspectRatio@ element.
preserveAspectRatio_meetOrSlice :: Lens' PreserveAspectRatio ( Maybe MeetOrSlice )
preserveAspectRatio_meetOrSlice f (PreserveAspectRatio x1 x2)
  = fmap (\y -> PreserveAspectRatio x1 y) (f x2)
{-# INLINE preserveAspectRatio_meetOrSlice #-}


makePreserveAspectRatio
  :: PreserveAspectRatio
  -> Text
makePreserveAspectRatio PreserveAspectRatio{..} =
  Text.stripEnd . Text.unwords $ Text.pack <$>
  [ _preserveAspectRatio_align ^. to show
  , _preserveAspectRatio_meetOrSlice ^. _Just . to show
  ]
