{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Miscellaneous functions of the API.
module Reflex.Dom.Widget.SVG.Types.Internal.Helper
  ( wrappedToText
  ) where

import           Control.Lens   (Contravariant, Rewrapped, Wrapped (..), to,
                                 _Wrapped)

import           Data.Text.Lens (IsText, packed)


-- | Helper function to convert a @Wrapped@ value to a @Text@ value.
wrappedToText
  :: ( Unwrapped t ~ Unwrapped s
     , IsText t1
     , Contravariant f
     , Functor f
     , Rewrapped t s
     , Rewrapped s t
     , Show (Unwrapped s)
     )
  => (t1 -> f t1)
  -> s
  -> f t
wrappedToText =
  _Wrapped . to show . packed
