{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and function for the @Length@.
module Reflex.Dom.Widget.CSS.DataTypes.Dimensions.Length
  ( Length (..)
  , lengthToText
  ) where

import           Control.Lens                                (Prism', prism)

import           Data.Text                                   (Text, unpack)

import           Reflex.Dom.Widget.SVG.Types.Internal.Helper (floatToText)


-- | Distance Units: the \<length\> type
-- Lengths refer to distance measurements and are denoted by \<length\> in the property definitions. A length is a dimension.
data Length
  =
-- Viewport-percentage lengths: the vw, vh, vi, vb, vmin, vmax units
    Vw Float
  | Vh Float
  | Vi Float
  | Vb Float
  | Vmin Float
  | Vmax Float
-- Font-relative lengths: the em, ex, cap, ch, ic, rem, lh, rlh units
  | Em  Float
  | Ex  Float
  | Cap Float
  | Ch  Float
  | Ic  Float
  | Rem Float
  | Lh  Float
  | Rlh Float
-- Physical units (in, cm, mm, pt, pc, Q):
  | Cm Float
  | Mm Float
  | Q  Float
  | In Float
  | Pc Float
  | Pt Float
-- Visual angle/pixel unit (px):
  | Px Float
  deriving Eq


instance Show Length where
  show = unpack . lengthToText

-- | Classy Prism set for the @Length@ type
class AsLength r where
  _Length :: Prism' r Length -- ^ General prism for when you have to deal with all of the constructors
  _Vw :: Prism' r Float  -- ^ Prism for 'vw'
  _Vh :: Prism' r Float  -- ^ Prism for 'vh'
  _Vi :: Prism' r Float  -- ^ Prism for 'vi'
  _Vb :: Prism' r Float  -- ^ Prism for 'vb'
  _Vmin :: Prism' r Float  -- ^ Prism for 'vmin'
  _Vmax :: Prism' r Float  -- ^ Prism for 'vmax'
  _Vw = _Length . _Vw
  _Vh = _Length . _Vh
  _Vi = _Length . _Vi
  _Vb = _Length . _Vb
  _Vmin = _Length . _Vmin
  _Vmax = _Length . _Vmax
  _Em  :: Prism' r Float  -- ^ Prism for 'em'
  _Ex  :: Prism' r Float  -- ^ Prism for 'ex'
  _Cap :: Prism' r Float  -- ^ Prism for 'cap'
  _Ch  :: Prism' r Float  -- ^ Prism for 'ch'
  _Ic  :: Prism' r Float  -- ^ Prism for 'ic'
  _Rem :: Prism' r Float  -- ^ Prism for 'rem'
  _Lh  :: Prism' r Float  -- ^ Prism for 'lh'
  _Rlh :: Prism' r Float  -- ^ Prism for 'rlh'
  _Em  = _Length . _Em
  _Ex  = _Length . _Ex
  _Cap = _Length . _Cap
  _Ch  = _Length . _Ch
  _Ic  = _Length . _Ic
  _Rem = _Length . _Rem
  _Lh  = _Length . _Lh
  _Rlh = _Length . _Rlh
  _Cm :: Prism' r Float  -- ^ Prism for 'cm'
  _Mm :: Prism' r Float  -- ^ Prism for 'mm'
  _Q  :: Prism' r Float  -- ^ Prism for 'Q'
  _In :: Prism' r Float  -- ^ Prism for 'in'
  _Pc :: Prism' r Float  -- ^ Prism for 'pc'
  _Pt :: Prism' r Float  -- ^ Prism for 'pt'
  _Cm = _Length . _Cm
  _Mm = _Length . _Mm
  _Q  = _Length . _Q
  _In = _Length . _In
  _Pc = _Length . _Pc
  _Pt = _Length . _Pt
  _Px  :: Prism' r Float  -- ^ Prism for 'px'
  _Px  = _Length . _Px

instance AsLength Length where
  _Length = id
  _Vw = prism Vw
    (\case Vw d -> Right d
           x    -> Left  x
    )
  _Vh = prism Vh
    (\case Vh d -> Right d
           x    -> Left  x
    )
  _Vi = prism Vi
    (\case Vi d -> Right d
           x    -> Left  x
    )
  _Vb = prism Vb
    (\case Vb d -> Right d
           x    -> Left  x
    )
  _Vmin = prism Vmin
    (\case Vmin d -> Right d
           x      -> Left  x
    )
  _Vmax = prism Vmax
    (\case Vmax d -> Right d
           x      -> Left  x
    )
--
  _Em = prism Em
    (\case Em d -> Right d
           x    -> Left  x
    )
  _Ex = prism Ex
    (\case Ex d -> Right d
           x    -> Left  x
    )
  _Cap = prism Cap
    (\case Cap d -> Right d
           x     -> Left  x
    )
  _Ch = prism Ch
    (\case Ch d -> Right d
           x    -> Left  x
    )
  _Ic = prism Ic
    (\case Ic d -> Right d
           x    -> Left  x
    )
  _Rem = prism Rem
    (\case Rem d -> Right d
           x     -> Left  x
    )
  _Lh = prism Lh
    (\case Lh d -> Right d
           x    -> Left  x
    )
  _Rlh = prism Rlh
    (\case Rlh d -> Right d
           x     -> Left  x
    )
  --
  _Cm = prism Cm
    (\case Cm d -> Right d
           x    -> Left  x
    )
  _Mm = prism Mm
    (\case Mm d -> Right d
           x    -> Left  x
    )
  _Q = prism Q
    (\case Q d -> Right d
           x   -> Left  x
    )
  _In = prism In
    (\case In d -> Right d
           x    -> Left  x
    )
  _Pc = prism Pc
    (\case Pc d -> Right d
           x    -> Left  x
    )
  _Pt = prism Pt
    (\case Pt d -> Right d
           x    -> Left  x
    )
  --
  _Px = prism Px
    (\case Px d -> Right d
           x    -> Left  x
    )

lengthToText :: Length -> Text
lengthToText (Vw v)   = floatToText "vw" v
lengthToText (Vh v)   = floatToText "vh" v
lengthToText (Vi v)   = floatToText "vi" v
lengthToText (Vb v)   = floatToText "vb" v
lengthToText (Vmin v) = floatToText "vmin" v
lengthToText (Vmax v) = floatToText "vmax" v
--
lengthToText (Em a)   = floatToText "em" a
lengthToText (Ex a)   = floatToText "ex" a
lengthToText (Cap a)  = floatToText "cap" a
lengthToText (Ch a)   = floatToText "ch" a
lengthToText (Ic a)   = floatToText "ic" a
lengthToText (Rem a)  = floatToText "rem" a
lengthToText (Lh a)   = floatToText "lh" a
lengthToText (Rlh a)  = floatToText "rlh" a
--
lengthToText (Cm a)   = floatToText "cm" a
lengthToText (Mm a)   = floatToText "mm" a
lengthToText (Q a)    = floatToText "Q" a
lengthToText (In a)   = floatToText "in" a
lengthToText (Pc a)   = floatToText "pc" a
lengthToText (Pt a)   = floatToText "pt" a
--
lengthToText (Px a)   = floatToText "px" a
