{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where

import           Control.Lens                (at, from, ( # ), (+~), (?~), (^?),
                                              _Wrapped)
import           Control.Monad.Fix           (MonadFix)
import           Data.Function               ((&))
import           Data.List.NonEmpty          (fromList)
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Monoid                 (mappend, mempty)

import           Reflex                      (Dynamic, MonadHold)
import qualified Reflex                      as R
import           Reflex.Dom                  (mainWidget)
import           Reflex.Dom.Core             (DomBuilder, PostBuild)
import qualified Reflex.Dom.Core             as RD

import qualified Reflex.Dom.Widget.CSS       as S
import qualified Reflex.Dom.Widget.SVG       as S
import qualified Reflex.Dom.Widget.SVG.Types as S


exampleUsage ::
  forall t m.
  ( DomBuilder t m
  , PostBuild t m
  , R.Reflex t
  ) => m ()
exampleUsage = do
  let
    -- Construct the ``<svg>`` element.
    dSvgProps = pure $
      S.SVG_Svg
        (S.Width (S.Length (S.Px 400)))
        (S.Height 300)
        Nothing  -- with no viewbox
        -- (Just  -- with viewbox
        --   (S.ViewBox 0 0 (S.Width 30) (S.Height 40)))
        Nothing -- no preserveAspectRatio
        -- (Just  -- with preserveAspectRatio
        --   (S.PreserveAspectRatio
        --     S.Align_XMaxYMax
        --     Nothing  -- with no meet|slice
        --     -- (Just S.MeetOrSlice_Meet)  -- with meet|slice
        --   )
        -- )

    -- Create a normal ``Map`` of HTML attributes to apply to the shapes
    attrsRect1 = mempty
      & at "id" ?~ "rect1"
      & at "class" ?~ "green yellow"
      & at "fill" ?~ "green"
      & at "stroke" ?~ "yellow"

    attrsBR = mempty
      & at "fill" ?~ "blue"
      & at "stroke" ?~ "red"

    attrsYG = mempty
      & at "fill" ?~ "yellow"
      & at "stroke" ?~ "green"

    attrsRB = mempty
      & at "fill" ?~ "red"
      & at "stroke" ?~ "black"

    attrsOrange = mempty
      & at "fill" ?~ "none"
      & at "stroke" ?~ "orange"

    -- Build our first ``<rect>``.
    dRect1 = pure $
      S.SVG_Rect
        (S._PosX # 40.0)
        (S._PosY # 40.0)
        (S.Width (S.Length (S.Px 50.0)))
        (S.Height 50.0)
        Nothing
        Nothing
    -- This is the same as writing: <rect x="40" y="40" height="50" width="50">.

    -- We can use lenses to modify the properties of our shape.
    shiftCircle :: Dynamic t S.SVG_Circle -> Dynamic t S.SVG_Circle
    shiftCircle = fmap (S.svg_circle_pos_centerX . S._PosCenterX +~ 70.0)

    -- We can also define a ``<rect>`` with corner radius.
    dRect3 = pure $
      S.SVG_Rect
        (S._PosX # 20.0)
        (S._PosY # 20.0)
        (S.Width (S.Length (S.Px 50.0)))
        (S.Height 50.0)
        (15.0 ^? from S._CornerRadiusX)
        (15.0 ^? from S._CornerRadiusY)
    -- This is the same as <rect x="20" y="20" height="50" width="50" rx="15" ry="15">.

    -- Build a ``<circle>``.
    dCircle = pure $
      S.SVG_Circle
        (S._PosCenterX # 200.0)
        (S._PosCenterY # 200.0)
        (S._Radius # 70.0)
    -- This is the same as writing: <circle cx="200" cy="200" r="70">.

    -- Build a ``<ellipse>``.
    dEllipse = pure $
      S.SVG_Ellipse
        (S._RadiusX # 100.0)
        (S._RadiusY # 50.0)
        (S._PosCenterX # 300.0)
        (S._PosCenterY # 150.0)
    -- This is the same as writing: <ellipse cx="300" cy="150" rx="100" ry="50">.

    -- Build a ``<line>``.
    dLine = pure $
      S.SVG_Line
        (S._PosX # 350, S._PosY # 80)
        (S._PosX # 490, S._PosY # 250)
    -- This is the same as writing: <line x1="0" y1="80" x2="100" y2="20"/>.

    -- Build a ``<polyline>``.
    dPolyLine = pure $
      S.SVG_PolyLine
        (S._PosX # 100, S._PosY # 100)
        (NonEmpty.fromList
          [ (S._PosX # 150, S._PosY # 25)
          , (S._PosX # 150, S._PosY # 75)
          , (S._PosX # 200, S._PosY # 0)
          ])
    -- This is the same as writing: <polyline points="100,100 150,25 150,75 200,0">

    -- Build a ``<polygon>``.
    dPolygon = pure $
      S.SVG_Polygon
        (S._PosX # 200, S._PosY # 100)
        (NonEmpty.fromList
          [ (S._PosX # 250, S._PosY # 25)
          , (S._PosX # 250, S._PosY # 75)
          , (S._PosX # 300, S._PosY # 0)
          ])
    -- This is the same as writing: <polygon points="200,100 250,25 250,75 300,0"/>.

    -- Build a ``<path>``.
    dPath = pure $
        _Wrapped # NonEmpty.fromList
          [ S._M (S._PosX # 10.0) (S._PosY # 10.0)
          , S._H (S._PosX # 90.0)
          , S._V (S._PosY # 90.0)
          , S._H (S._PosX # 10.0)
          , S._L (S._PosX # 10.0) (S._PosY # 10.0)
          ]
    -- This is the same as writing:
    --     <path d="M 10,30
    --              A 20,20 0,0,1 50,30
    --              A 20,20 0,0,1 90,30
    --              Q 90,60 50,90
    --              Q 10,60 10,30 z"/>.

    -- Build a ``<g>``.
    dGroup = pure $
      S.SVG_Group
    attrsGroup = mempty
      & at "id" ?~ "g1"
      & at "fill" ?~ "white"
      & at "stroke" ?~ "green"
      & at "stroke-width" ?~ "5"
    -- This is the same as writing: <g id="g1" fill="white" stroke="green" "stroke-width"="5">


    -- We can also build some ``Dynamic`` animation element properties:
    dAnimX = pure $
      S.SVG_Animate
        ( S.AttributeName "x" )
        ( S.AnimFrom 10 )
        ( S.AnimTo 100 )
        ( S.AnimDuration (S.Secs 10) )
        ( S.Indefinite )
    -- This is the same as having written: <animate attributeName="x" from="10" to="100" dur="10s" repeatCount="indefinite"/>
    dAnimCY = pure $
      S.SVG_Animate
        ( S.AttributeName "cy" )
        ( S.AnimFrom 200 )
        ( S.AnimTo 100 )
        ( S.AnimDuration (S.Secs 10) )
        ( S.Indefinite )
    -- This is the same as having written: <animate attributeName="cy" from="200" to="100" dur="10s" repeatCount="indefinite"/>


    -- Finally, put it all together for ``Reflex.Dom`` to add to our page.

  _ <- S.svg_ dSvgProps $ do
    _ <- S.rect_ attrsRect1 dRect1 RD.blank
    _ <- S.circle_ attrsBR (shiftCircle dCircle) RD.blank
    _ <- S.ellipse_ attrsYG dEllipse
          (S.animate_ dAnimCY)  -- animate
    _ <- S.line_ attrsRB dLine RD.blank
    _ <- S.polyLine_ attrsOrange dPolyLine RD.blank
    _ <- S.polygon_ attrsRB dPolygon RD.blank
    _ <- S.group_ attrsGroup dGroup $ do
      _ <- S.circle_ mempty
        (pure $
          S.SVG_Circle
            (S._PosCenterX # 240.0)
            (S._PosCenterY # 240.0)
            (S._Radius # 25.0))
        RD.blank
      S.circle_ mempty
        (pure $
          S.SVG_Circle
            (S._PosCenterX # 260.0)
            (S._PosCenterY # 260.0)
            (S._Radius # 25.0))
        RD.blank
    _ <- S.path_ attrsRB dPath RD.blank
    S.rect_ attrsYG dRect3
      (S.animate_ dAnimX)  -- animate

  pure ()

main :: IO ()
main = mainWidget exampleUsage
