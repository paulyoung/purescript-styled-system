module Styled.System where

import Prelude

import Color as C
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Record.Builder as Record
import Data.Variant (Variant)
import Style.Property (FontSizeValue, Property, WidthValue)
import Style.Property as Style
import Style.Property.Value (Auto, Em, Pct, Px, Zero)
import Type.Row (type (+))


type BackgroundColorState a s = ( backgroundColor :: Maybe a | s )

defaultBackgroundColorState :: forall a. { | BackgroundColorState a () }
defaultBackgroundColorState = { backgroundColor: Nothing }

backgroundColor
  :: forall a s
   . (a -> C.Color)
  -> { | BackgroundColorState a s }
  -> Array Property
backgroundColor fromTheme state = case state.backgroundColor of
  Just x -> [ Style.backgroundColor $ fromTheme x ]
  Nothing -> []


type ColorState a s =
  ( BackgroundColorState a
  + TextColorState a
  + s
  )

defaultColorState :: forall a. { | ColorState a () }
defaultColorState = Record.build builder {}
  where
  builder :: Record.Builder {} { | ColorState a () }
  builder =
    Record.merge defaultBackgroundColorState
      <<< Record.merge defaultTextColorState

color :: forall a s. (a -> C.Color) -> { | ColorState a s } -> Array Property
color = backgroundColor <> textColor


type FontSizeState a s = ( fontSize :: Maybe a | s )

defaultFontSizeState :: forall a. { | FontSizeState a () }
defaultFontSizeState = { fontSize: Nothing }

fontSize
  :: forall a s
   . (a -> FontSizeValue)
  -> { | FontSizeState a s }
  -> Array Property
fontSize fromTheme state = case state.fontSize of
  Just x -> [ Style.fontSize $ fromTheme x ]
  Nothing -> []


type SpaceValue =
  Variant
    ( Auto
    + Em
    + Pct
    + Px
    + Zero
    + ()
    )

type SpaceState a s =
  ( margin :: Maybe a
  , padding :: Maybe a
  | s
  )

-- TODO: Record.merge
defaultSpaceState :: forall a. { | SpaceState a () }
defaultSpaceState = { margin: Nothing, padding: Nothing }

-- TODO
--
-- marginX = marginLeft <> marginRight
-- marginY = marginTop <> marginBottom
-- margin = marginX <> marginY
--
-- paddingX = paddingLeft <> paddingRight
-- paddingY = paddingTop <> paddingBottom
-- padding = paddingX <> paddingY
--
-- space = margin <> padding
space
  :: forall a s
   . (a -> SpaceValue)
  -> { | SpaceState a s }
  -> Array Property
space fromTheme state = Array.catMaybes
  [ Style.margin <<< fromTheme <$> state.margin
  , Style.padding <<< fromTheme <$> state.padding
  ]


type TextColorState a s = ( color :: Maybe a | s )

defaultTextColorState :: forall a. { | TextColorState a () }
defaultTextColorState = { color: Nothing }

textColor
  :: forall a s
   . (a -> C.Color)
  -> { | TextColorState a s }
  -> Array Property
textColor fromTheme state = case state.color of
  Just x -> [ Style.color $ fromTheme x ]
  Nothing -> []


type WidthState s = ( width :: Maybe WidthValue | s )

defaultWidthState :: { | WidthState () }
defaultWidthState = { width: Nothing }

width :: forall s. { | WidthState s } -> Array Property
width state = case state.width of
  Just x -> [ Style.width x ]
  Nothing -> []
