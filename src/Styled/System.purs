module Styled.System where

import Prelude

import Color as C
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, expand)
import Record.Builder as Record
import Style.Declaration (Declaration, TRBL)
import Style.Declaration as Style
import Style.Declaration.Value as V
import Type.Row (type (+))


type BackgroundColorState a s = ( backgroundColor :: Maybe a | s )

defaultBackgroundColorState :: forall a. { | BackgroundColorState a () }
defaultBackgroundColorState = { backgroundColor: Nothing }

backgroundColor
  :: forall a s
   . (a -> C.Color)
  -> { | BackgroundColorState a s }
  -> Array Declaration
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

color
  :: forall a s
   . (a -> C.Color)
  -> { | ColorState a s }
  -> Array Declaration
color = backgroundColor <> textColor


type FontSizeState a s = ( fontSize :: Maybe a | s )

defaultFontSizeState :: forall a. { | FontSizeState a () }
defaultFontSizeState = { fontSize: Nothing }

fontSize
  :: forall a s
   . (a -> V.FontSizeValue)
  -> { | FontSizeState a s }
  -> Array Declaration
fontSize fromTheme state = case state.fontSize of
  Just x -> [ Style.fontSize $ fromTheme x ]
  Nothing -> []


type SpaceValue =
  Variant
    ( V.GlobalFields
    + V.LengthFields
    + V.Pct
    + V.Zero
    + ()
    )

type SpaceState a s =
  ( margin :: Maybe a
  , marginTop :: Maybe a
  , marginRight :: Maybe a
  , marginBottom :: Maybe a
  , marginLeft :: Maybe a
  , padding :: Maybe a
  , paddingTop :: Maybe a
  , paddingRight :: Maybe a
  , paddingBottom :: Maybe a
  , paddingLeft :: Maybe a
  | s
  )

-- TODO: defaults with compiler-solved Nub/Lacks
defaultSpaceState :: forall a. { | SpaceState a () }
defaultSpaceState =
  { margin: Nothing
  , marginBottom: Nothing
  , marginLeft: Nothing
  , marginRight: Nothing
  , marginTop: Nothing
  , padding: Nothing
  , paddingBottom: Nothing
  , paddingLeft: Nothing
  , paddingRight: Nothing
  , paddingTop: Nothing
  }


marginBottom
  :: forall a s
   . (a -> V.MarginValue)
  -> { | SpaceState a s }
  -> Array Declaration
marginBottom fromTheme state = case state.marginBottom of
  Just x -> [ Style.marginBottom $ fromTheme x ]
  Nothing -> []

marginLeft
  :: forall a s
   . (a -> V.MarginValue)
  -> { | SpaceState a s }
  -> Array Declaration
marginLeft fromTheme state = case state.marginLeft of
  Just x -> [ Style.marginLeft $ fromTheme x ]
  Nothing -> []

marginRight
  :: forall a s
   . (a -> V.MarginValue)
  -> { | SpaceState a s }
  -> Array Declaration
marginRight fromTheme state = case state.marginRight of
  Just x -> [ Style.marginRight $ fromTheme x ]
  Nothing -> []

marginTop
  :: forall a s
   . (a -> V.MarginValue)
  -> { | SpaceState a s }
  -> Array Declaration
marginTop fromTheme state = case state.marginTop of
  Just x -> [ Style.marginTop $ fromTheme x ]
  Nothing -> []

marginX
  :: forall a s
   . (a -> V.MarginValue)
  -> { | SpaceState a s }
  -> Array Declaration
marginX = marginLeft <> marginRight

marginY
  :: forall a s
   . (a -> V.MarginValue)
  -> { | SpaceState a s }
  -> Array Declaration
marginY = marginTop <> marginBottom

margin
  :: forall a s
   . (a -> TRBL V.MarginValue)
  -> { | SpaceState a s }
  -> Array Declaration
margin fromTheme state = case state.margin of
  Just x ->
    let { top, right, bottom, left } = fromTheme x
    in [ Style.margin top right bottom left ]
  Nothing -> []


paddingBottom
  :: forall a s
   . (a -> V.PaddingValue)
  -> { | SpaceState a s }
  -> Array Declaration
paddingBottom fromTheme state = case state.paddingBottom of
  Just x -> [ Style.paddingBottom $ fromTheme x ]
  Nothing -> []

paddingLeft
  :: forall a s
   . (a -> V.PaddingValue)
  -> { | SpaceState a s }
  -> Array Declaration
paddingLeft fromTheme state = case state.paddingLeft of
  Just x -> [ Style.paddingLeft $ fromTheme x ]
  Nothing -> []

paddingRight
  :: forall a s
   . (a -> V.PaddingValue)
  -> { | SpaceState a s }
  -> Array Declaration
paddingRight fromTheme state = case state.paddingRight of
  Just x -> [ Style.paddingRight $ fromTheme x ]
  Nothing -> []

paddingTop
  :: forall a s
   . (a -> V.PaddingValue)
  -> { | SpaceState a s }
  -> Array Declaration
paddingTop fromTheme state = case state.paddingTop of
  Just x -> [ Style.paddingTop $ fromTheme x ]
  Nothing -> []

paddingX
  :: forall a s
   . (a -> V.PaddingValue)
  -> { | SpaceState a s }
  -> Array Declaration
paddingX = paddingLeft <> paddingRight

paddingY
  :: forall a s
   . (a -> V.PaddingValue)
  -> { | SpaceState a s }
  -> Array Declaration
paddingY = paddingTop <> paddingBottom

padding
  :: forall a s
   . (a -> TRBL V.PaddingValue)
  -> { | SpaceState a s }
  -> Array Declaration
padding fromTheme state = case state.padding of
  Just x ->
    let { top, right, bottom, left } = fromTheme x
    in [ Style.padding top right bottom left ]
  Nothing -> []


space
  :: forall a s
   . (a -> TRBL SpaceValue)
  -> { | SpaceState a s }
  -> Array Declaration
space fromTheme = margin (fromSpaceValue <<< fromTheme) <> padding fromTheme

  where

  fromSpaceValue :: TRBL SpaceValue -> TRBL V.MarginValue
  fromSpaceValue { top, right, bottom, left } =
    { top: expand top
    , right: expand right
    , bottom: expand bottom
    , left: expand left
    }


type TextColorState a s = ( color :: Maybe a | s )

defaultTextColorState :: forall a. { | TextColorState a () }
defaultTextColorState = { color: Nothing }

textColor
  :: forall a s
   . (a -> C.Color)
  -> { | TextColorState a s }
  -> Array Declaration
textColor fromTheme state = case state.color of
  Just x -> [ Style.color $ fromTheme x ]
  Nothing -> []


type WidthState s = ( width :: Maybe V.WidthValue | s )

defaultWidthState :: { | WidthState () }
defaultWidthState = { width: Nothing }

width
  :: forall s
   . { | WidthState s }
  -> Array Declaration
width state = case state.width of
  Just x -> [ Style.width x ]
  Nothing -> []
