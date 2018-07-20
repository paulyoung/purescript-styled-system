module Example.Box where

import Prelude

import Example.Theme.Color (Color) as Theme
import Example.Theme.Color as Color
import Example.Theme.FontSize (FontSize) as Theme
import Example.Theme.FontSize as FontSize
import Example.Theme.Space (Space) as Theme
import Example.Theme.Space as Space
import Halogen.HTML as HH
import Record.Builder as Record
import Styled.Components as Styled
import Styled.System (ColorState, FontSizeState, SpaceState, WidthState)
import Styled.System as System
import Type.Row (type (+))

type State s =
  ( ColorState Theme.Color
  + FontSizeState Theme.FontSize
  + SpaceState Theme.Space
  + WidthState
  + s
  )

defaultState :: { | State () }
defaultState = Record.build builder {}
  where
  builder :: Record.Builder {} { | State ()  }
  builder =
    Record.merge System.defaultColorState
      <<< Record.merge System.defaultFontSizeState
      <<< Record.merge System.defaultSpaceState
      <<< Record.merge System.defaultWidthState

box
  :: forall p i
   . ({ | State () } -> { | State () })
  -> Array (HH.IProp _ i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
box mkArgs = mkArgs defaultState #
  Styled.element HH.div
    [ System.color Color.toValue
    , System.fontSize FontSize.toValue
    , System.space Space.toValue
    , System.width
    ]

box_
  :: forall p i
   . ({ | State ()  } -> { | State ()  })
  -> Array (HH.HTML p i)
  -> HH.HTML p i
box_ mkArgs = box mkArgs []
