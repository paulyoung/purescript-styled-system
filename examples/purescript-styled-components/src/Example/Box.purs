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
import Styled.Components (element) as Styled
import Styled.Components.Constructors (css)
import Styled.Components.Effect (StyledM)
import Styled.Components.Types (Element, Element_, ID) as Styled
import Styled.System (ColorState, FontSizeState, SpaceState, WidthState)
import Styled.System as System
import Type.Row (type (+))

type StateFields r =
  ( ColorState Theme.Color
  + FontSizeState Theme.FontSize
  + SpaceState Theme.Space
  + WidthState
  + r
  )

type State = { | StateFields () }

defaultState :: State
defaultState = Record.build builder {}

  where

  builder :: Record.Builder {} State
  builder =
    Record.merge System.defaultColorState
      <<< Record.merge System.defaultFontSizeState
      <<< Record.merge System.defaultSpaceState
      <<< Record.merge System.defaultWidthState

box
  :: forall p i
   . Styled.ID -- make id field part of State?
  -> (State -> State)
  -> StyledM (Styled.Element _ p i)
box id mkArgs = el id state

  where

  state :: State
  state = mkArgs defaultState -- TODO: compiler-solved Lacks/Nub instead

  el = Styled.element HH.div $
    [ css \s ->
       System.color Color.toValue s -- s or state? is a function unnecessary?
         <> System.fontSize FontSize.toValue s
         <> System.space Space.toValue s
         <> System.width s
    ]

box_
  :: forall p i
   . Styled.ID -- TODO: make id field part of State
  -> (State -> State)
  -> StyledM (Styled.Element_ p i)
box_ id mkArgs = box id mkArgs <@> []
