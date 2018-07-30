module Examples.StyledComponents.Box.Box where

import Prelude

import Examples.StyledComponents.Box.Theme.Color (Color) as Theme
import Examples.StyledComponents.Box.Theme.Color as Color
import Examples.StyledComponents.Box.Theme.FontSize (FontSize) as Theme
import Examples.StyledComponents.Box.Theme.FontSize as FontSize
import Examples.StyledComponents.Box.Theme.Space (Space) as Theme
import Examples.StyledComponents.Box.Theme.Space as Space
import Halogen.HTML as HH
import Record.Builder as Record
import Styled.Components (element) as Styled
import Styled.Components.Constructors (css)
import Styled.Components.Effect (StyledM)
import Styled.Components.Types (Element, Element_, ID(..)) as Styled
import Styled.System (ColorState, FontSizeState, SpaceState, WidthState)
import Styled.System as System
import Type.Row (type (+))

type SystemFields r =
  ( ColorState Theme.Color
  + FontSizeState Theme.FontSize
  + SpaceState Theme.Space
  + WidthState
  + r
  )

type State =
  { id :: Styled.ID
  | SystemFields ()
  }

defaultState :: State
defaultState = Record.build builder { id: Styled.ID "" }

  where

  builder :: Record.Builder { id :: Styled.ID } State
  builder =
    Record.merge System.defaultColorState
      <<< Record.merge System.defaultFontSizeState
      <<< Record.merge System.defaultSpaceState
      <<< Record.merge System.defaultWidthState

box
  :: forall p i
   . (State -> State)
  -> StyledM (Styled.Element _ p i)
box mkState = el state.id state

  where

  state :: State
  state = mkState defaultState -- TODO: compiler-solved Lacks/Nub instead

  el :: _
  el = Styled.element HH.div $
    [ css $
        System.color Color.toValue
          <> System.fontSize FontSize.toValue
          <> System.space Space.toValue
          <> System.width
    ]

box_
  :: forall p i
   . (State -> State)
  -> StyledM (Styled.Element_ p i)
box_ mkArgs = box mkArgs <@> []
