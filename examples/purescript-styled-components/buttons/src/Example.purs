module Examples.StyledComponents.Buttons.Example where

import Prelude

import Data.Int (decimal)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Examples.StyledComponents.Buttons.Button (button)
import Examples.StyledComponents.Buttons.Button as Button
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Style.Declaration as CSS
import Style.Declaration.Value (px)
import Styled.Components (css, id, modify_) as Styled
import Styled.Components.Constructors (css)
import Styled.Components.Effect (StyledM, deleteCSS)
import Styled.Components.Types (ID(..)) as Styled

type State =
  { html :: H.ParentHTML Query Button.Query ChildSlot StyledM
  , id :: Styled.ID
  , toggleCount :: Int
  }

data Query a
  = Initialize a
  | Finalize a
  | HandleButtonMessage Button.Message a

type Input = Unit

type Message = Void

type ChildSlot = Int

example :: H.Component HH.HTML Query Input Message StyledM
example =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render: _.html
    , eval
    , initializer: Just $ H.action Initialize
    , finalizer: Just $ H.action Finalize
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { html: HH.text ""
    , id: Styled.ID ""
    , toggleCount: 0
    }

  render :: State -> StyledM (H.ParentHTML Query Button.Query ChildSlot StyledM)
  render state = do
    let
      buttonCSS =
        [ css \_ ->
            [ CSS.marginLeft $ 8.0 # px
            ]
        ]

      button0 = HH.slot 0 button { css: [] } $ HE.input HandleButtonMessage
      button1 = HH.slot 1 button { css: buttonCSS } $ HE.input HandleButtonMessage
      button2 = HH.slot 2 button { css: buttonCSS } $ HE.input HandleButtonMessage

    styleTag <- Styled.css

    pure $
      HH.div_
        [ styleTag
        , button0
        , button1
        , button2
        , HH.p_
            [ HH.text ("Buttons have been toggled " <> Int.toStringAs decimal state.toggleCount <> " time(s)") ]
        ]

  eval :: Query ~> H.ParentDSL State Query Button.Query ChildSlot Message StyledM
  eval = case _ of
    Initialize next -> do
      id <- H.lift Styled.id
      Styled.modify_ render _ { id = id }
      pure next
    Finalize next -> do
      id <- H.gets _.id
      H.lift $ deleteCSS id
      pure next
    HandleButtonMessage Button.Initialized next -> do
      Styled.modify_ render identity
      pure next
    HandleButtonMessage Button.Finalized next -> do
      Styled.modify_ render identity
      pure next
    HandleButtonMessage (Button.Toggled _) next -> do
      Styled.modify_ render \state ->
        state { toggleCount = state.toggleCount + 1 }
      pure next
