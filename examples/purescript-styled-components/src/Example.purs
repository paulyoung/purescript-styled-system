module Example where

import Prelude

import Color.Scheme.X11 (tomato)
import Data.Maybe (Maybe(..))
import Example.Box (box_)
import Example.Theme.Color (colorException, colorGray0) as Theme
import Example.Theme.FontSize (fontSize20) as Theme
import Example.Theme.Space (space16, space32) as Theme
import Halogen as H
import Halogen.HTML as HH
import Style.Declaration.Value (pct)
import Styled.Components (css, id, modify_) as Styled
import Styled.Components.Effect (StyledM, deleteCSS)
import Styled.Components.Types (ID(..)) as Styled

type State =
  { html :: H.ComponentHTML Query
  , id :: Styled.ID
  }

data Query a
  = Initialize a
  | Finalize a

type Input = Unit

type Message = Void

type Slot = Unit

example :: H.Component HH.HTML Query Input Message StyledM
example =
  H.lifecycleComponent
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
    }

  render :: State -> StyledM (H.ComponentHTML Query)
  render state = do
    box1 <- box_ state.id _ { width = Just $ 50.0 # pct }
    box2 <- box_ state.id _ { fontSize = Just Theme.fontSize20 }
    box3 <- box_ state.id _ { margin = Just
                                { top: Theme.space16
                                , right: Theme.space16
                                , bottom: Theme.space16
                                , left: Theme.space16
                                }
                            }
    box4 <- box_ state.id _ { padding = Just
                                { top: Theme.space32
                                , right: Theme.space32
                                , bottom: Theme.space32
                                , left: Theme.space32
                                }
                            }
    box5 <- box_ state.id _ { color = Just $ Theme.colorException tomato }
    box6 <- box_ state.id _ { color = Just Theme.colorGray0 }
    box7 <- box_ state.id _ { backgroundColor = Just $ Theme.colorException tomato }

    styleTag <- Styled.css

    pure $
      HH.div_
        [ styleTag
        , box1 []
        , box2 []
        , box3 []
        , box4 []
        , box5 []
        , box6 []
        , box7 []
        ]

  eval :: Query ~> H.ComponentDSL State Query Message StyledM
  eval = case _ of
    Initialize next -> do
      id <- H.lift Styled.id
      Styled.modify_ render _ { id = id }
      pure next
    Finalize next -> do
      id <- H.gets _.id
      H.lift $ deleteCSS id
      pure next
