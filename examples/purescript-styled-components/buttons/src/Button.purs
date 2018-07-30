module Examples.StyledComponents.Buttons.Button where

import Prelude hiding (zero)

import Color (rgb, rgba, white)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, under, unwrap)
import Examples.StyledComponents.Buttons.Theme.Color (Color) as Theme
import Examples.StyledComponents.Buttons.Theme.Color as Color
import Examples.StyledComponents.Buttons.Theme.FontSize (FontSize) as Theme
import Examples.StyledComponents.Buttons.Theme.FontSize as FontSize
import Examples.StyledComponents.Buttons.Theme.Space (Space) as Theme
import Examples.StyledComponents.Buttons.Theme.Space as Space
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record.Builder as Record
import Style.Declaration as CSS
import Style.Declaration.Value (bold, boxShadow_, center, none, px, transparent, zero)
import Styled.Components (element, id, modifyOver_) as Styled
import Styled.Components.Constructors (Constructors, active, css, disabled, focus, hover)
import Styled.Components.Effect (StyledM, deleteCSS)
import Styled.Components.Types (Element, ID(..)) as Styled
import Styled.System (ColorState, FontSizeState, SpaceState)
import Styled.System as System
import Type.Row (type (+))

type SystemFields r =
  ( ColorState Theme.Color
  + FontSizeState Theme.FontSize
  + SpaceState Theme.Space
  + r
  )

type StateRep' r =
  { css :: Array (Constructors State) -- keep? limit to system values?
  , html :: H.ComponentHTML Query
  , id :: Styled.ID
  , isOn :: Boolean
  | r
  }

type StateRep = StateRep' (SystemFields ())

newtype State = State StateRep

derive instance newtypeState :: Newtype State _

defaultState :: State
defaultState = State $ Record.build builder
  { css: []
  , html: HH.text ""
  , id: Styled.ID ""
  , isOn: false
  }

  where

  builder :: Record.Builder (StateRep' ()) StateRep
  builder =
    Record.merge System.defaultColorState
      <<< Record.merge System.defaultFontSizeState
      <<< Record.merge System.defaultSpaceState

data Query a
  = Initialize a
  | Finalize a
  | Toggle a

type Input =
  { css :: Array (Constructors State)
  }

data Message
  = Initialized
  | Finalized
  | Toggled Boolean

buttonEl
  :: forall p i
   . (State -> State) -- TODO: compiler-solved Lacks/Nub
  -> StyledM (Styled.Element _ p i)
buttonEl mkState = el stateRep.id stateRep

  where

  state :: State
  state = mkState defaultState -- TODO: compiler-solved Lacks/Nub

  stateRep :: StateRep
  stateRep = unwrap state

  el
    :: Styled.ID
    -> StateRep
    -> StyledM (Styled.Element _ p i)
  el = Styled.element HH.button $
    [ css $
        System.backgroundColor Color.toValue
          -- <> System.border
          -- <> System.borderRadius Radii.toValue
          <> System.color Color.toValue
          <> System.fontSize FontSize.toValue
          -- <> System.fontWeight FontWeight.toValue
          <> System.space Space.toValue
          -- <> System.textAlign TextAlign.toValue
    -- [ css \_ ->
    --     [ CSS.backgroundColor $ rgb 0 103 238
    --     , CSS.border' zero none transparent
    --     , CSS.borderRadius (4.0 # px) (4.0 # px) (4.0 # px) (4.0 # px)
    --     , CSS.color white
    --     , CSS.fontSize $ 14.0 # px
    --     , CSS.fontWeight bold
    --     , CSS.padding (8.0 # px) (16.0 # px) (8.0 # px) (16.0 # px)
    --     , CSS.textAlign center
    --     -- FIXME: sometimes values from `s.css` appear first instead of last.
    --     -- This demonstrates the issue because the `Example` module provides
    --     -- `marginLeft` via `s.css`.
    --     -- , CSS.marginLeft $ 16.0 # px
    --     ]
    -- , hover \_ ->
    --     [ CSS.boxShadow
    --         [ boxShadow_ true zero zero zero (999.0 # px) (rgba 0 0 0 0.125)
    --         ]
    --     ]
    -- , focus \_ ->
    --     [ CSS.boxShadow
    --         [ boxShadow_ false zero zero zero (2.0 # px) (rgb 0 103 238)
    --         ]
    --     , CSS.outline' zero none transparent
    --     ]
    -- , active \_ ->
    --     [ CSS.boxShadow
    --         [ boxShadow_ true zero zero (8.0 # px) zero (rgba 0 0 0 0.25)
    --         ]
    --     ]
    -- , disabled \_ ->
    --     [
    --     ]
    ]
    -- <> s.css

button :: H.Component HH.HTML Query Input Message StyledM
button =
  H.lifecycleComponent
    { initialState
    , render: _.html <<< unwrap
    , eval
    , initializer: Just $ H.action Initialize
    , finalizer: Just $ H.action Finalize
    , receiver: const Nothing
    }

  where

  initialState :: Input -> State
  initialState input = over State _ { css = input.css } defaultState

  render :: State -> StyledM (H.ComponentHTML Query)
  render s@(State state) = do
    let label = if state.isOn then "On" else "Off"
    button' <- buttonEl $ const s
    pure $
      button'
        [ HP.title label
        , HE.onClick (HE.input_ Toggle)
        ]
        [ HH.text label ]

  -- TODO: Initalize/Finalize is tedious to do manually.
  -- Transparent HOCs introduced in https://github.com/slamdata/purescript-halogen/issues/526
  -- will help.
  eval :: Query ~> H.ComponentDSL State Query Message StyledM
  eval = case _ of
    Initialize next -> do
      id <- H.lift $ Styled.id
      Styled.modifyOver_ State render _ { id = id }
      H.raise Initialized -- With HOC, should we pass CSS up here instead of appendCSS/StyledM?
      pure next
    Finalize next -> do
      id <- H.gets $ _.id <<< unwrap
      H.lift $ deleteCSS id
      H.raise Finalized -- With HOC, should we pass id here instead of deleteCSS?
      pure next
    Toggle next -> do
      (State state) <- H.get
      let isOn = not state.isOn
      Styled.modifyOver_ State render _ { isOn = isOn }
      H.raise $ Toggled isOn
      pure next
