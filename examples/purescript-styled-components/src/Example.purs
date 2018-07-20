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

type State = Unit

data Query a = NoOp a

type Input = Unit

type Message = Void

type Slot = Unit

example :: forall m. H.Component HH.HTML Query Input Message m
example =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = unit

  render :: State -> H.ComponentHTML Query
  render _ =
    HH.div_
      [ box_ _ { width = Just $ 50.0 # pct } $ []
      , box_ _ { fontSize = Just Theme.fontSize20 } $ []
      , box_ _ { margin = Just Theme.space16 } $ []
      , box_ _ { padding = Just Theme.space32 } $ []
      , box_ _ { color = Just $ Theme.colorException tomato } $ []
      , box_ _ { color = Just Theme.colorGray0 } $ []
      , box_ _ { backgroundColor = Just $ Theme.colorException tomato } $ []
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (NoOp next) = pure next
