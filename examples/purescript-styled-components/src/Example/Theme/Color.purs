module Example.Theme.Color where

import Prelude

import Color (black, white)
import Color as C
import Color.Scheme.HTML (gray)

data Color
  = Exception C.Color
  | Black
  | White
  | Gray0

derive instance eqColor :: Eq Color

toValue :: Color -> C.Color
toValue = case _ of
  Exception color -> color
  Black -> black
  White -> white
  Gray0 -> gray
