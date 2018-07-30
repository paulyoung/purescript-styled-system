module Examples.StyledComponents.Box.Theme.Space where

import Prelude

import Style.Declaration.Value (px)
import Styled.System (SpaceValue)

data Space
  = Zero
  | Four
  | Eight
  | Sixteen
  | ThirtyTwo

derive instance eqSpace :: Eq Space

space0 = Zero :: Space
space4 = Four :: Space
space8 = Eight :: Space
space16 = Sixteen :: Space
space32 = ThirtyTwo :: Space

toValue :: Space -> SpaceValue
toValue = case _ of
  Zero -> 0.0 # px
  Four -> 4.0 # px
  Eight -> 8.0 # px
  Sixteen -> 16.0 # px
  ThirtyTwo -> 32.0 # px
