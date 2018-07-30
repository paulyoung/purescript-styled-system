module Examples.StyledComponents.Buttons.Main where

import Prelude

import Data.Map as Map
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (new)
import Examples.StyledComponents.Buttons.Example (example)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Styled.Components.Effect (runStyledM)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  css <- liftEffect $ new Map.empty
  let env = { css }
  let example' = H.hoist (runStyledM env) example
  runUI example' unit body
