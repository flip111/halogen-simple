module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State =
  { enabled :: Boolean
  }

initialState :: forall i. i -> State
initialState _ = { enabled: false }

component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: initialState
  , render: render
  , eval: H.mkEval $ H.defaultEval {handleAction = handleAction}
  }

data Action = Toggle

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle -> H.modify_ \st -> st {enabled = not st.enabled}

render :: forall m. State -> H.ComponentHTML Action () m
render s =
  let label = if s.enabled then "On" else "Off"
  in HH.button [ HP.title label, HE.onClick \_ -> Toggle] [HH.text label]
