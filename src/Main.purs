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

type State = { enabled :: Boolean }

data Action = Toggle

component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: initialState
  , render: render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

initialState :: forall i. i -> State
initialState _ = { enabled: false }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let label = if state.enabled then "On" else "Off"
  in HH.button [ HP.title label, HE.onClick \_ -> Toggle] [HH.text label]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle -> H.modify_ \st -> st { enabled = not st.enabled }
