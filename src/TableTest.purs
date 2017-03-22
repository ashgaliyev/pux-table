module App.TableTest where

import Prelude ((+), (-), const, show, (<$>), map, ($))
import Pux.Html (Html, div, span, button, text)
import Pux.Html.Events (onClick)

import App.Components.Table as Table

data Action = NoOp | Child (Table.Action)

type State = {}

update :: Action -> State -> State
update _ st = st

init :: State
init = {}

view :: State -> Html Action
view st = div [] [
  map Child $ Table.view Table.sampleTable Table.init
]
