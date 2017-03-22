module App.ListTest where

import Prelude ((+), (-), const, show, (<$>), map, ($), class Show)
import Pux.Html (Html, div, span, button, text)
import Pux.Html.Events (onClick)
import Data.Ord (class Ord, compare)
import Data.Eq (class Eq, eq)

import App.Components.SortableList as SL

data Action = NoOp | Child (SL.Action)

newtype Num = Num Int

instance eqNum :: Eq Num where
  eq (Num a) (Num b) = eq a b

instance ordNum :: Ord Num where
  compare (Num a) (Num b) = compare a b

instance showNum :: Show Num where
  show (Num i) = show i

type State = {
    list :: Array Num
  , tableState :: SL.State
}

init :: State
init = {
    list : [
      Num 1, Num 2, Num 3
    ]
  , tableState : SL.init
}


update :: Action -> State -> State
update (Child a) st = st { tableState = SL.update a st.tableState }
update _ st = st

view :: State -> Html Action
view st = div [] [
    map Child $ SL.view st.list st.tableState
  ]
