module App.Components.SortableList where

import Prelude (($), show, (<$>), class Show, const)
import Pux.Html (Html, ul, li, text, button, div)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
import Data.Ord (class Ord)
import Data.Array (sort, reverse)

import Data.Array as A

data Action = SetAsc | SetDesc

data Sort = ASC | DESC

type State = {
    sort :: Sort
  }

init :: State
init = { sort : DESC }

update :: Action -> State -> State
update SetAsc st = st { sort = ASC }
update SetDesc st = st { sort = DESC }

renderButton :: Sort -> Html Action
renderButton ASC = button [ className "btn btn-primary", onClick $ const SetDesc ] [ text "DESC" ]
renderButton DESC = button [ className "btn btn-primary", onClick $ const SetAsc ] [ text "ASC" ]

view :: forall a. (Ord a, Show a) => Array a -> State -> Html Action
view _list { sort : _sort } =
  div [] [
      renderButton _sort
    , ul [] (renderItem <$> (applySort _sort _list))
  ]

  where
    renderItem x = li [] [ text $ show x ]

applySort :: forall a. (Ord a) => Sort -> Array a -> Array a
applySort ASC l = A.sort l
applySort DESC l = A.reverse $ A.sort l
