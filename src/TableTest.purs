module App.TableTest where

import Prelude ((+), (-), const, show, (<$>), map, ($), (<>))
import Pux.Html (Html, div, span, button, text, h1, input)
import Pux.Html.Events (onClick, onInput, FormEvent)
import Pux.Html.Attributes (placeholder)
import Data.Array (sort, reverse, sortBy, filter, elem, length)

import App.Components.Table as Table
import Data.String as S

data Action =
    Child (Table.Action)
  | SetQuery FormEvent

type State = {
    table :: Table.State
  , people :: Array Person
  , query :: String
}

init :: State
init = {
      table : {
           columns : [
                Table.Column "name" "Name" Table.ASC
              , Table.Column "year" "Year" Table.NoSort
              , Table.Column "city" "City" Table.NoSort
          ]
    }
    , people : presidents
    , query : ""
  }

update :: Action -> State -> State
update (SetQuery ev) st = st { query = ev.target.value }
update (Child a) st = st { table = Table.update a st.table }
update _ st = st

view :: State -> Html Action
view st =
  let
    lowerQuery =
      S.toLower st.query

    acceptablePeople =
      filter (\(Person { name : _name }) -> S.contains (S.Pattern lowerQuery) (S.toLower _name)) st.people
  in
    div [] [
        h1 [] [ text "Birthplaces of some abstract persons" ]
      , input [ placeholder "Search by Name", onInput SetQuery ] []
      , span [] [ text $ "found " <> (show $ length acceptablePeople) ]
      , map Child $ Table.view (toTable acceptablePeople) st.table
    ]


data Person = Person
  { name :: String
  , year :: Int
  , city :: String
  }

presidents :: Array Person
presidents =
  [ Person { name : "A" , year: 1,  city : "X" }
  , Person { name : "A" , year: 1,  city : "Y" }
  , Person { name : "A" , year: 2,  city : "Z" }
  , Person { name : "B" , year: 1,  city : "X" }
  , Person { name : "B" , year: 2,  city : "Y" }
  , Person { name : "C" , year: 1,  city : "X" }

  ]

toTable :: Array Person -> Table.Table
toTable xs = Table.Table ( toRow <$> xs )
  where
    toRow (Person {name: _name, year: _year, city: _city }) =
      Table.Row [
          (Table.CellString (Table.Name "name") _name)
        , (Table.CellInt (Table.Name "year") _year)
        , (Table.CellString (Table.Name "city") _city)
      ]
