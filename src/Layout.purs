module App.Layout where

import App.ListTest as ListTest
import App.TableTest as TableTest
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text)

data Action
  = ChildList (ListTest.Action)
  | ChildTable (TableTest.Action)
  | PageView Route

type State =
  { route :: Route
  , list :: ListTest.State
  , table :: TableTest.State }

init :: State
init =
  { route: NotFound
  , list: ListTest.init
  , table : TableTest.init }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (ChildList action) state = state { list = ListTest.update action state.list }
update (ChildTable action) state = state { table = TableTest.update action state.table }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        --Home -> map ChildList $ ListTest.view state.list
        Home -> map ChildTable $ TableTest.view state.table
        NotFound -> NotFound.view state
    ]
