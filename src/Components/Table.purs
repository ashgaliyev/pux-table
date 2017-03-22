module App.Components.Table where

import Prelude (($), show, (<$>), class Show, const, Ordering(..), (==))
import Pux.Html (Html, ul, li, text, button, div, table, tr, td)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
import Data.Ord (class Ord, compare)
import Data.Eq (class Eq, eq)
import Data.Array (sort, reverse, sortBy, filter, elem)
import Data.Maybe (Maybe(..))

data Sort = ASC | DESC

derive instance eqSort :: Eq Sort

data Action = NoOp

data SortColumn = SortColumn String Sort

type State = {
    sortCol :: SortColumn
  , columns :: Array String
}

init :: State
init = {
      sortCol : SortColumn "age" DESC
    , columns : ["name", "age"]
  }

newtype Name = Name String

data Cell =
    CellString Name String
  | CellInt Name Int

class WithName a where
  getName :: a -> String

instance cellWithName :: WithName Cell where
  getName (CellString (Name a) _) = a
  getName (CellInt (Name a) _) = a

instance cellEq :: Eq Cell where
  eq (CellString _ a) (CellString _ b) = eq a b
  eq (CellInt _ a) (CellInt _ b) = eq a b
  eq _ _ = false

instance cellOrd :: Ord Cell where
  compare (CellString _ a) (CellString _ b) = compare a b
  compare (CellInt _ a) (CellInt _ b) = compare a b
  compare _ _ = EQ

data Row = Row (Array Cell)

instance eqRow :: Eq Row where
  eq (Row xs) (Row ys) = eq xs ys

instance ordRow :: Ord Row where
  compare (Row xs) (Row ys) = compare xs ys

data Table = Table (Array Row)

sortTable :: SortColumn -> Table -> Table
sortTable (SortColumn col dir) (Table rows) =
  if dir == ASC
  then Table sorted
  else Table $ reverse sorted
    where
      sorted = sortBy (\(Row x) (Row y) -> compareMy col x y) rows
      compareMy name xs ys = compareRows (Row (filter (\x -> getName x == name ) xs)) (Row (filter (\x -> getName x == name ) ys))
      compareRows (Row a) (Row b) = compare a b


sampleTable :: Table
sampleTable = Table [
      Row [
         CellString (Name "name") "anna"
       , CellInt (Name "age") 24
      ]
    , Row [
        CellString (Name "name") "boris"
      , CellInt (Name "age") 32
    ]
]

renderCell :: Cell -> Html Action
renderCell (CellString name str) = td [] [ text str ]
renderCell (CellInt name n) = td [] [ text $ show n ]

renderRow :: Array String -> Row -> Html Action
renderRow cols (Row xs)  = tr [] (renderCell <$> ( filter ( \x -> elem (getName x) cols) xs))

renderTable :: Table -> Array String -> Html Action
renderTable (Table xs) cols = table [] (renderRow cols <$> xs)

view :: Table -> State -> Html Action
view t { sortCol : _sortCol, columns : _cols } =
    div [] [
      renderTable sorted _cols
    ]
  where
    sorted = sortTable _sortCol t
