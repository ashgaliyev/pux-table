module App.Components.Table where

import Prelude (($), show, (<$>), class Show, const, Ordering(..), (==), map, (<>), const)
import Pux.Html (Html, ul, li, text, button, div, table, tr, td, th)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
import Data.Ord (class Ord, compare)
import Data.Eq (class Eq, eq)
import Data.Array (sort, reverse, sortBy, filter, elem)
import Data.Maybe (Maybe(..))

data Sort = ASC | DESC

derive instance eqSort :: Eq Sort

data Action =
    SortAsc String
  | SortDesc String

type Field = String
type Heading = String

data SortColumn = SortColumn Field Sort

data ViewColumn = ViewColumn Field Heading

type State = {
    sortCol :: SortColumn
  , columns :: Array ViewColumn
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
renderRow cols (Row xs)  = tr [] (renderCell <$> cells)
  where
    cells = filter (\x -> elem (getName x) cols) xs

renderTable :: Table -> Array (Html Action) -> Array String -> Html Action
renderTable (Table xs) headings cols = table [] (headings <> (renderRow cols <$> xs))

renderHeadings :: State -> Array (Html Action)
renderHeadings { sortCol : _sortCol, columns : _cols } =
  [ tr [] (renderHeading _sortCol <$> _cols) ]
  where
    renderHeading :: SortColumn -> ViewColumn -> Html Action
    renderHeading (SortColumn n1 s) (ViewColumn n2 title) =
      if n1 == n2 then
        case s of
          ASC -> th [ onClick ( const $ SortDesc n1) ] [ text $ title <> "↑" ]
          _   -> th [ onClick ( const $ SortAsc n1) ] [ text $ title <> "↓" ]
      else th [ onClick ( const $ SortAsc n2) ] [ text title ]



update :: Action -> State -> State
update (SortAsc str) st = st { sortCol = SortColumn str ASC }
update (SortDesc str) st = st { sortCol = SortColumn str DESC }
update _ st = st


view :: Table -> State -> Html Action
view t st@{ sortCol : _sortCol, columns : _cols } =
    div [] [
      renderTable sorted (renderHeadings st) (getFields _cols)
    ]
  where
    sorted = sortTable _sortCol t
    getFields :: Array ViewColumn -> Array String
    getFields xs = map (\(ViewColumn f h) -> f) xs
