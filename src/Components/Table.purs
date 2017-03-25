module App.Components.Table where

import Prelude (($), show, (<$>), class Show, const, Ordering(..), (==), (/=), map, (<>), const)
import Pux.Html (Html, ul, li, text, button, div, table, tr, td, th)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
import Data.Ord (class Ord, compare)
import Data.Eq (class Eq, eq)
import Data.Array (sort, reverse, sortBy, filter, elem, (:), groupBy, concatMap)
import Data.Maybe (Maybe(..))
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)
import Data.NonEmpty (fromNonEmpty)

data Sort = NoSort | ASC | DESC

derive instance eqSort :: Eq Sort

data Action =
    SortAsc String
  | SortDesc String

type Field = String
type Heading = String

data Column = Column Field Heading Sort

type State = {
  columns :: Array Column
}

newtype Name = Name String

data Cell =
    CellString Name String
  | CellInt Name Int


getName :: Cell -> String
getName (CellString (Name n) _) = n
getName (CellInt (Name n) _) = n

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

sortTable :: Array Column -> Table -> Table
sortTable xs (Table rows) = Table (sortRows' (filter (\(Column _ _ s) -> s /= NoSort) xs) rows)

compareMy :: Field -> Row -> Row -> Ordering
compareMy name (Row xs) (Row ys) = compareRows (Row (filter (\x -> getName x == name ) xs)) (Row (filter (\x -> getName x == name ) ys))

compareRows :: Row -> Row -> Ordering
compareRows (Row a) (Row b) = compare a b


-- sort rows by multiple columns
sortRows' :: Array Column -> Array Row -> Array Row
sortRows' [] rows = rows
sortRows' [col] rows = sortRows col rows
sortRows' cols rows = concatMap
    (\x -> sortRows' (unsafePartial tail cols) x)
    ((fromNonEmpty (:)) <$> (groupBy (\x y -> eq (compareMy (getF col) x y) EQ) sorted))
  where
    sorted = sortRows col rows
    col = unsafePartial head cols
    getF (Column f _ _) = f
sortRows' _ rows = []


sortRows :: Column -> Array Row -> Array Row
sortRows (Column f h s) rows =
  case s of
    ASC  -> sorted
    DESC -> reverse sorted
    _ -> rows
  where
    sorted = sortBy (\x y -> compareMy f x y) rows

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
renderHeadings { columns : _cols } =
  [ tr [] (renderHeading <$> _cols) ]
  where
    renderHeading :: Column -> Html Action
    renderHeading (Column f h s) =
        case s of
          ASC  -> th [ onClick ( const $ SortDesc f) ] [ text $ h <> "↑" ]
          DESC -> th [ onClick ( const $ SortAsc f) ] [ text $ h <> "↓" ]
          _    -> th [ onClick ( const $ SortAsc f) ] [ text h ]



update :: Action -> State -> State
update (SortAsc str) st = st {
  columns = map (\(Column f h s) ->
                    if str == f
                    then (Column f h ASC)
                    else  (Column f h s)) st.columns
  }

update (SortDesc str) st = st {
  columns = map (\(Column f h s) ->
                    if str == f
                    then (Column f h DESC)
                    else  (Column f h s)) st.columns
  }
update _ st = st


view :: Table -> State -> Html Action
view t st@{ columns : _cols } =
    div [] [
      renderTable sorted (renderHeadings st) (getFields _cols)
    ]
  where
    sorted = sortTable _cols t
    getFields :: Array Column -> Array String
    getFields xs = map (\(Column f _ _) -> f) xs
