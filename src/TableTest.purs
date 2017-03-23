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
            sortCol : Table.SortColumn "name" Table.DESC
          , columns : [
                Table.ViewColumn "name" "Name"
              , Table.ViewColumn "year" "Year"
              , Table.ViewColumn "city" "City"
              , Table.ViewColumn "state" "State"
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
        h1 [] [ text "Birthplaces of U.S. Presidents" ]
      , input [ placeholder "Search by Name", onInput SetQuery ] []
      , span [] [ text $ "found " <> (show $ length acceptablePeople) ]
      , map Child $ Table.view (toTable acceptablePeople) st.table
    ]


data Person = Person
  { name :: String
  , year :: Int
  , city :: String
  , state :: String
  }

presidents :: Array Person
presidents =
  [ Person { name : "George Washington"      , year: 1732,  city : "Westmoreland County", state : "Virginia" }
  , Person { name : "John Adams"             , year: 1735,  city : "Braintree"          , state : "Massachusetts" }
  , Person { name : "Thomas Jefferson"       , year: 1743,  city : "Shadwell"           , state : "Virginia" }
  , Person { name : "James Madison"          , year: 1751,  city : "Port Conway"        , state : "Virginia" }
  , Person { name : "James Monroe"           , year: 1758,  city : "Monroe Hall"        , state : "Virginia" }
  , Person { name : "Andrew Jackson"         , year: 1767,  city : "Waxhaws Region"     , state : "South/North Carolina" }
  , Person { name : "John Quincy Adams"      , year: 1767,  city : "Braintree"          , state : "Massachusetts" }
  , Person { name : "William Henry Harrison" , year: 1773,  city : "Charles City County", state : "Virginia" }
  , Person { name : "Martin Van Buren"       , year: 1782,  city : "Kinderhook"         , state : "New York" }
  , Person { name : "Zachary Taylor"         , year: 1784,  city : "Barboursville"      , state : "Virginia" }
  , Person { name : "John Tyler"             , year: 1790,  city : "Charles City County", state : "Virginia" }
  , Person { name : "James Buchanan"         , year: 1791,  city : "Cove Gap"           , state : "Pennsylvania" }
  , Person { name : "James K. Polk"          , year: 1795,  city : "Pineville"          , state : "North Carolina" }
  , Person { name : "Millard Fillmore"       , year: 1800,  city : "Summerhill"         , state : "New York" }
  , Person { name : "Franklin Pierce"        , year: 1804,  city : "Hillsborough"       , state : "New Hampshire" }
  , Person { name : "Andrew Johnson"         , year: 1808,  city : "Raleigh"            , state : "North Carolina" }
  , Person { name : "Abraham Lincoln"        , year: 1809,  city : "Sinking spring"     , state : "Kentucky" }
  , Person { name : "Ulysses S. Grant"       , year: 1822,  city : "Point Pleasant"     , state : "Ohio" }
  , Person { name : "Rutherford B. Hayes"    , year: 1822,  city : "Delaware"           , state : "Ohio" }
  , Person { name : "Chester A. Arthur"      , year: 1829,  city : "Fairfield"          , state : "Vermont" }
  , Person { name : "James A. Garfield"      , year: 1831,  city : "Moreland Hills"     , state : "Ohio" }
  , Person { name : "Benjamin Harrison"      , year: 1833,  city : "North Bend"         , state : "Ohio" }
  , Person { name : "Grover Cleveland"       , year: 1837,  city : "Caldwell"           , state : "New Jersey" }
  , Person { name : "William McKinley"       , year: 1843,  city : "Niles"              , state : "Ohio" }
  , Person { name : "Woodrow Wilson"         , year: 1856,  city : "Staunton"           , state : "Virginia" }
  , Person { name : "William Howard Taft"    , year: 1857,  city : "Cincinnati"         , state : "Ohio" }
  , Person { name : "Theodore Roosevelt"     , year: 1858,  city : "New York City"      , state : "New York" }
  , Person { name : "Warren G. Harding"      , year: 1865,  city : "Blooming Grove"     , state : "Ohio" }
  , Person { name : "Calvin Coolidge"        , year: 1872,  city : "Plymouth"           , state : "Vermont" }
  , Person { name : "Herbert Hoover"         , year: 1874,  city : "West Branch"        , state : "Iowa" }
  , Person { name : "Franklin D. Roosevelt"  , year: 1882,  city : "Hyde Park"          , state : "New York" }
  , Person { name : "Harry S. Truman"        , year: 1884,  city : "Lamar"              , state : "Missouri" }
  , Person { name : "Dwight D. Eisenhower"   , year: 1890,  city : "Denison"            , state : "Texas" }
  , Person { name : "Lyndon B. Johnson"      , year: 1908,  city : "Stonewall"          , state : "Texas" }
  , Person { name : "Ronald Reagan"          , year: 1911,  city : "Tampico"            , state : "Illinois" }
  , Person { name : "Richard M. Nixon"       , year: 1913,  city : "Yorba Linda"        , state : "California" }
  , Person { name : "Gerald R. Ford"         , year: 1913,  city : "Omaha"              , state : "Nebraska" }
  , Person { name : "John F. Kennedy"        , year: 1917,  city : "Brookline"          , state : "Massachusetts" }
  , Person { name : "George H. W. Bush"      , year: 1924,  city : "Milton"             , state : "Massachusetts" }
  , Person { name : "Jimmy Carter"           , year: 1924,  city : "Plains"             , state : "Georgia" }
  , Person { name : "George W. Bush"         , year: 1946,  city : "New Haven"          , state : "Connecticut" }
  , Person { name : "Bill Clinton"           , year: 1946,  city : "Hope"               , state : "Arkansas" }
  , Person { name : "Barack Obama"           , year: 1961,  city : "Honolulu"           , state : "Hawaii" }
  , Person { name : "Donald Trump"           , year: 1946,  city : "New York City"      , state : "New York" }
  ]

toTable :: Array Person -> Table.Table
toTable xs = Table.Table ( toRow <$> xs )
  where
    toRow (Person {name: _name, year: _year, city: _city, state: _state }) =
      Table.Row [
          (Table.CellString (Table.Name "name") _name)
        , (Table.CellInt (Table.Name "year") _year)
        , (Table.CellString (Table.Name "city") _city)
        , (Table.CellString (Table.Name "state") _state)
      ]
