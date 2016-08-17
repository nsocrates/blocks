module FieldContainer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

-- Our playfield will be 300x600 : 10x20
-- We will have 30x30 cells
-- (+/-)10% will move 1 cell
    -- 10% = 30px
-- Vanish zone is 2 cells high (60px)
    -- Start at -10%

containerStyle : Html.Attribute a
containerStyle =
  style
    [ ("position", "relative")
    , ("width", "300px")
    , ("height", "600px")
    , ("background-color", "#d4dee4")
    ]

makeContainer : List (Html.Html a) -> Html.Html a
makeContainer children =
  div [containerStyle]
    children
