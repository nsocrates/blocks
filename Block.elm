module Block exposing (..)

import Html exposing (li)
import Html.Attributes exposing (style)

type alias Block = Html.Html String
type alias Fill = Html.Attribute Maybe
type alias Color = String

fill : Color -> Fill
fill color =
  style 
    [ ("position", "absolute")
    , ("list-style", "none")
    , ("width", "30px")
    , ("height", "30px")
    , ("background-color", color)
    ]
