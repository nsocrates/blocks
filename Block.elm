-- Block stylings


module Block exposing (..)

import Color exposing (Color)
import Collage exposing (Form)
import Config


type alias Block =
    { color : Color }


{-| Make a single block size 30x30
-}
shape : Block -> Form
shape block =
    let
        shape =
            Collage.square Config.blockSize

        blockShape =
            Collage.filled block.color shape

        border =
            Collage.outlined (Collage.solid <| Color.rgba 33 33 33 0.5) shape
    in
        Collage.group
            [ blockShape, border ]
