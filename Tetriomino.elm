import Html exposing (..)
import Html.Attributes exposing (..)

import Block exposing (..)
import FieldContainer exposing (..)

-- (x, y) Coordinates
type alias Space =
  (Int, Int)

type alias Pivot =
  (Float, Float)


{- Tetriomino Object

  shape - Default shape in first position
  pivot - Pivot origin
  color - Default color

-}
type alias Tetriomino =
  { shape : List Space
  , pivot : Pivot
  , color : Block.Color
  }


-- Li element
type alias BlockGroup =
  List (Html Maybe)


-- Block
liStyle : String -> (String, String) -> Html.Attribute a
liStyle color (x, y) =
  style
    [ ("position", "absolute")
    , ("list-style", "none")
    , ("width", "30px")
    , ("height", "30px")
    , ("top" , y)
    , ("left", x)
    , ("background-color", color)
    ]


-- PlayField
ulStyle : Html.Attribute a
ulStyle =
  style
    [ ("position", "relative")
    , ("list-style", "none")
    , ("width", "100%")
    , ("height", "100%")
    , ("margin", "0")
    , ("padding", "0")
    ]


-- Active Tetriomino Matrix
activeMatrix : List (String, String)
activeMatrix =
    [ ("position", "absolute")
    , ("width", "120px")
    , ("height", "120px")
    , ("left", "90px")
    , ("top", "0")
    --, ("width", "40%")
    --, ("height", "20%")
    --, ("top", "0")
    --, ("left", "30%")
    ]


-- ############################################
-- # BUILD FN
-- ############################################

-- Assigns Space
makeSpace : Int -> Int -> Space
makeSpace =
  (,)


-- Builds Tetriomino Shape as li
makeTet : Tetriomino -> BlockGroup
makeTet { shape, color } =
  let
    -- 30x30
    coordinates (x, y) =
      (toString (x * 30) ++ "px"
      , toString (y * 30) ++ "px")
      --(toString (x * 25) ++ "%"
      --, toString (y * 25) ++ "%")

    draw (x, y) =
      li [liStyle color (coordinates (x, y))] []

    group =
      List.map draw shape

  in group


-- ############################################
-- # SHIFT FN
-- ############################################

-- Shift +/- 30px left
shift : String -> List (String, String)
shift px =
  [ ("left", px) ]


-- ############################################
-- # ROTATION FN
-- ############################################

{--
  /* ************************************************************************************** */

    https://www.youtube.com/watch?v=Atlr5vvdchY
    http://gamedev.stackexchange.com/questions/17974/how-to-rotate-blocks-in-tetris

  /* ************************************************************************************** */

  * 90deg = pi / 2

  1. Get relative vector
    -- absolute - pivot = relative (Vr)

  2. Multiply by rotation matrix (pi / 2)
    -- r * Vr = Vt

  [ 0, -1 ]  *  [ a ]  =  [ (0 * a) + ( -1 * b ) ]
  [ 1,  0 ]  *  [ b ]  =  [ (1 * a) + ( 0 *  b ) ]

  3. Add transformed vector with pivot to get coordinates
    -- pivot + Vt = V1

--}

-- Plots transformation rotation points
transform : Tetriomino -> Int -> List Space
transform { shape, pivot } sequence =
  let
    -- Get relative points by subtracting absolute from pivot
    getRelative (pX, pY) (aX, aY) =
      (aX - pX, aY - pY)

    -- Multiply by rotation matrix
    -- For CCW, multiply by -1
    getTransformed (rX, rY) =
      ( ( 0 * rX  + -1 * rY ) * sequence
      , ( 1 * rX  +  0 * rY ) * sequence )

    -- Add transformed vector with pivot vectors to get our rotated vector
    getTransformedCoordinates (pX, pY) (tX, tY) =
      ( pX + tX |> round
      , pY + tY |> round )

    -- Prepare functions
    calcRelative = getRelative pivot
    calcNewCoordinates = getTransformedCoordinates pivot

    -- Map it all out
    newShape =
      shape
        |> List.map (
          \l -> ( fst l |> toFloat
                , snd l |> toFloat )
                  |> calcRelative
                  |> getTransformed
                  |> calcNewCoordinates
                )

  in
    newShape

-- Rotates the block 90deg CW
rotateCW : Tetriomino -> Tetriomino
rotateCW tetriomino =
  -- We only need to update the shape
  { tetriomino |
    shape = transform tetriomino 1
  }

-- Rotates the block 90deg CCW
rotateCCW : Tetriomino -> Tetriomino
rotateCCW tetriomino =
  -- We only need to update the shape
  { tetriomino |
    shape = transform tetriomino -1
  }


-- ############################################
-- # TETRIOMINO OBJECTS
-- ############################################

{- *************************************************

       -  0,  25,  50,  75  -
  =======================================
    0  [  0,   0,   0,   0  ]  ||  0
    1  [  0,   0,   0,   0  ]  ||  25
    2  [  0,   0,   0,   0  ]  ||  50
    3  [  0,   0,   0,   0  ]  ||  75
  =======================================
       -  0    1    2    3  -

************************************************** -}


{-
  [0, 0, 0, 0]
  [1, 1, 1, 1]
  [0, 0, 0, 0]
  [0, 0, 0, 0]
-}

i : Tetriomino
i =
  { shape = [ makeSpace 0 1
            , makeSpace 1 1
            , makeSpace 2 1
            , makeSpace 3 1
            ]

  , pivot = (1.5, 1.5)
  , color = "#00bcd4"
  }


{-
  [1, 0, 0, 0]
  [1, 1, 1, 0]
  [0, 0, 0, 0]
  [0, 0, 0, 0]
-}

j : Tetriomino
j =
  { shape = [ makeSpace 0 0
            , makeSpace 0 1
            , makeSpace 1 1
            , makeSpace 2 1
            ]

  , pivot = (1, 1)
  , color = "#2196f3"
  }


{-
  [0, 0, 1, 0]
  [1, 1, 1, 0]
  [0, 0, 0, 0]
  [0, 0, 0, 0]
-}

l : Tetriomino
l =
  { shape = [ makeSpace 2 0
            , makeSpace 2 1
            , makeSpace 1 1
            , makeSpace 0 1
            ]

  , pivot = (1, 1)
  , color = "#ff9800"
  }


{-
  [0, 1, 1, 0]
  [0, 1, 1, 0]
  [0, 0, 0, 0]
  [0, 0, 0, 0]
-}
o : Tetriomino
o =
  { shape = [ makeSpace 1 0
            , makeSpace 2 0
            , makeSpace 1 1
            , makeSpace 2 1
            ]

  , pivot = (1.5, 0.5)
  , color = "#ffeb3b"
  }


{-
  [0, 1, 1, 0]
  [1, 1, 0, 0]
  [0, 0, 0, 0]
  [0, 0, 0, 0]
-}

s : Tetriomino
s =
  { shape = [ makeSpace 1 0
            , makeSpace 2 0
            , makeSpace 0 1
            , makeSpace 1 1
            ]

  , pivot = (1, 1)
  , color = "#4caf50"
  }


{-
  [0, 1, 0, 0]
  [1, 1, 1, 0]
  [0, 0, 0, 0]
  [0, 0, 0, 0]
-}

t : Tetriomino
t =
  { shape = [ makeSpace 1 0
            , makeSpace 0 1
            , makeSpace 1 1
            , makeSpace 2 1
            ]

  , pivot = (1, 1)
  , color = "#9c27b0"
  }


{-
  [1, 1, 0, 0]
  [0, 1, 1, 0]
  [0, 0, 0, 0]
  [0, 0, 0, 0]
-}

z : Tetriomino
z =
  { shape = [ makeSpace 0 0
            , makeSpace 1 0
            , makeSpace 1 1
            , makeSpace 2 1
            ]

  , pivot = (1, 1)
  , color = "#f44336"
  }


-- Main
-- ===================

tetriomino : Tetriomino
tetriomino = t |> rotateCW |> rotateCCW

main : Html Maybe
main =
  makeContainer
    [ div [style (activeMatrix ++ shift "0")]
        [ makeTet tetriomino
          |> ul [ulStyle]
        ]
    ]
