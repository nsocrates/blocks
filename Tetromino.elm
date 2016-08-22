module Tetromino exposing (..)

import Color exposing (Color)
import Collage exposing (Form)
import Block exposing (Block)
import Matrix exposing (Space)
import Config


{-| Block offset to line up grid
-}
blockOffset : ( Float, Float )
blockOffset =
    ( negate Config.blockSize / 2
    , negate Config.blockSize / 2
    )


{-|
   shape - Default shape in first position
   pivot - Rotation origin
   color - Default color
   matrix - Center coordinates on the grid
-}
type alias Tetromino =
    { shape : List Space
    , pivot : ( Float, Float )
    , block : Block
    , matrix : Space
    }


{-| Draws the shape Tetromino
-}
render : Tetromino -> Form
render { shape, block, matrix } =
    let
        -- Matrix to attach to our Tetromino
        matrix' =
            Collage.filled (Color.rgba 0 0 0 0) (Collage.square Config.matrixSize)

        -- Makes a block form element
        block' =
            Block.shape block

        {--
            Builds the Tetromino shape by moving each block to the specified
            coordinates and offsetting everything by -1/2 block
        --}
        build ( x, y ) =
            block'
                |> Collage.move
                    ( toFloat x * Config.blockSize + fst blockOffset
                    , toFloat y * Config.blockSize + snd blockOffset
                    )

        -- Block group
        group =
            List.map build shape

        -- The Tetromino form
        form =
            Collage.group (group ++ [ matrix' ])
    in
        -- Apply any translations to the Tetromino form
        form
            |> Collage.move
                ( toFloat (fst matrix) * Config.blockSize
                , toFloat (snd matrix) * Config.blockSize
                )


renderGhost : Tetromino -> Form
renderGhost tetromino =
    let
        tetromino' =
            { tetromino | block = Block (Color.rgb 96 125 139) }
    in
        Collage.alpha 0.3 (render tetromino')



-- *******************
-- Matrix
-- *******************


matrix : Space
matrix =
    Matrix.assign 0 9



-- *******************
-- Shift
-- *******************


type ShiftMsg
    = Left
    | Right
    | Down
    | XY Space
    | Reset


shift : ShiftMsg -> Tetromino -> Tetromino
shift msg model =
    case msg of
        Right ->
            { model
                | matrix =
                    ( fst model.matrix + 1
                    , snd model.matrix
                    )
            }

        Left ->
            { model
                | matrix =
                    ( fst model.matrix - 1
                    , snd model.matrix
                    )
            }

        Down ->
            { model
                | matrix =
                    ( fst model.matrix
                    , snd model.matrix - 1
                    )
            }

        XY ( x, y ) ->
            { model
                | matrix =
                    ( fst model.matrix + x
                    , snd model.matrix + y
                    )
            }

        Reset ->
            { model | matrix = matrix }



-- *******************
-- Rotation
-- *******************
{--

  [ 0, -1 ]  *  [ a ]  =  [ (0 * a) + ( -1 * b ) ]
  [ 1,  0 ]  *  [ b ]  =  [ (1 * a) + ( 0 *  b ) ]

--}


{-| Plots transformation rotation points
-}
transformRotate : Tetromino -> Int -> List Space
transformRotate { shape, pivot } rotation =
    let
        -- Get relative points by subtracting absolute from pivot
        getRelative ( pX, pY ) ( aX, aY ) =
            ( aX - pX, aY - pY )

        -- Multiply by rotation matrix
        getTransformed ( rX, rY ) =
            ( (0 * rX + 1 * rY) * rotation
            , (-1 * rX + 0 * rY) * rotation
            )

        -- Add transformed vector with pivot vectors to get our rotated vector
        getTransformedCoordinates ( pX, pY ) ( tX, tY ) =
            ( pX + tX |> round
            , pY + tY |> round
            )

        -- Prepare functions
        calcRelative =
            getRelative pivot

        calcNewCoordinates =
            getTransformedCoordinates pivot

        -- Map it all out
        newShape =
            shape
                |> List.map
                    (\l ->
                        ( l |> fst |> toFloat
                        , l |> snd |> toFloat
                        )
                            |> calcRelative
                            |> getTransformed
                            |> calcNewCoordinates
                    )
    in
        newShape


type RotationMsg
    = CW
    | CCW
    | On Int


{-| Rotates the tetromino 90deg CW / CWW
-}
rotate : RotationMsg -> Tetromino -> Tetromino
rotate msg model =
    case msg of
        CW ->
            { model | shape = transformRotate model 1 }

        CCW ->
            { model | shape = transformRotate model -1 }

        On int ->
            { model | shape = transformRotate model int }



-- *******************
-- Tetrominos
-- *******************
{--
   [  0,   0,   0,   0  ]
   [  0,   0,   0,   0  ]
   [  0,   0,   0,   0  ]
   [  0,   0,   0,   0  ]
--}


i : Tetromino
i =
    { shape =
        [ Matrix.assign -1 1
        , Matrix.assign 0 1
        , Matrix.assign 1 1
        , Matrix.assign 2 1
        ]
    , pivot = ( 0.5, 0.5 )
    , block = Block (Color.rgb 0 188 212)
    , matrix = matrix
    }



-- ==============================================


j : Tetromino
j =
    { shape =
        [ Matrix.assign -1 2
        , Matrix.assign -1 1
        , Matrix.assign 0 1
        , Matrix.assign 1 1
        ]
    , pivot = ( 0, 1 )
    , block = Block (Color.rgb 33 150 243)
    , matrix = matrix
    }



-- ==============================================


l : Tetromino
l =
    { shape =
        [ Matrix.assign 1 2
        , Matrix.assign 1 1
        , Matrix.assign 0 1
        , Matrix.assign -1 1
        ]
    , pivot = ( 0, 1 )
    , block = Block (Color.rgb 255 152 0)
    , matrix = matrix
    }



-- ==============================================


o : Tetromino
o =
    { shape =
        [ Matrix.assign 0 1
        , Matrix.assign 1 1
        , Matrix.assign 0 2
        , Matrix.assign 1 2
        ]
    , pivot = ( 0.5, 1.5 )
    , block = Block (Color.rgb 255 235 59)
    , matrix = matrix
    }



-- ==============================================


s : Tetromino
s =
    { shape =
        [ Matrix.assign 0 2
        , Matrix.assign 1 2
        , Matrix.assign -1 1
        , Matrix.assign 0 1
        ]
    , pivot = ( 0, 1 )
    , block = Block (Color.rgb 76 175 80)
    , matrix = matrix
    }



-- ==============================================


t : Tetromino
t =
    { shape =
        [ Matrix.assign 0 2
        , Matrix.assign -1 1
        , Matrix.assign 0 1
        , Matrix.assign 1 1
        ]
    , pivot = ( 0, 1 )
    , block = Block (Color.rgb 156 39 176)
    , matrix = matrix
    }



-- ==============================================


z : Tetromino
z =
    { shape =
        [ Matrix.assign -1 2
        , Matrix.assign 0 2
        , Matrix.assign 0 1
        , Matrix.assign 1 1
        ]
    , pivot = ( 0, 1 )
    , block = Block (Color.rgb 244 67 54)
    , matrix = matrix
    }



-- ==============================================


u : Tetromino
u =
    { shape =
        [ Matrix.assign -1 2
        , Matrix.assign 0 2
        , Matrix.assign 1 2
        , Matrix.assign -1 1
        , Matrix.assign 1 1
        ]
    , pivot = ( 0, 1 )
    , block = Block (Color.rgb 121 85 72)
    , matrix = matrix
    }
