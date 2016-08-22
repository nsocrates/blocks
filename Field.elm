module Field exposing (..)

import Collage exposing (Form)
import Dict exposing (Dict)
import Tetromino exposing (Tetromino)
import Matrix exposing (Space)
import Config
import Block exposing (Block)


{-| Grid type
-}
type alias Grid =
    Dict Space Block



-- ############################################
-- # UTILITY FN
-- ############################################


{-| Turns our grid into a form element
-}
shape : Grid -> Form
shape grid =
    Dict.foldr addBlock background grid


{-| Makes our field bg
-}
background : Form
background =
    let
        shape =
            Collage.rect Config.fieldW Config.fieldH

        border =
            Collage.outlined (Collage.solid Config.bgColor) shape
    in
        Collage.group [ border, Collage.filled Config.bgColor shape ]


{-| Makes a new grid
-}
newGrid : List ( Space, Block ) -> Grid
newGrid =
    Dict.fromList


{-| Adds a new block to the grid
-}
addBlock : Space -> Block -> Form -> Form
addBlock ( x, y ) block form =
    let
        offset =
            ( negate (toFloat (Config.totalCols - 1) / 2 * Config.blockSize)
            , negate (toFloat (Config.totalRows - 1) / 2 * Config.blockSize)
            )

        coordinates =
            ( (toFloat x * Config.blockSize + fst offset)
            , (toFloat y * Config.blockSize + snd offset)
            )

        blockShape =
            Block.shape block |> Collage.move coordinates
    in
        Collage.group
            [ form, blockShape ]



-- ############################################
-- # UPDATE
-- ############################################


{-| Check if row is filled; returns True / False
-}
shouldClearRow : Int -> Grid -> Bool
shouldClearRow row grid =
    let
        blocks =
            -- Our grid is type : (x, y) color; we only need y value
            Dict.filter (\( _, y ) _ -> y == row) grid
    in
        -- Return True if blocks in specified row === 10
        Dict.size blocks == Config.totalCols


{-| Clears a single row on command
-}
clearRow : Int -> Grid -> Grid
clearRow row grid =
    -- Determines how each block will be copied over
    let
        assign ( x, y ) color newGrid =
            -- If current row is below cleared row, just copy it
            if (y < row) then
                Dict.insert ( x, y ) color newGrid
                -- If current row is above cleared row, shift down
            else if (y > row) then
                Dict.insert ( x, y - 1 ) color newGrid
                -- Empty row
            else
                newGrid
    in
        {--
            1. Fold over the key-value paris of our grid
            2. Feed our shift fn with an empty grid
            3. Apply it to our grid
        --}
        Dict.foldr assign Dict.empty grid


{-|
  Determines which rows to clear, and clears it
  @returns [(Int, _)] - Number of lines cleared
  @returns [(_, Grid)] - New grid
-}
clearRoutine : Grid -> ( Int, Grid )
clearRoutine =
    let
        clearlines row lineCount grid =
            -- We've reached the top, so return the number of lines cleared and a new grid
            if (row >= Config.totalRows) then
                ( lineCount, grid )
                {--If the row should be cleared, then:
                       1. Clear it
                       2. Increment line count
                       3. Call fn on the same row
                --}
            else if (shouldClearRow row grid) then
                clearlines row (lineCount + 1) (clearRow row grid)
                -- If we do not need to clear the current row, we call fn on the row above it
            else
                clearlines (row + 1) lineCount grid
    in
        -- Start at the bottom with 0 lines cleared
        clearlines 0 0


{-| Adds a tetromino to the current grid
-}
addTetromino : Tetromino -> Grid -> Grid
addTetromino tetromino grid =
    let
        -- Get relative position
        { shape, block } =
            getRelativePos tetromino

        -- Map through the shapes and record its positions on the grid
        asGrid =
            List.map2 (,) shape (List.repeat (List.length shape) block) |> newGrid
    in
        -- Return a union of old and new grid
        Dict.union asGrid grid


{-| Calculates Tetromino block position within the active matrix relative to the grid
-}
getRelativePos : Tetromino -> Tetromino
getRelativePos tetromino =
    let
        -- This is the point where matrix === (0, 0)
        center =
            ( 4, 9 )

        -- Add the center to x and y position of matrix to center it relative to the grid
        xR =
            fst tetromino.matrix + fst center

        yR =
            snd tetromino.matrix + snd center

        -- Add the relative points to each point of the shape to get the coordinates
        -- relative to the grid
        rel shape =
            List.map (\l -> ( fst l + xR, snd l + yR )) shape
    in
        { tetromino
            | shape = rel tetromino.shape
            , matrix = ( xR, yR )
        }


{-| Checks if Tetromino is within the grid
-}
isInBounds : Tetromino -> Bool
isInBounds { shape } =
    let
        checkBounds ( x, y ) =
            y >= 0 && x >= 0 && x < Config.totalCols
    in
        List.all checkBounds shape


{-| Checks if Tetromino blocks are overlapping an occupied space
-}
isColliding : Tetromino -> Grid -> Bool
isColliding { shape } grid =
    let
        checkLocation space =
            Dict.member space grid
    in
        List.any checkLocation shape


{-| Examines the validity of the current position of the Tetromino
-}
isValid : Tetromino -> Grid -> Bool
isValid tetromino grid =
    let
        tetrominoR =
            getRelativePos tetromino
    in
        (isInBounds tetrominoR) && not (isColliding tetrominoR grid)


{-| Checks for validity of landing position
-}
isValidLanding : Tetromino -> Grid -> Bool
isValidLanding tetromino grid =
    (isInBounds tetromino) && not (isColliding tetromino grid)


{-| Gets position of ghost piece
-}
ghost : Tetromino -> Grid -> Tetromino
ghost tetromino =
    let
        rTetromino =
            getRelativePos tetromino

        startY =
            snd rTetromino.matrix

        calculateLanding holder y tetromino grid =
            let
                landing =
                    tetromino |> Tetromino.shift (Tetromino.XY ( 0, y ))

                relativeLandingPos =
                    getRelativePos landing

                relativeYpos =
                    snd relativeLandingPos.matrix
            in
                -- Break fn because ghost piece is above active Tetromino
                if (relativeYpos > startY) then
                    holder
                    -- Return landing if position is found
                else if (isValidLanding relativeLandingPos grid) then
                    calculateLanding landing (y - 1) tetromino grid
                else
                    -- Call fn again on the row above
                    holder
    in
        calculateLanding tetromino -1 tetromino
