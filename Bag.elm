module Bag exposing (..)

import Random exposing (Generator, Seed)
import Tetromino exposing (Tetromino)


type alias Bag =
    List Tetromino


{-| Generates our bag
-}
shuffle : List Float -> Bag
shuffle weights =
    let
        tetrominos =
            [ Tetromino.i
            , Tetromino.j
            , Tetromino.l
            , Tetromino.o
            , Tetromino.s
            , Tetromino.t
            , Tetromino.z
            , Tetromino.u
            ]

        weighted =
            List.map2 (,) weights tetrominos

        sorted =
            List.sortBy fst weighted
    in
        List.map snd sorted


{-| Produces a list of Tetrominos
-}
generate : Generator Bag
generate =
    let
        weights =
            Random.list 8 (Random.float 0 1)
    in
        Random.map shuffle weights


{-| Pulls our weighted Tetrominos into a list
-}
choice : Seed -> ( Bag, Random.Seed )
choice seed =
    Random.step generate seed
