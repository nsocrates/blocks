module Matrix exposing (..)


type alias Space =
    ( Int, Int )


{-| Assigns Space
-}
assign : Int -> Int -> Space
assign =
    (,)
