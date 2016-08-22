{--
    https://github.com/chrisbuttery/elm-subscriptions/blob/master/KeyboardArrows.elm
    https://github.com/w0rm/elm-flatris/blob/master/src/Update.elm
    https://github.com/jcollard/elmtris/tree/master/src
    https://gist.github.com/pdamoc/6f7aa2d3774e5af58ebeba369637c228
--}


module Main exposing (..)

import Collage exposing (Form)
import Element exposing (Element)
import Html exposing (..)
import Html.Attributes exposing (..)
import Config
import Html exposing (..)
import Html.App as App
import Keyboard exposing (KeyCode)
import AnimationFrame exposing (..)
import Time exposing (..)
import Field exposing (Grid)
import Tetromino exposing (Tetromino)
import Random exposing (Generator, Seed)
import Bag exposing (Bag)
import Set


-- ############################################
-- # UTIL
-- ############################################


{-| Remove dupes from a list
-}
dropDuplicates : List comparable -> List comparable
dropDuplicates list =
    let
        step next ( set, acc ) =
            if Set.member next set then
                ( set, acc )
            else
                ( Set.insert next set, next :: acc )
    in
        List.foldl step ( Set.empty, [] ) list |> snd |> List.reverse


{-| Gets last item from list
-}
takeLast : List a -> Maybe a
takeLast =
    List.foldl (Just >> always) Nothing



-- ############################################
-- # MODEL
-- ############################################


type alias AnimationState =
    Maybe
        { isActive : Bool
        , interval : Time
        }


{-|
   [model.active] - Current Tetromino
   [model.bag] - Next Tetrominos
   [model.currentTime] - Current that has passed since the game started
   [model.dropTimer] - Timer for next drop
   [model.field] - The tetris playfield containing points of occupied and unoccupied spaces
   [model.isHardDrop] - Instant drop
   [model.isSoftDrop] - Drop acceleration
   [model.level] - Current level
   [model.linesCleared] - Total lines cleared
   [model.lockTimer] - Lock delay
   [model.dropDelay] - Drop Throttle / Delay
   [model.next] - Next Tetromino
   [model.rotate] - Current rotation direction
   [model.rotation] - Animation state
   [model.seed] - Next seed number
   [model.shift] - Animation state
   [model.shiftX] - Curret shift direction
-}
type alias Model =
    { active : Tetromino
    , bag : Bag
    , currentTime : Time
    , dropTimer : Time
    , field : Field.Grid
    , isHardDrop : Bool
    , isSoftDrop : Bool
    , level : Int
    , linesCleared : Int
    , dropDelay : Time
    , next : Tetromino
    , lockTimer : Time
    , rotate : ( Bool, Bool )
    , rotation : AnimationState
    , seed : Seed
    , shift : AnimationState
    , shiftX : ( Bool, Bool )
    }


initialSeed : Seed
initialSeed =
    Random.initialSeed 42


{-| Initial state
-}
model : Model
model =
    let
        ( bag, seed ) =
            Bag.choice initialSeed

        -- Pick out first item in bag; default to I piece if bag is empty
        active =
            List.head bag
                |> Maybe.withDefault Tetromino.i

        -- Pick out second item from bag; defaults to I piece
        next =
            List.take 2 bag |> takeLast |> Maybe.withDefault Tetromino.i

        -- Drop frist 2 Tetrominos because active will pick out from next
        bag' =
            List.drop 2 bag
    in
        { active = active
        , bag = bag'
        , currentTime = 0
        , dropTimer = Time.second
        , field = Field.newGrid []
        , isHardDrop = False
        , isSoftDrop = False
        , level = 1
        , linesCleared = 0
        , dropDelay = Time.second
        , next = next
        , lockTimer = Time.second
        , rotate = ( False, False )
        , rotation = Nothing
        , seed = seed
        , shift = Nothing
        , shiftX = ( False, False )
        }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- ############################################
-- # UPDATE
-- ############################################


type Msg
    = Tick Time
    | HardDrop Bool
    | SoftDrop Bool
    | ShiftLeft Bool
    | ShiftRight Bool
    | RotateCW Bool
    | RotateCCW Bool
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        HardDrop isActive ->
            ( { model | isHardDrop = isActive }
            , Cmd.none
            )

        SoftDrop isActive ->
            ( { model | isSoftDrop = isActive }
            , Cmd.none
            )

        ShiftLeft isActive ->
            ( forkShift { model | shiftX = ( isActive, False ) }
            , Cmd.none
            )

        ShiftRight isActive ->
            ( forkShift { model | shiftX = ( False, isActive ) }
            , Cmd.none
            )

        RotateCW isActive ->
            ( forkRotate { model | rotate = ( False, isActive ) }
            , Cmd.none
            )

        RotateCCW isActive ->
            ( forkRotate { model | rotate = ( isActive, False ) }
            , Cmd.none
            )

        Tick dt ->
            ( { model | currentTime = model.currentTime + dt }
                |> dropRoutine dt
                |> shiftRoutine dt
                |> rotationRoutine dt
            , Cmd.none
            )



-- #############
-- # Movement
-- ###########################


{-| Shifts Tetromino down 1 block every x ms
-}
dropRoutine : Time -> Model -> Model
dropRoutine dt model =
    let
        speed =
            if model.isSoftDrop then
                model.dropDelay / 20
            else
                model.dropDelay
    in
        if model.isHardDrop then
            let
                newActive =
                    Field.ghost model.active model.field

                returnModel =
                    if Field.isValid newActive model.field then
                        { model | active = newActive, lockTimer = 0 }
                    else
                        tetrisFlow model
            in
                { returnModel | dropTimer = Time.second }
        else if model.currentTime >= model.dropTimer then
            let
                newActive =
                    model.active |> Tetromino.shift Tetromino.Down

                isValid =
                    Field.isValid newActive model.field

                returnModel =
                    if isValid then
                        { model | active = newActive }
                    else if model.lockTimer > 0 then
                        { model | lockTimer = 0 }
                    else
                        tetrisFlow model
            in
                { returnModel | dropTimer = model.currentTime + speed }
        else
            model


{-| Gets shift direction
-}
selectDirection : ( Bool, Bool ) -> Int
selectDirection dir =
    case dir of
        ( True, False ) ->
            -1

        ( False, True ) ->
            1

        _ ->
            0


{-| Triggers shift routine
-}
forkShift : Model -> Model
forkShift model =
    if selectDirection model.shiftX /= 0 then
        { model | shift = Just { isActive = True, interval = 0 } }
    else
        { model | shift = Nothing }


{-| Triggers rotation routine
-}
forkRotate : Model -> Model
forkRotate model =
    if selectDirection model.rotate /= 0 then
        { model | rotation = Just { isActive = True, interval = 0 } }
    else
        { model | rotation = Nothing }


{-| Watches for model.shift and initiates the shift animation
-}
shiftRoutine : Time -> Model -> Model
shiftRoutine dt model =
    case model.shift of
        Just animationState ->
            -- Runs until model.animationState.isActive === Flase
            { model | shift = Just (tickFlow 120 dt animationState) }
                |> if animationState.isActive then
                    doShift (selectDirection model.shiftX)
                   else
                    identity

        Nothing ->
            model


{-| Does the same thing as our shiftRoutine except it sets model.rotation
-}
rotationRoutine : Time -> Model -> Model
rotationRoutine dt model =
    case model.rotation of
        Just animationState ->
            { model | rotation = Just (tickFlow 120 dt animationState) }
                |> if animationState.isActive then
                    doRotation (selectDirection model.rotate)
                   else
                    identity

        Nothing ->
            model


{-| Shifts our model left / right
-}
doShift : Int -> Model -> Model
doShift x model =
    { model | active = model.active |> Tetromino.shift (Tetromino.XY ( x, 0 )) }
        |> takeValid model


{-| Rotates the Tetromino
-}
doRotation : Int -> Model -> Model
doRotation int model =
    let
        current =
            { model | active = model.active |> Tetromino.rotate (Tetromino.On int) }

        next =
            takeValid model current

        -- Check for possible kicks
        next' =
            shouldWallKick model current next

        next'' =
            shouldFloorKick model current next'
    in
        next''



-- #############
-- # Effects
-- ###########################


{-| Controls the animation tick
-}
tickFlow :
    Time
    -> Time
    -> { a | isActive : Bool, interval : Time }
    -> { a | isActive : Bool, interval : Time }
tickFlow shiftRate dt animationState =
    let
        newElapsed =
            animationState.interval + dt
    in
        if newElapsed > shiftRate then
            { animationState | isActive = True, interval = newElapsed - shiftRate }
        else
            { animationState | isActive = False, interval = newElapsed }


{-| Checks if our updated model is within bounds
    @returns [Model] - The latest valid model
-}
takeValid : Model -> Model -> Model
takeValid current next =
    if Field.isValid next.active next.field then
        next
    else
        current



-- #############
-- # Kicks
-- ###########################


{-| Takes the earliest valid Tetromino position after kick
-}
kick : List ( Int, Int ) -> Model -> Model -> Model
kick msgList current next =
    case msgList of
        [] ->
            current

        msg :: rest ->
            let
                shifted =
                    { next | active = next.active |> Tetromino.shift (Tetromino.XY msg) }
            in
                if Field.isValid shifted.active next.field then
                    shifted
                else
                    kick rest current next


{-| Passes x coordinates to kick
-}
wallKick : Model -> Model -> Model
wallKick current next =
    let
        columnsOccupied =
            List.map
                (\( x, _ ) -> x)
                next.active.shape
                |> dropDuplicates
                |> List.length

        range =
            columnsOccupied // 2

        shifts =
            [1..range]
                |> List.concatMap
                    (\x -> [ ( x, 0 ), ( -x, 0 ) ])
    in
        kick shifts current next


{-| Calls wallKick fn if initial Model === latest valid Model
-}
shouldWallKick : Model -> Model -> Model -> Model
shouldWallKick prev current next =
    if next == prev then
        wallKick prev current
    else
        next


{-| Passes y coordinates to kick
-}
floorKick : Model -> Model -> Model
floorKick current next =
    let
        rowsOccupied =
            List.map
                (\( _, y ) -> y)
                next.active.shape
                |> dropDuplicates
                |> List.length

        range =
            rowsOccupied // 2

        shifts =
            [1..range]
                |> List.map (\y -> ( 0, y ))
    in
        kick shifts current next


{-| Calls floorKick fn if initial Model === latest valid Model
-}
shouldFloorKick : Model -> Model -> Model -> Model
shouldFloorKick prev current next =
    if next == prev then
        floorKick prev current
    else
        next



-- #############
-- # Bag
-- ###########################


{-| Generates new bag if current bag is empty
-}
checkBag : Model -> Model
checkBag model =
    if not (List.isEmpty model.bag) then
        model
    else
        let
            ( bag, seed ) =
                Bag.choice model.seed
        in
            { model
                | bag = bag
                , seed = seed
            }


{-|
    - Generate new bag if needed
    - Spawns new Tetromino
    - Performs clear routine
-}
tetrisFlow : Model -> Model
tetrisFlow model =
    let
        modelWithBag =
            checkBag model

        nextActive =
            model.next

        nextQueue =
            List.head modelWithBag.bag
                |> Maybe.withDefault Tetromino.i
                |> Tetromino.shift Tetromino.Reset

        nextBag =
            List.drop 1 modelWithBag.bag

        -- Attach fallen Tetromino to the grid and check clears
        ( linesCleared, nextField ) =
            Field.addTetromino modelWithBag.active modelWithBag.field
                |> Field.clearRoutine

        nextLevel =
            (model.linesCleared + linesCleared) // 10 + 1

        nextDropDelay =
            Time.second - toFloat (nextLevel * 50) - 1
    in
        { modelWithBag
            | active = nextActive
            , next = nextQueue
            , bag = nextBag
            , field = nextField
            , linesCleared = model.linesCleared + linesCleared
            , level = nextLevel
            , dropDelay = nextDropDelay
            , lockTimer = Time.second
        }



-- ############################################
-- # SUBSCRIPTIONS
-- ############################################


key : Bool -> Model -> KeyCode -> Msg
key isActive model keycode =
    case keycode of
        32 ->
            HardDrop isActive

        37 ->
            ShiftLeft isActive

        38 ->
            RotateCW isActive

        39 ->
            ShiftRight isActive

        40 ->
            SoftDrop isActive

        67 ->
            HardDrop isActive

        88 ->
            RotateCW isActive

        90 ->
            RotateCCW isActive

        _ ->
            Noop


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs (key True model)
        , Keyboard.ups (key False model)
        , AnimationFrame.diffs Tick
        ]



-- ############################################
-- # VIEW
-- ############################################


view : Model -> Html Msg
view model =
    div [ style [ ( "padding-top", "5px" ) ] ]
        [ Element.toHtml <|
            Element.flow Element.down <|
                [ Collage.collage Config.containerW Config.containerH <|
                    [ Field.shape model.field
                    , Tetromino.renderGhost (model.next |> Tetromino.shift (Tetromino.XY ( 8, -9 )))
                    , Tetromino.renderGhost (Field.ghost model.active model.field)
                    , Tetromino.render model.active
                    ]
                , Element.show ("Level: " ++ (model.level |> toString) ++ "       Lines Cleared: " ++ (model.linesCleared |> toString))
                ]
        ]



-- ############################################
-- # MAIN
-- ############################################


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
