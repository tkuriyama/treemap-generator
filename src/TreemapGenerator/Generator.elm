module TreemapGenerator.Generator exposing (main)

{-| A squarified treemap generator with user controls.
-}

import Browser
import Browser.Events exposing (onResize)
import List.Nonempty as NE
import Random
import Random.List as RL
import TreemapGenerator.SampleData as SampleData
import TreemapGenerator.Types exposing (..)
import TreemapGenerator.View as View



--------------------------------------------------------------------------------
-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }



--------------------------------------------------------------------------------
-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        data =
            parseData SampleData.data

        env =
            { windowW = flags.windowWidth
            , windowH = flags.windowHeight
            , w = flags.windowWidth * 0.9
            , h = flags.windowHeight * 0.7
            , groupCt = NE.length data
            , cellCt = NE.map NE.length data |> NE.foldl1 (+)
            , groupBorderWidth = 0.25
            , cellBorderWidth = 0.25
            , colorScale = RedGreen
            , groupSortOrder = Descending
            , cellSortOrder = Descending
            }
    in
    ( { env = env, data = data }, Cmd.none )


parseData : List (List ( Float, Float )) -> Data
parseData data =
    let
        defaultPair =
            ( 0, 0 )

        defaultGroup =
            NE.fromElement defaultPair

        parseGroup g =
            case g of
                [] ->
                    defaultGroup

                x :: xs ->
                    NE.Nonempty x xs
    in
    case data of
        [] ->
            NE.fromElement defaultGroup

        x :: xs ->
            NE.Nonempty (parseGroup x) (List.map parseGroup xs)



--------------------------------------------------------------------------------
-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        env =
            model.env
    in
    case msg of
        UpdateWidth w ->
            ( { model | env = { env | w = w } }, Cmd.none )

        UpdateHeight h ->
            ( { model | env = { env | h = h } }, Cmd.none )

        UpdateGroupCt n ->
            ( { model | env = { env | groupCt = n } }
            , Random.generate Regroup (regroup n model.data)
            )

        UpdateCellCt n ->
            ( { model | env = { env | cellCt = n } }
            , Random.generate
                UpdateCells
                (updateCells model.data model.env.cellCt n)
            )

        UpdateGroupSortOrder x ->
            ( { model | env = { env | groupSortOrder = x } }, Cmd.none )

        UpdateCellSortOrder x ->
            ( { model | env = { env | cellSortOrder = x } }, Cmd.none )

        UpdateColorScale c ->
            ( { model | env = { env | colorScale = c } }, Cmd.none )

        UpdateGroupBorderWidth w ->
            ( { model | env = { env | groupBorderWidth = w } }, Cmd.none )

        UpdateCellBorderWidth w ->
            ( { model | env = { env | cellBorderWidth = w } }, Cmd.none )

        WindowResize ( w, h ) ->
            let
                (w_, h_) =
                    (toFloat w, toFloat h)
            in 
            ( { model | env = { env
                                  | windowW = w_
                                  , windowH = h_
                                  , w = env.w * w_ / env.windowW
                                  , h = env.h * h_ / env.windowH
                              }
              } 
            , Cmd.none
            )

        Regroup data ->
            ( { model | data = data }, Cmd.none )

        UpdateCells data ->
            ( { model | data = data }, Cmd.none )



-- Update Groups nad Cells


regroup : Int -> Data -> Random.Generator Data
regroup n groups =
    NE.concat groups |> NE.toList |> regroupList n


regroupList : Int -> List Pair -> Random.Generator Data
regroupList n pairs =
    let
        f i generatorAcc =
            generatorAcc |> Random.andThen (takeRandom i)
    in
    List.range 1 n
        |> List.foldr f (Random.constant ( [], pairs ))
        |> Random.map (Tuple.first >> parseData)


takeRandom :
    Int
    -> ( List (List a), List a )
    -> Random.Generator ( List (List a), List a )
takeRandom n ( acc, elems ) =
    if n == 1 then
        Random.constant ( elems :: acc, [] )

    else
        Random.int 1 (maxElems elems n)
            |> Random.andThen
                (\ct -> RL.choices ct elems)
            |> Random.andThen
                (\( xs, ys ) -> Random.constant ( xs :: acc, ys ))


maxElems : List a -> Int -> Int
maxElems elems n =
    (List.length elems - n)
        |> toFloat
        |> (*) 0.8
        |> round


updateCells : Data -> Int -> Int -> Random.Generator Data
updateCells groups current target =
    if current == target then
        Random.constant groups

    else
        let
            ( f, diff ) =
                if current > target then
                    ( removeCells, current - target )

                else
                    ( addCells, target - current )
        in
        f (NE.concat groups |> NE.toList) diff
            |> Random.andThen (regroupList (NE.length groups))


removeCells : List Pair -> Int -> Random.Generator (List Pair)
removeCells pairs n =
    RL.choices n pairs
        |> Random.andThen (\( xs, ys ) -> Random.constant ys)


addCells : List Pair -> Int -> Random.Generator (List Pair)
addCells pairs n =
    RL.choices n pairs
        |> Random.andThen (\( xs, ys ) -> Random.constant (xs ++ pairs))



--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\w h -> WindowResize ( w, h ))
        ]
