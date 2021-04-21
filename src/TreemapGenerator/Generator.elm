module TreemapGenerator.Generator exposing (main)

{-| A squarified treemap generator with user controls.
-}

import Browser
import Browser.Events exposing (onResize)
import List.Nonempty as NE
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
        , subscriptions = \_ -> Sub.none
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
            , h = flags.windowHeight * 0.9
            , groupCt = NE.length data
            , nodeCt = NE.map NE.length data |> NE.foldl1 (+)
            , noise = 0.0
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

        UpdateGroupSortOrder x ->
            ( { model | env = { env | groupSortOrder = x } }, Cmd.none )

        UpdateCellSortOrder x ->
            ( { model | env = { env | groupSortOrder = x } }, Cmd.none )

        UpdateColorScale c ->
            ( { model | env = { env | colorScale = c } }, Cmd.none )

        UpdateGroupBorderWidth w ->
            ( { model | env = { env | groupBorderWidth = w } }, Cmd.none )

        UpdateCellBorderWidth w ->
            ( { model | env = { env | cellBorderWidth = w } }, Cmd.none )

        WindowResize ( w, h ) ->
            ( { model | env = { env | w = toFloat w, h = toFloat h } }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\w h -> WindowResize ( w, h ))
        ]
