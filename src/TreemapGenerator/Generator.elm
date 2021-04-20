module TreemapGenerator.Generator exposing (main)

{-| A squarified treemap generator with user controls.
-}

import Browser
import List.Nonempty as NE
import TreemapGenerator.Types exposing (..)
import TreemapGenerator.SampleData as SampleData
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


init : Flags -> (Model, Cmd Msg)
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
            , nodeCt = NE.map (NE.length) data |> NE.foldl1 (+)
            , noise = 0.0
            , groupBorderWidth = 0.25
            , cellBorderWidth = 0.25
            , colorScale = RedGreen
            , groupSortOrder = Descending
            , cellSortOrder = Descending
            }
    in
        ( { env = env, data = data }, Cmd.none)

parseData : List ( List (Float, Float ) ) -> Data
parseData data =
    let defaultPair =
            (0, 0)
        defaultGroup =
            NE.fromElement defaultPair

        parseGroup g =
            case g of
                [] ->
                    defaultGroup
                (x :: xs ) ->
                    NE.Nonempty x xs
    in 
    case data of
        [] ->
            NE.fromElement defaultGroup
        (x :: xs) ->
            NE.Nonempty (parseGroup x) (List.map parseGroup xs)


--------------------------------------------------------------------------------
-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
      let env =
              model.env
      in 
      case msg of
          UpdateGroupSortOrder x ->
            ({ model | env = { env | groupSortOrder = x } }, Cmd.none)
          UpdateCellSortOrder x ->
              ({ model | env = { env | groupSortOrder = x } }, Cmd.none)
          UpdateColorScale c ->
              ({ model | env = { env | colorScale = c } }, Cmd.none)
          _ ->
              (model, Cmd.none)
