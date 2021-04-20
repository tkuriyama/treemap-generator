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


main : Flags -> Program flags Model Msg
main flags =
    Browser.element
        { init = init flags
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

        model = 
            { windowW = flags.windowWidth
            , windowH = flags.windowHeight
            , x = 1800
            , y = 1200
            , data = data
            , groupCt = NE.length data
            , nodeCt = NE.map (NE.length) data |> NE.foldl1 (+)
            , borderWidth = 0.25
            , colorScale = RedGreen
            , sortOrder = Descending
            }
    in
        (model, Cmd.none)

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

update : Msg -> Model -> (Model, Msg)
update msg model =
    (model, msg)
