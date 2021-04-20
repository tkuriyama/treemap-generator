module TreemapGenerator.View exposing (view)

{-| View / render treemap generator with controls.
-}


import Color exposing (Color)
import Element as E
import Element.Font as Font
import Html exposing (Html)
import List.Nonempty as NE
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Scale.Color as CScale
import String.Format
import TreemapGenerator.Types exposing (..)
import TreemapGenerator.SquarifiedTreemap as ST
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes
    exposing
        ( class
        , fill
        , transform
        , viewBox
        )
import TypedSvg.Attributes.InPx
    exposing
        ( fontSize
        , height
        , rx
        , strokeWidth
        , width
        , x
        , y
        )
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Paint(..), Transform(..))


--------------------------------------------------------------------------------
-- View


view : Model -> Html msg
view model =

    E.layout
         [ Font.family [ Font.typeface "Consolas", Font.sansSerif ]
         , E.padding 20
         ]
         ( E.column
               [ E.centerX
               , E.alignTop
               , E.height <| E.px <| round model.env.windowH
               , E.width <| E.px <| round model.env.windowW
               , E.spacing 10
               ]
               [ controls model
               ,  E.el [ E.centerX
                       , E.width <| E.px <| round model.env.w
                     , E.height <| E.px <| round model.env.h
                     ]
                     (render model |> E.html)
               ]
         )


controls : Model -> E.Element msg
controls model =
    E.el [] E.none


render : Model -> Svg msg
render model =
    let
        (env, data) =
            (model.env, model.data)

        groups =
            genGroups model

        groupCells =
            ST.makeTreemap { x = env.w, y = env.h }  groups

        subtrees =
            NE.map2 genSubtree groups groupCells
    in
    svg [ viewBox 0 0 env.w env.h ]
        [ style [] [ text <| genStyle env ]
        , g
            [ class [ "tree_cell" ]
            ]
            (NE.map (renderSubtree env) subtrees |> NE.toList)
        , g
            [ class [ "tree_group" ]
            ]
            (NE.map renderGroupCell groupCells |> NE.toList)
        ]



--------------------------------------------------------------------------------
-- Groups


renderGroupCell : ST.Cell -> Svg msg
renderGroupCell cell =
    rect
        [ x cell.x
        , y cell.y
        , width cell.w
        , height cell.h
        ]
        []


genGroups : Model -> NE.Nonempty Group
genGroups model =
    let
        (env, data) =
            (model.env, model.data)

        area =
            env.w * env.h

        groups =
            NE.map genGroup data

        totalWeight =
            NE.map .area groups |> NE.foldl1 (+)

        scalar =
            area / totalWeight
    in
    NE.sortBy (.area >> (*) -1) groups
        |> NE.map (\s -> { s | area = s.area * scalar })


genGroup : NE.Nonempty Pair -> Group
genGroup pairs =
    { area = sumWeights pairs
    , series = pairs
    }



sumWeights : NE.Nonempty Pair -> Float
sumWeights =
    NE.map Tuple.first >> NE.foldl1 (+)



--------------------------------------------------------------------------------
-- Subtrees


genSubtree : Group -> ST.Cell -> Subtree
genSubtree group groupCell =
    let
        dims =
            { x = groupCell.w, y = groupCell.h }

        areaScalar =
            group.area / sumWeights group.series

        treeCells =
            NE.map (genTreeCell areaScalar) group.series

        treemap =
            ST.makeTreemap dims treeCells
    in
    ( groupCell, treeCells, treemap )


genTreeCell : Float -> Pair -> TreeCell
genTreeCell areaScalar (weight, value) =
    { weight = weight
    , value = value
    , area = weight * areaScalar
    }


renderSubtree : Env -> Subtree -> Svg msg
renderSubtree env ( groupCell, treeCells, treemap ) =
    g
        [ transform [ Translate groupCell.x groupCell.y ]
        ]
        (NE.map2 (renderTreeCell env) treeCells treemap
            |> NE.toList
        )


renderTreeCell : Env -> TreeCell -> ST.Cell -> Svg msg
renderTreeCell env t cell =
    rect
        [ x cell.x
        , y cell.y
        , width cell.w
        , height cell.h
        , rx 1
        , fill <| Paint <| getColor env.colorScale (t.value * 10)
        ]
        []

--------------------------------------------------------------------------------
-- Scales

getColor : ColorScale -> Float -> Color
getColor cScale val =
    case cScale of
        _ ->
            getRedGreen val


getRedGreen : Float -> Color
getRedGreen f =
    if f >= 0 then
        CScale.viridisInterpolator (1 - f)

    else
        CScale.plasmaInterpolator (1 - abs f)


--------------------------------------------------------------------------------
-- Style


genStyle : Env -> String
genStyle env =
    """
     .tree_group rect { display: inline; fill: none;
                        stroke: rgb(120, 120, 120); stroke-width: 1.5px; }
     .tree_cell rect { display: inline;
                       stroke: rgb(160, 160, 160); stroke-width: 0.5px; 
     """
