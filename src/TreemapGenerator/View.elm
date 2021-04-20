module TreemapGenerator.View exposing (view)

{-| View / render treemap generator with controls.
-}


import Color exposing (Color)
import Element as E
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List.Extra as LE
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


view : Model -> Html Msg
view model =

    E.layout
         [ Font.family [ Font.typeface "Consolas", Font.sansSerif ]
         , Font.size 24
         , E.padding 10
         ]
         ( E.column
               [ E.centerX
               , E.alignTop
               , E.height <| E.px <| round model.env.windowH
               , E.width <| E.px <| round model.env.windowW
               , E.spacing 0
               ]
               [ controls model
               ,  E.el [ E.alignTop
                       , E.centerX
                       , E.width <| E.px <| round model.env.w
                     , E.height <| E.px <| round model.env.h
                     ]
                     (render model |> E.html)
               ]
         )


controls : Model -> E.Element Msg
controls model =
    E.row
        [ E.alignTop
        , E.centerX
        , E.spacing 25
        ]
        [ sortOrderChoice UpdateGroupSortOrder "Group " model.env.groupSortOrder
        , sortOrderChoice UpdateCellSortOrder "Cell " model.env.groupSortOrder
        , colorScaleChoice model.env.colorScale
        ] 

sortOrderChoice : (SortOrder -> Msg) -> String -> SortOrder -> E.Element Msg
sortOrderChoice cmd title selected =
    Input.radioRow
    [ E.padding 10
    , E.spacing 10
    ]
    { onChange = cmd
    , selected = Just selected
    , label = Input.labelAbove [] (E.text <| title ++ "Sort Order")
    , options =
        [ Input.option Ascending (E.text "Ascending")
        , Input.option Descending (E.text "Descending")
        , Input.option Random (E.text "Random")
        ]
    }

colorScaleChoice: ColorScale -> E.Element Msg
colorScaleChoice selected =
    Input.radioRow
    [ E.padding 10
    , E.spacing 10
    ]
    { onChange = UpdateColorScale
    , selected = Just selected
    , label = Input.labelAbove [] (E.text "Color Scale")
    , options =
        [ Input.option RedGreen (E.text "Red Green")
        , Input.option BlackWhite (E.text "Black White")
        , Input.option TenColors (E.text "Ten Colors")
        , Input.option TenMoreColors (E.text "Ten More Colors")
        ]
    }


--------------------------------------------------------------------------------
-- Render


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
            NE.map2 (genSubtree model.env) groups groupCells
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
        sortByArea env.groupSortOrder groups
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


genSubtree : Env -> Group -> ST.Cell -> Subtree
genSubtree env group groupCell =
    let
        dims =
            { x = groupCell.w, y = groupCell.h }

        areaScalar =
            group.area / sumWeights group.series

        treeCells =
            NE.map (genTreeCell areaScalar) group.series

        treemap =
            sortByArea env.cellSortOrder treeCells
                |> ST.makeTreemap dims
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
        RedGreen ->
            getRedGreen val
        BlackWhite ->
            getBlackWhite val
        TenColors ->
            getTenColors CScale.category10 val
        TenMoreColors ->
            getTenColors CScale.tableau10 val


getRedGreen : Float -> Color
getRedGreen f =
    if f >= 0 then
        CScale.viridisInterpolator (1 - f)

    else
        CScale.plasmaInterpolator (1 - abs f)


getBlackWhite : Float -> Color
getBlackWhite f =
    let
        f_ =
            abs f

        x =
            if f >= 0 then
                min 0.5 (f / 2)
            else
                max -0.5 (f / 2)

        (epsilonR, epsilonG, epsilonB) =
            if f_ >= 0.66 then
                (min 0.10 ((f_ - 0.66) / 0.11), 0, 0)
            else if f_ > 0.33 then
                 (0, (f - 0.33) / 0.11, 0)
            else
                (0, 0, (f - 0.33) / 0.11)
        alpha =
            max 0.2 (abs f)

        c =
            0.5 + x
    in 
        Color.rgba (c + epsilonR) (c + epsilonG)  (c + epsilonB) alpha


getTenColors : List Color -> Float -> Color
getTenColors colors f =
    let
        f_ =
            min 1.0 (abs f)

        index =
            min 9 (round <| f_ * 10)

        c =
            LE.getAt index colors
                |> Maybe.withDefault (Color.rgb 0 0 0)
                |> Color.toRgba

        alpha =
            0.75 + f_ * 0.25

    in
        Color.rgba c.red c.green c.blue alpha

------------
-- Helpers

type alias HasArea a =
    { a | area : Float } 

sortByArea : SortOrder -> NE.Nonempty (HasArea a) -> NE.Nonempty (HasArea a)
sortByArea sortOrder xs =
    let sort dir =
            NE.sortBy (.area >> (*) dir) xs
    in 
    case sortOrder of
        Ascending ->
            sort 1

        Descending ->
            sort (-1)

        Random ->
            xs

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
