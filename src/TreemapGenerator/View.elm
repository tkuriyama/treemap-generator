module TreemapGenerator.View exposing (view)

{-| View / render treemap generator with controls.
-}

import Color exposing (Color)
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List.Extra as LE
import List.Nonempty as NE
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Scale.Color as CScale
import String.Format
import TreemapGenerator.SquarifiedTreemap as ST
import TreemapGenerator.Types exposing (..)
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes
    exposing
        ( class
        , fill
        , stroke
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
        , Font.size 18
        , E.padding 10
        ]
        (E.column
            [ E.centerX
            , E.alignTop
            , E.width <|
                E.maximum (round model.env.windowW) E.fill
            , E.height <|
                E.maximum (round model.env.windowH) E.fill
            , E.spacing 0
            ]
            [ controls model
            , E.el
                [ E.alignTop
                , E.centerX
                , E.width <| E.px <| round model.env.w
                , E.height <| E.px <| round model.env.h
                ]
                (render model |> E.html)
            ]
        )


controls : Model -> E.Element Msg
controls model =
    let
        rowAttrs =
            [ E.alignTop
            , E.centerX
            , E.spacing 30
            , E.padding 5
            ]
    in
    E.column
        [ E.alignTop
        , E.centerX
        , E.spacing 10
        ]
        [ E.row
            rowAttrs
            [ sortOrderChoice UpdateGroupSortOrder "Group " model.env.groupSortOrder
            , sortOrderChoice UpdateCellSortOrder "Cell " model.env.groupSortOrder
            , colorScaleChoice model.env.colorScale
            ]
        , E.row
            rowAttrs
            [ dimensionSlider
                UpdateWidth
                model.env.windowW
                "Treemap Width"
                model.env.w
            , dimensionSlider
                UpdateHeight
                model.env.windowH
                "Treemap Height"
                model.env.h
            , borderSlider
                UpdateGroupBorderWidth
                "Group Border Size"
                model.env.groupBorderWidth
            , borderSlider
                UpdateCellBorderWidth
                "Cell Border Size"
                model.env.cellBorderWidth
            ]
        ]


dimensionSlider : (Float -> Msg) -> Float -> String -> Float -> E.Element Msg
dimensionSlider msg max_ title currentVal =
    Input.slider
        [ E.height <| E.px 30
        , E.behindContent sliderElement
        ]
        { onChange = msg
        , label = titleLabel title
        , min = max_ * 0.5
        , max = max_
        , step = Just 10
        , value = currentVal
        , thumb =
            Input.defaultThumb
        }


sortOrderChoice : (SortOrder -> Msg) -> String -> SortOrder -> E.Element Msg
sortOrderChoice cmd title selected =
    Input.radioRow
        [ E.padding 10
        , E.spacing 10
        ]
        { onChange = cmd
        , selected = Just selected
        , label = titleLabel <| title ++ "Sort Order"
        , options =
            [ Input.option Ascending (E.text "Ascend")
            , Input.option Descending (E.text "Descend")
            , Input.option Random (E.text "Random")
            ]
        }


colorScaleChoice : ColorScale -> E.Element Msg
colorScaleChoice selected =
    Input.radioRow
        [ E.padding 10
        , E.spacing 10
        ]
        { onChange = UpdateColorScale
        , selected = Just selected
        , label = titleLabel "Color Scale"
        , options =
            [ Input.option RedGreen (E.text "R & G")
            , Input.option BlackWhite (E.text "B & W")
            , Input.option TenColors (E.text "Ten")
            , Input.option TenMoreColors (E.text "Ten More")
            ]
        }


borderSlider : (Float -> Msg) -> String -> Float -> E.Element Msg
borderSlider msg title currentVal =
    Input.slider
        [ E.height <| E.px 30
        , E.behindContent sliderElement
        ]
        { onChange = msg
        , label = titleLabel title
        , min = 0
        , max = 4
        , step = Just 0.25
        , value = currentVal
        , thumb =
            Input.defaultThumb
        }


titleLabel : String -> Input.Label msg
titleLabel s =
    Input.labelAbove
        [ Font.heavy
        , E.centerX
        ]
        (E.text s)


sliderElement : E.Element msg
sliderElement =
    E.el
        [ E.width E.fill
        , E.height <| E.px 10
        , E.centerY
        , Background.color <| E.rgb255 66 135 245
        , Border.rounded 2
        ]
        E.none



--------------------------------------------------------------------------------
-- Render


render : Model -> Svg msg
render model =
    let
        ( env, data ) =
            ( model.env, model.data )

        groups =
            genGroups model

        groupCells =
            ST.makeTreemap { x = env.w, y = env.h } groups

        subtrees =
            NE.map2 (genSubtree model.env) groups groupCells
    in
    svg [ viewBox 0 0 env.w env.h ]
        [ g
            [ class [ "tree_cell" ]
            ]
            (NE.map (renderSubtree env) subtrees |> NE.toList)
        , g
            [ class [ "tree_group" ]
            ]
            (NE.map (renderGroupCell env) groupCells |> NE.toList)
        ]



--------------------------------------------------------------------------------
-- Groups


renderGroupCell : Env -> ST.Cell -> Svg msg
renderGroupCell env cell =
    rect
        [ x cell.x
        , y cell.y
        , width cell.w
        , height cell.h
        , stroke <| Paint <| Color.rgb255 160 160 160
        , strokeWidth env.groupBorderWidth
        , fill <| PaintNone
        ]
        []


genGroups : Model -> NE.Nonempty Group
genGroups model =
    let
        ( env, data ) =
            ( model.env, model.data )

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
genTreeCell areaScalar ( weight, value ) =
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
        , stroke <| Paint <| Color.rgb255 120 120 120
        , strokeWidth env.cellBorderWidth
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

        ( epsilonR, epsilonG, epsilonB ) =
            if f_ >= 0.66 then
                ( min 0.1 ((f_ - 0.66) / 0.11), 0, 0 )

            else if f_ > 0.33 then
                ( 0, (f - 0.33) / 0.11, 0 )

            else
                ( 0, 0, (f - 0.33) / 0.11 )

        alpha =
            max 0.2 (abs f)

        c =
            0.5 + x
    in
    Color.rgba (c + epsilonR) (c + epsilonG) (c + epsilonB) alpha


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
    let
        sort dir =
            NE.sortBy (.area >> (*) dir) xs
    in
    case sortOrder of
        Ascending ->
            sort 1

        Descending ->
            sort -1

        Random ->
            xs
