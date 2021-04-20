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
               [ E.alignTop
               , E.height model.windowHeight
               , E.width model.windowWidth
               , E.spacing 5
               ]
               [ controls model
               ,  render model |> E.html
               ]
         )


controls : Model -> E.Element msg
controls model =
    E.el [] []


--------------------------------------------------------------------------------

render : Model -> Svg msg
render model =
    let
        dims =
            { x = env.w - env.pad.left - env.pad.right
            , y = env.h - env.pad.top - env.pad.bottom
            }

        groups =
            genGroups (dims.x * dims.y) data

        groupCells =
            ST.makeTreemap dims groups

        subtrees =
            NE.map2 genSubtree groups groupCells
    in
    svg [ viewBox 0 0 env.w env.h ]
        [ style [] [ text <| env.style ]
        , g
            [ class [ "tree_cell" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            (NE.map (renderSubtree env) subtrees |> NE.toList)
        , g
            [ class [ "tree_group" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            (NE.map (renderGroupCell env) groupCells |> NE.toList)
        ]



--------------------------------------------------------------------------------
-- Groups


renderGroupCell : ChartEnv -> ST.Cell -> Svg msg
renderGroupCell env cell =
    rect
        [ x cell.x
        , y cell.y
        , width cell.w
        , height cell.h
        ]
        []


genGroups : Float -> NE.Nonempty (Cfg.GridSeries Cfg.GridTriple) -> NE.Nonempty Group
genGroups area data =
    let
        groups =
            NE.map genGroup data

        totalWeight =
            NE.map .area groups |> NE.foldl1 (+)

        scalar =
            area / totalWeight
    in
    NE.sortBy (.area >> (*) -1) groups
        |> NE.map (\s -> { s | area = s.area * scalar })


genGroup : Cfg.GridSeries Cfg.GridTriple -> Group
genGroup ( groupName, pairs ) =
    let
        treeSeries =
            genTreeSeries pairs

        area =
            sumWeights treeSeries
    in
    { name = groupName
    , area = area
    , series = treeSeries
    }


genTreeSeries : List ( String, List Cfg.GridTriple ) -> NE.Nonempty TreeTriple
genTreeSeries pairs =
    let
        default =
            ( "", 0, 0 )

        f ( name, triples ) =
            case triples of
                x :: y :: [] ->
                    ( name, x, y )

                _ ->
                    ( name, default, default )
    in
    case pairs of
        [] ->
            NE.fromElement ( "", default, default )

        x :: xs ->
            NE.Nonempty (f x) (List.map f xs)
                |> NE.sortBy (\( _, _, ( _, w, _ ) ) -> w * -1)


sumWeights : NE.Nonempty TreeTriple -> Float
sumWeights =
    NE.map (\( _, _, ( _, w, _ ) ) -> w) >> NE.foldl1 (+)



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
            NE.map (genTreeCell group.name areaScalar) group.series

        treemap =
            ST.makeTreemap dims treeCells
    in
    ( groupCell, treeCells, treemap )


genTreeCell : String -> Float -> TreeTriple -> TreeCell
genTreeCell groupName areaScalar triple =
    let
        ( cellName, ( pLbl, pWeight, pVal ), ( cLbl, cWeight, cVal ) ) =
            triple
    in
    { groupName = groupName
    , cellName = cellName
    , previousLabel = pLbl
    , previousValue = pVal
    , currentLabel = cLbl
    , currentValue = cVal
    , area = cWeight * areaScalar
    }


renderSubtree : ChartEnv -> Subtree -> Svg msg
renderSubtree env ( groupCell, treeCells, treemap ) =
    g
        [ transform [ Translate groupCell.x groupCell.y ]
        ]
        (NE.map2 (renderTreeCell env) treeCells treemap
            |> NE.toList
        )


renderTreeCell : ChartEnv -> TreeCell -> ST.Cell -> Svg msg
renderTreeCell env t cell =
    let
        length =
            String.length t.cellName * env.minFontSize |> toFloat |> (*) 0.7

        fontSz =
            min
                (toFloat env.baseFontSize)
                (cell.w / 0.7 / (String.length t.cellName |> toFloat))

        fits =
            length * 1.2 < cell.w && fontSz * 1.2 < cell.h

        colorVal =
            (t.currentValue - t.previousValue) / t.previousValue
    in
    g []
        ([ rect
            [ x cell.x
            , y cell.y
            , width cell.w
            , height cell.h
            , rx 1
            , fill <| Paint <| GridChart.getColor (colorVal * 10)
            ]
            []
         ]
            ++ (if fits then
                    [ text_
                        [ x <| cell.x + toFloat env.innerPad
                        , y <| cell.y + toFloat env.innerPad + fontSz
                        , fontSize fontSz
                        ]
                        [ text t.cellName ]
                    ]

                else
                    []
               )
        )

--------------------------------------------------------------------------------
-- Scales

getRedGreen : Float -> Color
getRedGreen f =
    if f >= 0 then
        CScale.viridisInterpolator (1 - f)

    else
        CScale.plasmaInterpolator (1 - abs f)


--------------------------------------------------------------------------------
-- Style


genStyle : Cfg.GridChartCfg -> Int -> String
genStyle cfg sz =
    let
        ( fCfg, tCfg ) =
            ( cfg.fontSpec, cfg.tooltips )
    in
    """
     text { font-family: {{typeface}}, monospace, sans-serif;
            fill: {{textColor}}; }
     .tree_group rect { display: inline; fill: none;
                        stroke: rgb(120, 120, 120); stroke-width: 1.5px; }
     .tree_cell rect { display: inline;
                       stroke: rgb(160, 160, 160); stroke-width: 0.5px; }
     .tree_cell text { opacity: 0.85; }
     .transparent { opacity : 0.0; }
     .tree_tooltip_hover { display: none; font-size: {{szH}}px;}
     .tree_tooltip_hover rect { fill: rgba(250, 250, 250, 1.0); }
     .tree_tooltip_area:hover .tree_tooltip_hover { display: inline; }
     """
        |> String.Format.namedValue "sz" (String.fromInt sz)
        |> String.Format.namedValue "textColor" fCfg.textColor
        |> String.Format.namedValue "szH" (String.fromInt tCfg.hoverTooltipSize)
        |> String.Format.namedValue "typeface" fCfg.typeface
        |> String.Format.namedValue "showHover"
            (UI.display tCfg.showHoverTooltips)
