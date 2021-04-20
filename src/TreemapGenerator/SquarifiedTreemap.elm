module TreemapGenerator.SquarifiedTreemap exposing (..)

{-| generate a Squarified Treemap
<https://www.win.tue.nl/~vanwijk/stm.pdf>
-}

import List.Nonempty as NE
import TreemapGenerator.Utils as Utils



--------------------------------------------------------------------------------


type alias Row a =
    NE.Nonempty (HasArea a)


type alias HasArea a =
    { a | area : Float }


type alias Dimensions =
    { x : Float
    , y : Float
    }


type alias Delta =
    { x : Float
    , y : Float
    }


sizeOrdered : Dimensions -> ( Float, Float )
sizeOrdered dims =
    let
        x =
            dims.x

        y =
            dims.y
    in
    if x < y then
        ( x, y )

    else
        ( y, x )



--------------------------------------------------------------------------------


type alias SquarifiedTreemap =
    NE.Nonempty Cell


type alias Cell =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , area : Float
    }


type alias Origin =
    { x : Float
    , y : Float
    }


makeTreemap : Dimensions -> NE.Nonempty (HasArea a) -> SquarifiedTreemap
makeTreemap dims areas =
    partition dims areas
        |> Utils.neMapAccumL rowToCells ( { x = 0, y = 0 }, dims )
        |> Tuple.first
        |> NE.concat


rowToCells : Row a -> ( Origin, Dimensions ) -> ( NE.Nonempty Cell, ( Origin, Dimensions ) )
rowToCells row ( origin, dims ) =
    let
        ( origin_, dims_ ) =
            updateOriginAndDims origin dims row

        delta =
            { x = dims.x - dims_.x
            , y = dims.y - dims_.y
            }

        cells =
            Utils.neMapAccumL rowToCellsHelper ( origin, delta ) row |> Tuple.first
    in
    ( cells, ( origin_, dims_ ) )


rowToCellsHelper : HasArea a -> ( Origin, Delta ) -> ( Cell, ( Origin, Dimensions ) )
rowToCellsHelper area ( origin, delta ) =
    let
        ( ( w, h ), origin_ ) =
            if delta.x > 0 then
                ( ( delta.x, area.area / delta.x )
                , { origin | y = origin.y + area.area / delta.x }
                )

            else
                ( ( area.area / delta.y, delta.y )
                , { origin | x = origin.x + area.area / delta.y }
                )
    in
    ( { x = origin.x
      , y = origin.y
      , w = w
      , h = h
      , area = area.area
      }
    , ( origin_, delta )
    )


updateOriginAndDims : Origin -> Dimensions -> Row a -> ( Origin, Dimensions )
updateOriginAndDims origin dims row =
    let
        dims_ =
            updateDims dims row

        origin_ =
            { x = origin.x + dims.x - dims_.x
            , y = origin.y + dims.y - dims_.y
            }
    in
    ( origin_, dims_ )



--------------------------------------------------------------------------------


partition : Dimensions -> NE.Nonempty (HasArea a) -> NE.Nonempty (Row a)
partition dims areas =
    NE.tail areas
        |> List.foldl partitionHelper ( dims, NE.fromElement (NE.head areas), [] )
        |> (\( _, row, rows ) ->
                NE.Nonempty row rows
                    |> NE.map NE.reverse
                    |> NE.reverse
           )


type alias PartitionAcc a =
    ( Dimensions, Row a, List (Row a) )


partitionHelper : HasArea a -> PartitionAcc a -> PartitionAcc a
partitionHelper area ( dims, row, rows ) =
    let
        w =
            Tuple.first <| sizeOrdered dims
    in
    if worst row w >= worst (NE.cons area row) w then
        ( dims, NE.cons area row, rows )

    else
        ( updateDims dims row, NE.fromElement area, row :: rows )


updateDims : Dimensions -> Row a -> Dimensions
updateDims dims row =
    let
        ( s, l ) =
            sizeOrdered dims

        l_ =
            l - totalArea row / s
    in
    if s == dims.x then
        { x = s, y = l_ }

    else
        { x = l_, y = s }


worst : Row a -> Float -> Float
worst row w =
    let
        areas =
            NE.map .area row

        rowMax =
            NE.foldl1 max areas

        rowMin =
            NE.foldl1 min areas

        rowTotal =
            totalArea row
    in
    max (w ^ 2 * rowMax / rowTotal ^ 2) (rowTotal ^ 2 / (w ^ 2 * rowMin))



--------------------------------------------------------------------------------
-- Helpers


totalArea : Row a -> Float
totalArea =
    NE.map .area >> NE.foldl1 (+)
