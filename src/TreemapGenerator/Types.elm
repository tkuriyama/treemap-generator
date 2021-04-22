module TreemapGenerator.Types exposing (..)

{-| Types for TreemapGenerator files.
-}

import List.Nonempty as NE
import TreemapGenerator.SquarifiedTreemap as ST



--------------------------------------------------------------------------------


type alias Flags =
    { windowWidth : Float
    , windowHeight : Float
    }


type alias Model =
    { env : Env
    , data : Data
    }


type alias Env =
    { windowW : Float
    , windowH : Float
    , w : Float
    , h : Float
    , groupCt : Int
    , cellCt : Int
    , groupBorderWidth : Float
    , cellBorderWidth : Float
    , colorScale : ColorScale
    , groupSortOrder : SortOrder
    , cellSortOrder : SortOrder
    }


type ColorScale
    = RedGreen
    | BlackWhite
    | TenColors
    | TenMoreColors


type SortOrder
    = Ascending
    | Descending
    | Random


type alias Data =
    NE.Nonempty (NE.Nonempty Pair)


type alias Pair =
    ( Float, Float )


type Msg
    = UpdateWidth Float
    | UpdateHeight Float
    | UpdateGroupCt Int
    | UpdateCellCt Int
    | UpdateGroupBorderWidth Float
    | UpdateCellBorderWidth Float
    | UpdateColorScale ColorScale
    | UpdateGroupSortOrder SortOrder
    | UpdateCellSortOrder SortOrder
    | WindowResize ( Int, Int )
    | Regroup Data
    | UpdateCells Data



--------------------------------------------------------------------------------
-- Squarified Treemap Helpers


type alias Group =
    { area : Float
    , series : NE.Nonempty Pair
    }


type alias Subtree =
    ( ST.Cell, NE.Nonempty TreeCell, ST.SquarifiedTreemap )


type alias TreeCell =
    { weight : Float
    , value : Float
    , area : Float
    }
