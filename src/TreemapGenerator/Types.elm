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
    { windowW : Float
    , windowH : Float
    , x : Float
    , y : Float
    , data : Data
    , groupCt : Int
    , nodeCt : Int
    , borderWidth : Float
    , colorScale : ColorScale
    , sortOrder : SortOrder
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
    | UpdateNodeCt Int
    | UpdateBorderWidth Float
    | UpdateColorScale ColorScale
    | UpdateSortOrder SortOrder


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
