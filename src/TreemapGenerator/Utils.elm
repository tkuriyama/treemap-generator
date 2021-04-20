module TreemapGenerator.Utils exposing (..)

import List.Nonempty as NE
import MapAccumulate



--------------------------------------------------------------------------------
-- Non-Empty Lists


neMapAccumL :
    (a -> acc -> ( b, acc ))
    -> acc
    -> NE.Nonempty a
    -> ( NE.Nonempty b, acc )
neMapAccumL f z ne =
    case ne of
        NE.Nonempty x xs ->
            let
                ( y, z_ ) =
                    f x z

                ( ys, z__ ) =
                    MapAccumulate.mapAccumL f z_ xs
            in
            ( NE.Nonempty y ys, z__ )



--------------------------------------------------------------------------------
-- Statistics


mean : List Float -> Float
mean xs =
    List.sum xs / (toFloat <| List.length xs)



--------------------------------------------------------------------------------
-- Strings


nbsp : String
nbsp =
    String.fromChar (Char.fromCode 160)
