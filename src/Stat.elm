module Stat exposing (Data, Point, average, maximum, minimum)


type alias Point =
    { x : Float
    , y : Float
    }


type alias Data =
    List Point


average : (data -> Float) -> List data -> Float
average selector dataList =
    let
        values =
            List.map selector dataList

        sum =
            List.sum values

        n =
            toFloat (List.length values)
    in
    sum / n


minimum : (data -> Float) -> List data -> Maybe Float
minimum selector dataList =
    List.minimum (List.map selector dataList)


maximum : (data -> Float) -> List data -> Maybe Float
maximum selector dataList =
    List.maximum (List.map selector dataList)
