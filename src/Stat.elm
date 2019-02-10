module Stat exposing (Data, Point, average, maximum, minimum, stdev)


type alias Point =
    { x : Float
    , y : Float
    }


type alias Data =
    List Point


average : (data -> Float) -> List data -> Maybe Float
average selector dataList =
    let
        values =
            List.map selector dataList

        sum =
            List.sum values

        n =
            toFloat (List.length values)
    in
    case n > 0 of
        True ->
            Just <| sum / n

        False ->
            Nothing


stdev : (data -> Float) -> List data -> Maybe Float
stdev selector dataList =
    let
        n =
            List.length dataList
    in
    case n > 1 of
        False ->
            Nothing

        True ->
            let
                mean =
                    average selector dataList |> Maybe.withDefault 0

                square x =
                    x * x

                squaredDifferences =
                    List.map (\x -> square (x - mean)) (List.map selector dataList)
            in
            Just <| List.sum squaredDifferences / toFloat (n - 1)


minimum : (data -> Float) -> List data -> Maybe Float
minimum selector dataList =
    List.minimum (List.map selector dataList)


maximum : (data -> Float) -> List data -> Maybe Float
maximum selector dataList =
    List.maximum (List.map selector dataList)
