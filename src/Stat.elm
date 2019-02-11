module Stat exposing (Data, Point, Statistics, average, maximum, minimum, statistics, stdev)


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


type alias Statistics =
    { a : Float
    , b : Float
    , r2 : Float
    , xMin : Float
    , xMax : Float
    , leftDataPoint : Point
    , rightDataPoint : Point
    , regressionPoint : Point
    }


statistics : Data -> Maybe Statistics
statistics data =
    let
        m =
            List.length data
    in
    case m < 2 of
        True ->
            Nothing

        False ->
            let
                n =
                    toFloat m

                xs =
                    List.map .x data

                ys =
                    List.map .y data

                xMin =
                    minimum .x data |> Maybe.withDefault 0

                xMax =
                    maximum .x data |> Maybe.withDefault 0

                origin =
                    Point 0 0

                leftDataPoint =
                    data |> List.filter (\point -> .x point == xMin) |> List.head |> Maybe.withDefault origin

                rightDataPoint =
                    data |> List.filter (\point -> .x point == xMax) |> List.head |> Maybe.withDefault origin

                xSum =
                    List.sum xs

                xMean =
                    xSum / n

                ySum =
                    List.sum ys

                yMean =
                    ySum / n

                xsSquared =
                    List.sum (List.map (\x -> x * x) xs)

                xySum =
                    List.map2 (*) xs ys |> List.sum

                square x =
                    x * x

                ssTot =
                    List.sum (List.map (\y -> square (y - yMean)) ys)

                xDeltaSquaredSum =
                    xs |> List.map (\x -> square (x - xMean)) |> List.sum

                determinant =
                    n * xDeltaSquaredSum

                a =
                    (1 / determinant) * (n * xySum - xSum * ySum)

                b =
                    (1 / determinant) * (-xSum * xySum + xsSquared * ySum)

                fs =
                    List.map (\x -> a * x + b) xs

                ssRes =
                    List.sum (List.map2 (\f y -> square (f - y)) fs ys)

                r2 =
                    1 - ssRes / ssTot

                regressionPoint =
                    { x = rightDataPoint.x, y = a * rightDataPoint.x + b }
            in
            Just
                { xMax = xMax
                , xMin = xMin
                , a = a
                , b = b
                , r2 = r2
                , leftDataPoint = leftDataPoint
                , rightDataPoint = rightDataPoint
                , regressionPoint = regressionPoint
                }


minimum : (data -> Float) -> List data -> Maybe Float
minimum selector dataList =
    List.minimum (List.map selector dataList)


maximum : (data -> Float) -> List data -> Maybe Float
maximum selector dataList =
    List.maximum (List.map selector dataList)
