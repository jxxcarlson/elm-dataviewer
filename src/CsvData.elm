module CsvData exposing (get, toPointList)

import Csv exposing (Csv)
import Maybe.Extra
import Stat exposing (Data, Point)


get : String -> Maybe Csv
get str =
    Just <| Csv.parse ("x,y\n" ++ str)


xValuesFromCsv : Csv -> List Float
xValuesFromCsv csv =
    let
        xValuesAsString =
            List.map List.head csv.records |> Maybe.Extra.values
    in
    List.map String.toFloat xValuesAsString |> Maybe.Extra.values


yValuesFromCsv : Csv -> List Float
yValuesFromCsv csv =
    let
        yValuesAsString =
            List.map (List.head << List.drop 1) csv.records |> Maybe.Extra.values
    in
    List.map String.toFloat yValuesAsString |> Maybe.Extra.values


toPointList : Csv -> Data
toPointList csv =
    let
        xs =
            xValuesFromCsv csv

        ys =
            yValuesFromCsv csv
    in
    List.map2 Point xs ys
