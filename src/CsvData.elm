module CsvData exposing (csv2, csv3, get, getColumn, getColumnAsFloats, getDataString, getHeader, toPointList)

import Csv exposing (Csv)
import List.Extra
import Maybe.Extra
import Stat exposing (Data, Point)


csv2 =
    "bla bla bla\nx,y\n0, 1\n1,2\n2,4"


csv3 =
    "x,y,z\n0, 1, 1\n1,2,4\n2,4,7"


get : String -> Maybe Csv
get str =
    Just <| Csv.parse str



-- ("x,y\n" ++ str)


getDataString : String -> String
getDataString str =
    str
        |> String.lines
        |> List.filter (\x -> String.contains "," x)
        |> String.join "\n"


getHeader : String -> String
getHeader str =
    str
        |> String.lines
        |> List.filter (\x -> not (String.contains "," x))
        |> String.join "\n"


getColumn : Int -> Csv -> List String
getColumn k csv =
    List.map (List.Extra.getAt k) csv.records |> Maybe.Extra.values


getColumnAsFloats : Int -> Csv -> List Float
getColumnAsFloats k csv =
    List.map (Maybe.andThen String.toFloat << List.Extra.getAt k) csv.records
        |> Maybe.Extra.values


toPointList : Int -> Int -> Csv -> Data
toPointList i j csv =
    let
        xs =
            getColumnAsFloats i csv

        ys =
            getColumnAsFloats j csv
    in
    List.map2 Point xs ys
