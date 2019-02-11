module CsvData exposing (get, getColumn, getColumnAsFloats, intelligentGet, toPointList)

import Csv exposing (Csv)
import List.Extra
import Maybe.Extra
import Stat exposing (Data, Point)


type alias DataState =
    { headerStatus : HeaderStatus
    , sep : String
    , columns : Int
    }


type HeaderStatus
    = HeaderPresent
    | HeaderMissing
    | HeaderUndetermined


{-| Return a Csv value for the string after
filtering out obvious bad fields (no commaa)
-}
get : String -> Maybe Csv
get str =
    Just <| Csv.parse <| filter str


{-| Find the DataState of the string `str` using
a given separator, e.g., "," and attempt to Return
a pair `(Just csvData, Just headerString)`. To do this,
first find the `DataState` of the input string. If it is
Nothing, representing invalid data, return `(Nothing, Nothing)`.
Otherwise, return `(Just csvData, Just headerString)`
Notice that if the data consists of a single column, it
assumed to be a time series and so is augmented by prepending
a column `1,2,3, ...` using `makeSeries`.
-}
intelligentGet : String -> String -> ( Maybe Csv, Maybe String )
intelligentGet sep str =
    case dataState sep str of
        Nothing ->
            ( Nothing, Nothing )

        Just dataState_ ->
            if dataState_.columns == 1 then
                ( makeSeries str, Nothing )

            else
                ( get str, Just <| getHeader str )


makeSeries : String -> Maybe Csv
makeSeries str =
    let
        str2 =
            String.lines str
                |> List.indexedMap (\n x -> String.fromInt (n + 1) ++ "," ++ x)
                |> String.join "\n"
    in
    get <| "n,value\n" ++ str2


{-| Return Nothing if the string does not
meet the criteria to be a Csv file. Otherwise
return Just a record describing what kind
of Csv file it is.
-}
dataState : String -> String -> Maybe DataState
dataState sep str =
    let
        spectrum_ =
            spectrum sep str
    in
    case List.length spectrum_ of
        0 ->
            Nothing

        1 ->
            let
                columns =
                    (List.head spectrum_ |> Maybe.withDefault 0) + 1
            in
            case columns of
                1 ->
                    Just { headerStatus = HeaderUndetermined, sep = sep, columns = columns }

                _ ->
                    Just { headerStatus = HeaderMissing, sep = sep, columns = columns }

        2 ->
            let
                lo =
                    List.Extra.getAt 0 spectrum_ |> Maybe.withDefault 0

                columns =
                    (List.Extra.getAt 1 spectrum_ |> Maybe.withDefault 0) + 1
            in
            if lo == 0 && columns > 0 then
                Just { headerStatus = HeaderPresent, sep = sep, columns = columns }

            else
                Nothing

        _ ->
            Nothing


{-| Return the "comma spectrum" of a string.
Example:

> examine "," csv2
> [0,1] : List Int
> examine "," csv3
> [2] : List Int

A string whose spectrum has length 1 or 2
is "good". If it has length 1, all lines
have the same number of separators. If it
has length two, and has the form [0,n] where
n > 0, then it likely consists of a header
followed by good data with n+1 records per line.

-}
spectrum : String -> String -> List Int
spectrum sep str =
    str
        |> String.lines
        |> List.map (String.indices sep)
        |> List.map List.length
        |> List.Extra.unique
        |> List.sort


filter : String -> String
filter str =
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
