module Display exposing (info)

import Element exposing (..)
import Element.Font as Font
import Stat exposing (Data, Point)


info : String -> Maybe String -> (Point -> Float) -> Data -> Element msg
info defaultLabel maybeLabel selector data =
    column [ spacing 5 ]
        [ el [ Font.bold ] (text <| label defaultLabel maybeLabel)
        , el []
            (text <| displayAverage selector data)
        , el []
            (text <| displayMinimum selector data)
        , el []
            (text <| displayMaximum selector data)
        ]


label : String -> Maybe String -> String
label defaultLabel maybeLabel =
    case maybeLabel of
        Nothing ->
            defaultLabel

        Just str ->
            if str == "" then
                defaultLabel

            else
                str


displayMinimum : (Point -> Float) -> Data -> String
displayMinimum selector data =
    case Stat.minimum selector data of
        Nothing ->
            "min: ?"

        Just value ->
            "min: " ++ String.left 6 (String.fromFloat value)


displayMaximum : (Point -> Float) -> Data -> String
displayMaximum selector data =
    case Stat.maximum selector data of
        Nothing ->
            "max: ?"

        Just value ->
            "max: " ++ String.left 6 (String.fromFloat value)


displayAverage : (Point -> Float) -> Data -> String
displayAverage selector data =
    case List.length (List.map selector data) of
        0 ->
            "average: ?"

        _ ->
            "average: " ++ String.left 6 (String.fromFloat (Stat.average selector data))
