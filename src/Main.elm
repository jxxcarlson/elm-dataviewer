module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Csv exposing (Csv)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes as HA
import LineChart
import LineChart.Colors as Colors
import LineChart.Dots as Dots
import Maybe.Extra
import Svg exposing (Svg)
import Task


type alias Point =
    { x : Float
    , y : Float
    }


type alias Data =
    List Point


type ViewMode
    = RawDataView
    | CsvView


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { filename : String
    , csvText : Maybe String
    , csvData : Maybe Csv
    , data : Data
    , xLabel : Maybe String
    , yLabel : Maybe String
    , viewMode : ViewMode
    , output : String
    }


type Msg
    = NoOp
    | InputXLabel String
    | InputYLabel String
    | CsvRequested
    | CsvSelected File
    | CsvLoaded String


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { filename = "no file yet"
      , csvText = Nothing
      , csvData = Nothing
      , data = []
      , viewMode = RawDataView
      , xLabel = Nothing
      , yLabel = Nothing
      , output = "Ready!"
      }
    , Cmd.none
    )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputXLabel str ->
            ( { model | xLabel = Just str }, Cmd.none )

        InputYLabel str ->
            ( { model | yLabel = Just str }, Cmd.none )

        CsvRequested ->
            ( model
            , Select.file [ "text/csvText" ] CsvSelected
            )

        CsvSelected file ->
            ( model
            , Task.perform CsvLoaded (File.toString file)
            )

        CsvLoaded content ->
            let
                csvData =
                    getCsvData content

                numericalData =
                    case csvData of
                        Nothing ->
                            []

                        Just data ->
                            numericalDataFromCsv data
            in
            ( { model | csvText = Just content, csvData = csvData, data = numericalData }
            , Cmd.none
            )


xAverage : Data -> Float
xAverage data =
    let
        n =
            List.length (List.map .x data)

        sum =
            List.sum (List.map .x data)
    in
    sum / toFloat n


yAverage : Data -> Float
yAverage data =
    let
        n =
            List.length (List.map .y data)

        sum =
            List.sum (List.map .y data)
    in
    sum / toFloat n


getCsvData : String -> Maybe Csv
getCsvData str =
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


numericalDataFromCsv : Csv -> Data
numericalDataFromCsv csv =
    let
        xs =
            xValuesFromCsv csv

        ys =
            yValuesFromCsv csv
    in
    List.map2 Point xs ys



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout outerStyle
        (row [ spacing 24, alignTop ]
            [ mainColumn model
            , dataInfoPanel model
            , visualDataDisplay model
            ]
        )


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 20 ]
            [ column [ spacing 8 ] [ title "Data Explorer", openFileButton ]
            , column
                [ spacing 8 ]
                [ inputXLabel model, inputYLabel model ]
            , dataDisplay model
            ]
        ]


visualDataDisplay : Model -> Element msg
visualDataDisplay model =
    row
        [ Font.size 12
        , width (px 800)
        , height (px 600)
        ]
        [ Element.html (chart model) ]


footer : Model -> Element msg
footer model =
    row [ spacing 18, Font.size 12 ]
        [ el []
            (text <| numberOfRecordsString model.csvData)
        , el []
            (text <| viewModeAsString model.viewMode)
        , el []
            (text <| displayXAverage model.data)
        , el []
            (text <| displayYAverage model.data)
        ]


chart1 : Model -> Svg msg
chart1 model =
    LineChart.view1 .x .y model.data


chart : Model -> Svg msg
chart model =
    LineChart.view .x .y [ LineChart.line Colors.red Dots.none "" model.data ]


dataInfoPanel : Model -> Element msg
dataInfoPanel model =
    column
        [ spacing 12
        , Font.size 12
        , Background.color (rgb255 245 245 245)
        , width (px 200)
        , height (px 510)
        , paddingXY 8 12
        , moveDown 40
        ]
        [ el []
            (text <| numberOfRecordsString model.csvData)
        , el []
            (text <| viewModeAsString model.viewMode)
        , xDisplay model
        , yDisplay model
        ]


xDisplay : Model -> Element msg
xDisplay model =
    column [ spacing 5 ]
        [ el [ Font.bold ] (text <| xLabel model)
        , el []
            (text <| displayXAverage model.data)
        , el []
            (text <| displayXMinimum model.data)
        , el []
            (text <| displayXMaximum model.data)
        ]


xLabel : Model -> String
xLabel model =
    case model.xLabel of
        Nothing ->
            "x"

        Just str ->
            if str == "" then
                "x"

            else
                str


yLabel : Model -> String
yLabel model =
    case model.yLabel of
        Nothing ->
            "y"

        Just str ->
            if str == "" then
                "x"

            else
                str


yDisplay : Model -> Element msg
yDisplay model =
    column [ spacing 5 ]
        [ el [ Font.bold ] (text <| yLabel model)
        , el []
            (text <| displayYAverage model.data)
        , el []
            (text <| displayYMinimum model.data)
        , el []
            (text <| displayYMaximum model.data)
        ]


displayXMinimum : Data -> String
displayXMinimum data =
    case xMinimum data of
        Nothing ->
            "min: ?"

        Just x ->
            "min: " ++ String.left 6 (String.fromFloat x)


displayXMaximum : Data -> String
displayXMaximum data =
    case xMaximum data of
        Nothing ->
            "max: ?"

        Just x ->
            "max: " ++ String.left 6 (String.fromFloat x)


displayYMinimum : Data -> String
displayYMinimum data =
    case yMinimum data of
        Nothing ->
            "min: ?"

        Just y ->
            "min: " ++ String.left 6 (String.fromFloat y)


displayYMaximum : Data -> String
displayYMaximum data =
    case yMaximum data of
        Nothing ->
            "max: ?"

        Just y ->
            "max: " ++ String.left 6 (String.fromFloat y)


xMinimum : Data -> Maybe Float
xMinimum data =
    List.minimum (List.map .x data)


xMaximum : Data -> Maybe Float
xMaximum data =
    List.maximum (List.map .x data)


yMinimum : Data -> Maybe Float
yMinimum data =
    List.minimum (List.map .y data)


yMaximum : Data -> Maybe Float
yMaximum data =
    List.maximum (List.map .y data)


displayXAverage : Data -> String
displayXAverage data =
    case List.length (List.map .x data) of
        0 ->
            "average: ?"

        _ ->
            "average: " ++ String.left 6 (String.fromFloat (xAverage data))


displayYAverage : Data -> String
displayYAverage data =
    case List.length (List.map .y data) of
        0 ->
            "average: ?"

        _ ->
            "average: " ++ String.left 6 (String.fromFloat (yAverage data))


viewModeAsString : ViewMode -> String
viewModeAsString viewMode =
    case viewMode of
        RawDataView ->
            "View: Raw data"

        CsvView ->
            "View: Csv"


numberOfRecordsString : Maybe Csv -> String
numberOfRecordsString maybeCsvData =
    case maybeCsvData of
        Nothing ->
            "No records yet"

        Just data ->
            "Records: " ++ String.fromInt (List.length data.records)


title : String -> Element msg
title str =
    row [ centerX, Font.bold ] [ text str ]


dataDisplay : Model -> Element msg
dataDisplay model =
    let
        content =
            case model.csvText of
                Nothing ->
                    "No data yet"

                Just data ->
                    data
    in
    row
        [ Background.color (rgb255 245 245 245)
        , width <| px 200
        , height <| px 450
        , scrollbarY
        , Font.size 12
        , paddingXY 8 12
        , alignTop
        ]
        [ text content ]


inputXLabel : Model -> Element Msg
inputXLabel model =
    let
        labelText =
            case model.xLabel of
                Nothing ->
                    "Label for x values"

                Just str ->
                    str
    in
    Input.text [ height (px 18), Font.size 12, paddingXY 8 0 ]
        { onChange = InputXLabel
        , text = labelText
        , placeholder = Nothing
        , label = Input.labelLeft [ moveDown 4 ] <| el [] (text "X:")
        }


inputYLabel : Model -> Element Msg
inputYLabel model =
    let
        labelText =
            case model.yLabel of
                Nothing ->
                    "Label for y values"

                Just str ->
                    str
    in
    Input.text [ height (px 18), Font.size 12, paddingXY 8 0, width (px 185) ]
        { onChange = InputYLabel
        , text = labelText
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [ moveDown 4 ] (text "Y:")
        }


openFileButton : Element Msg
openFileButton =
    row [ centerX ]
        [ Input.button buttonStyle
            { onPress = Just CsvRequested
            , label = el [] (text "Open CSV file")
            }
        ]



--
-- STYLE
--


outerStyle =
    [ Background.color (rgb255 180 180 180)
    , paddingXY 20 20
    , height fill
    , width fill
    ]


mainColumnStyle =
    [ Background.color (rgb255 180 180 180)
    , paddingXY 20 20
    , height fill
    , width fill
    ]


buttonStyle =
    [ Background.color (rgb255 40 40 40)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    , Font.size 14
    ]
