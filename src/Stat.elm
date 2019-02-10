module Stat exposing (Data, Point)


type alias Point =
    { x : Float
    , y : Float
    }


type alias Data =
    List Point
