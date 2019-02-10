module Style exposing (button, mainColumn, outer)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font



--
-- STYLE
--


outer =
    [ Background.color (rgb255 180 180 180)
    , paddingXY 20 20
    , height fill
    , width fill
    ]


mainColumn =
    [ Background.color (rgb255 180 180 180)
    , paddingXY 20 20
    , height fill
    , width fill
    ]


button =
    [ Background.color (rgb255 40 40 40)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    , Font.size 14
    ]
