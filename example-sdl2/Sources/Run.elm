module Run exposing (initWindow, renderFilledRectangle)

import Color exposing (Color)


initWindow : { windowWidth : Int, windowHeight : Int, title : String }
initWindow =
    { windowWidth = 800
    , windowHeight = 600
    , title = "example-sdl2"
    }


type alias FilledRectangleToRender =
    { left : Float
    , top : Float
    , width : Float
    , height : Float
    , color : Color
    }


renderFilledRectangle : FilledRectangleToRender
renderFilledRectangle =
    { left = 50
    , top = 50
    , width = 100
    , height = 100
    , color = Color.rgb255 0 0 100
    }
