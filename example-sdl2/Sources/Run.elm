module Run exposing (initWindow)

initWindow : { windowWidth : Int, windowHeight : Int, title : String }
initWindow =
    { windowWidth = 800, windowHeight = 600, title = "example-sdl2" }
