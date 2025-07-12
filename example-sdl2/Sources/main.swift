import SDL

if SDL_Init(SDL_INIT_VIDEO) != 0 {
    fatalError("SDL could not initialize! SDL_Error: \(String(cString: SDL_GetError()))")
}

let (
    title:title,
    windowHeight:windowHeight,
    windowWidth:windowWidth
) = Elm.Run_initWindow
var window: OpaquePointer?
var renderer: OpaquePointer?
SDL_CreateWindowAndRenderer(
    Int32(windowWidth),
    Int32(windowHeight),
    SDL_WINDOW_RESIZABLE.rawValue,
    &window,
    &renderer
)
SDL_SetWindowTitle(window, title)
SDL_SetWindowPosition(
    window,
    Int32(SDL_WINDOWPOS_CENTERED_MASK),
    Int32(SDL_WINDOWPOS_CENTERED_MASK)
)

var shouldQuit: Bool = false
var event: SDL_Event = SDL_Event()
while !shouldQuit {
    while SDL_PollEvent(&event) > 0 {
        shouldQuit = (event.type == SDL_QUIT.rawValue)
    }
    SDL_SetRenderDrawColor(renderer, 100, 0, 0, 0)
    SDL_RenderClear(renderer)

    renderFilledRectangle(renderer, Elm.Run_renderFilledRectangle)

    SDL_RenderPresent(renderer)

    SDL_Delay(1000 / 60)
}

func renderFilledRectangle(
    _ renderer: OpaquePointer?,
    _ filledRectToRender: Elm.Run_FilledRectangleToRender
) {
    let colorComponents = Elm.Color_toRgba(filledRectToRender.color)
    SDL_SetRenderDrawColor(
        renderer,
        UInt8(colorComponents.red * 255), UInt8(colorComponents.green * 255),
        UInt8(colorComponents.blue * 255), UInt8(colorComponents.alpha * 255)
    )
    var rectToRender: SDL_Rect = SDL_Rect(
        x: Int32(filledRectToRender.left),
        y: Int32(filledRectToRender.top),
        w: Int32(filledRectToRender.width),
        h: Int32(filledRectToRender.height)
    )
    SDL_RenderFillRect(renderer, &rectToRender)
}

SDL_DestroyWindow(window)
SDL_Quit()
