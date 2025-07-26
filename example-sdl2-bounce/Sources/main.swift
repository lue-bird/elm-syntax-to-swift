import SDL

if SDL_Init(SDL_INIT_EVENTS | SDL_INIT_VIDEO) != 0 {
    fatalError("SDL could not initialize! SDL_Error: \(String(cString: SDL_GetError()))")
}

let elmInitialized = Elm.Main_initWindow
var window: OpaquePointer?
var renderer: OpaquePointer?
SDL_CreateWindowAndRenderer(
    Int32(elmInitialized.windowWidth),
    Int32(elmInitialized.windowHeight),
    SDL_WINDOW_RESIZABLE.rawValue,
    &window,
    &renderer
)
SDL_SetWindowTitle(window, elmInitialized.windowTitle)
SDL_SetWindowPosition(
    window,
    Int32(SDL_WINDOWPOS_CENTERED_MASK),
    Int32(SDL_WINDOWPOS_CENTERED_MASK)
)

var shouldQuit: Bool = false
var event: SDL_Event = SDL_Event()
var elmState = elmInitialized.state
while !shouldQuit {
    var onQuit: () -> Void = {}
    for command in Elm.Main_stateToInterface(elmState) {
        switch command {
        case let .Main_InterfaceOnQuit(constructNewState):
            onQuit = { elmState = constructNewState(.Unit) }
        case .Main_InterfaceQuit:
            shouldQuit = true
        case let .Main_InterfaceOnSimulationTick(constructNewState):
            elmState = constructNewState(.Unit)
        case let .Main_InterfaceRender(toRender):
            let clearColor = elmColorToRgba255(toRender.clearColor)
            SDL_SetRenderDrawColor(renderer, clearColor.r, clearColor.g, clearColor.b, clearColor.a)
            SDL_RenderClear(renderer)
            for elementToRender in toRender.elements {
                renderElement(renderer, elementToRender)
            }
            SDL_RenderPresent(renderer)
        }
    }
    while SDL_PollEvent(&event) > 0 {
        switch event.type {
        case SDL_QUIT.rawValue: onQuit()
        case _: break
        }
    }

    SDL_Delay(1000 / 60)
}

func renderElement(
    _ renderer: OpaquePointer?,
    _ element: Elm.Main_ElementToRender
) {
    switch element {
    case let .Main_FilledRectangleToRender(filledRectToRender):
        renderFilledRectangle(renderer, filledRectToRender)
    }
}
func renderFilledRectangle(
    _ renderer: OpaquePointer?,
    _ filledRectToRender: Elm.Main_FilledRectangleToRender
) {
    let color = elmColorToRgba255(filledRectToRender.color)
    SDL_SetRenderDrawColor(renderer, color.r, color.g, color.b, color.a)
    var rectToRender: SDL_Rect = SDL_Rect(
        x: Int32(filledRectToRender.left),
        y: Int32(filledRectToRender.top),
        w: Int32(filledRectToRender.width),
        h: Int32(filledRectToRender.height)
    )
    SDL_RenderFillRect(renderer, &rectToRender)
}
func elmColorToRgba255(_ elmColor: Elm.Color_Color) -> (r: UInt8, g: UInt8, b: UInt8, a: UInt8) {
    let colorComponents = Elm.Color_toRgba(elmColor)
    return (
        r: UInt8(colorComponents.red * 255),
        g: UInt8(colorComponents.green * 255),
        b: UInt8(colorComponents.blue * 255),
        a: UInt8(colorComponents.alpha * 255)
    )
}

SDL_DestroyWindow(window)
SDL_Quit()
