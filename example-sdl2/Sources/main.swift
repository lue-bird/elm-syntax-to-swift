import SDL

if SDL_Init(SDL_INIT_VIDEO) != 0 {
    fatalError("SDL could not initialize! SDL_Error: \(String(cString: SDL_GetError()))")
}

let (title:title, windowHeight:windowHeight, windowWidth:windowWidth) = Elm.Run_initWindow
let window = SDL_CreateWindow(
    title,
    Int32(SDL_WINDOWPOS_CENTERED_MASK),
    Int32(SDL_WINDOWPOS_CENTERED_MASK),
    Int32(windowWidth),
    Int32(windowHeight),
    SDL_WINDOW_SHOWN.rawValue)

var shouldQuit = false
var event = SDL_Event()
while !shouldQuit {
    while SDL_PollEvent(&event) > 0 {
        shouldQuit = (event.type == SDL_QUIT.rawValue)
    }
    SDL_Delay(1000 / 60)
}

SDL_DestroyWindow(window)
SDL_Quit()
