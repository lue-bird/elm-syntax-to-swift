More or less a drop-in replacement for [node-elm-to-swift](https://github.com/lue-bird/elm-syntax-to-swift/tree/main/node-elm-to-swift).

There's no real use case for this (beyond a proof of concept that it can compile itself) because
- the setup for users is more complicated (build node-elm-to-swift, install dotnet, wait a minute for `dotnet publish`)
- ahead of time compiling this project currently runs into a dotnet bug when running (`System.TypeLoadException`). Maybe some dontnet update will fix it
