#!/bin/sh
./../node-elm-to-swift/dist/elm-to-swift
swift build
./.build/debug/example-sdl2
