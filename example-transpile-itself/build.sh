#!/bin/sh
./../node-elm-to-swift/dist/elm-to-swift
swiftc Sources/main.swift Sources/Elm.swift -o .build/main
# to run, go into the elm project to transpile and reference ./.build/main
