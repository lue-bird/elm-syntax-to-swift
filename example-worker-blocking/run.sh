#!/bin/sh
./../node-elm-to-swift/dist/elm-to-swift
swiftc Sources/main.swift Sources/Elm.swift -o .build/main
./.build/main $1
