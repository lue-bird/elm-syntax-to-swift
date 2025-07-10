#!/bin/sh
set -x # print the commands being run

cc -c Sources/C/c.c

swiftc Sources/Elm.swift Sources/main.swift -I Sources -L Sources/C -Xlinker ./c.o
