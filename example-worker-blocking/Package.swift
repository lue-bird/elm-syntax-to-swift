// swift-tools-version: 6.1
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "example-worker",
    targets: [
        .executableTarget(
            name: "example-worker",
            resources: [ .copy("Main.elm") ]
        )
    ]
)