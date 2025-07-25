// swift-tools-version: 6.1
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "example-transpile-itself",
    targets: [
        .executableTarget(
            name: "example-transpile-itself",
            resources: [.copy("Main.elm")]
        )
    ]
)
