// swift-tools-version: 6.1
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "example-sdl2",
    dependencies: [
        .package(url: "https://github.com/ctreffs/SwiftSDL2.git", from: "1.4.0")
    ],
    targets: [
        .executableTarget(
            name: "example-sdl2",
            dependencies: [
                .product(name: "SDL", package: "SwiftSDL2")
            ],
            resources: [.copy("Main.elm")]
        )
    ]
)
