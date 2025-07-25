// swift-tools-version: 6.1
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "example-worker-blocking",
    targets: [
        .executableTarget(
            name: "example-worker-blocking",
            resources: [.copy("Main.elm")]
        )
    ]
)
