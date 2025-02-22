// import Elm
import Foundation

print("running")
let inputBytes = FileHandle.standardInput.availableData
if let input = String(bytes: inputBytes, encoding: .utf8) {
    print(Elm.string_reverse(input))
} else {
    try FileHandle.standardError.write(contentsOf: Data("failed to read input from stdin".utf8))
}

func example() -> String {
    {
        let x = ""
        return x
    }()
}
