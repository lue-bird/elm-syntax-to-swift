Print [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) declarations as [swift](https://swift.org/) code.
To try it out, you can
run [this script](https://github.com/lue-bird/elm-syntax-to-swift/tree/main/node-elm-to-swift).

```elm
import Elm.Parser
import ElmSyntaxToSwift

"""module Sample exposing (..)

plus2 : Int -> Int
plus2 n =
    n + ([ 2 ] |> List.sum)
"""
    |> Elm.Parser.parseToFile
    |> Result.mapError (\_ -> "failed to parse elm source code")
    |> Result.map
        (\syntaxModule ->
            [ syntaxModule ]
                |> ElmSyntaxToSwift.modules
                |> .declarations
                |> ElmSyntaxToSwift.swiftDeclarationsToModuleString
        )
-->
Ok """import Foundation
...
public enum Elm {
    ..some default declarations..

    @Sendable public static func Sample_plus2(_ n: Double) -> Double {
        Basics_add(n, List_sum(Array_toList([ 2.0 ])))
    }
}
"""
```

### be aware

-   not supported are
    -   ports that use non-json values like `port sendMessage : String -> Cmd msg`, glsl
    -   `elm/file`, `elm/http`, `elm/browser`, `elm-explorations/markdown`, `elm-explorations/webgl`, `elm-explorations/benchmark` (for the moment also `elm/random`)
    -   `Task`, `Process`, `Platform.Task`, `Platform.ProcessId`, `Platform.Router`, `Platform.sendToApp`, `Platform.sendToSelf`, `Random.generate`, `Time.now`, `Time.every`, `Time.here`, `Time.getZoneName`, `Bytes.getHostEndianness`, `Math.Matrix4` (due to swift's standard library not exposing many [simd types and operations available in apple's SDK](https://developer.apple.com/documentation/simd/simd_double4x4))
    -   extensible record types outside of module-level value/function declarations. For example, these declarations might not work:
        ```elm
        -- in variant value
        type Named rec = Named { rec | name : String }
        -- in let type, annotated or not
        let getName : { r | name : name } -> name
        ```
        Allowed is only record extension in module-level value/functions, annotated or not:
        ```elm
        userId : { u | name : String, server : Domain } -> String
        ```
        In the non-allowed cases listed above, we assume that you intended to use a regular record type with only the extension fields which can lead to swift compile errors if you actually pass in additional fields.
-   dependencies cannot internally use the same module names as the transpiled project
-   the resulting code might not be readable or even conventionally formatted and comments are not preserved
-  on every web search you need to ignore -app -swiftui -ios -apple -xcode -taylor

Please [report any issues](https://github.com/lue-bird/elm-syntax-to-swift/issues/new) you notice <3

### why swift?
-   it runs decently fast natively (and semi-officially as wasm)
-   it's a kind-of superset of elm which makes transpiling easier

### how do I use the transpiled output?
An example can be found in [`example-hello-world/`](https://github.com/lue-bird/elm-syntax-to-swift/tree/main/example-hello-world).

In your elm project, add a file `Sources/main.swift` that uses `Elm.swift`:
```swift
print(Elm.YourModule_yourFunction("yourInput"))
```

where `Elm.YourModule_yourFunction(firstArgument)(secondArgument)` is the transpiled elm function `Your.Module.yourFunction firstArgument secondArgument`. (If the value/function contains extensible records, search for `Elm.YourModule_yourFunction__` to see the different specialized options)

You will find these types:
  - elm `Bool` (`True` or `False`) → swift `Bool` (`true` or `false`), `String` → `String`, `Array Float` → `Array<Double>`, `Set Float` -> `Set<Double>`, `Dict Float String` → `Map<Double, String>`, `Never` → [`Never`](https://developer.apple.com/documentation/swift/never)
  - elm `Float`s, `Int`s and `number-` variable typed values will be of type [`Double`](https://developer.apple.com/documentation/swift/double)
  - elm `Char`s will be of type [`UnicodeScalar`](https://developer.apple.com/documentation/swift/unicode/scalar)
  - elm unit `()` will be of type `Elm.Unit` and can be constructed and matched with `.Unit`
  - elm tuples like `( Float, String )` will be of type `Elm.Tuple<Double, String>` and can be constructed and matched with `.Tuple(_, _)`. `.first` and `.second` access also works
  - elm triples like `( Float, Float, String )` will be of type `Elm.Triple<Double, Double, String>` and can be constructed and matched with `.Triple(_, _, _)`. `.first`, `.second` and `.third` access also works
  - elm records like `{ y : Float, x : Float }` will be of type `Elm.Generated_x_y<Double, Double>` with the fields sorted and can be constructed and matched with `Elm.Generated_x_y.Record(x: _, y: _)`. `record.x` access also works
  - a transpiled elm app does not run itself.
    An elm main `Platform.worker` program type will literally just consist of fields `Init`, `Update` and `Subscriptions` where
    subscriptions/commands are returned as a list of `Elm.PlatformSub_SubSingle`/`Elm.PlatformCmd_CmdSingle` with possible elm subscriptions/commands in a choice type.
    It's then your responsibility as "the platform" to perform effects, create events and manage the state. For an example see [example-worker/](https://github.com/lue-bird/elm-syntax-to-swift/tree/main/example-worker) TODO

Compile the resulting swift to an executable:
```bash
swiftc Sources/main.swift Sources/Elm.swift
```
The built executable can now be found at `main`; append ` -o your-path` to set a different output path; append `-O` to enable optimizations like tail-call elimination.

When in a project that has a `Package.swift`, you can also use
```bash
swift build
```

If something unexpected happened,
please [report an issue](https://github.com/lue-bird/elm-syntax-to-swift/issues/new).
