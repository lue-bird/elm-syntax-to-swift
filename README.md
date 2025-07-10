# overall TODO
- split let and func declarations in result of `ElmSyntaxToSwift.modules`


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
public enum Elm {
    ..some default declarations..

    static public func sample_plus2(n: Double) -> Double {
        return basics_add(n, list_sum(List_List.Cons(2.0, List_List.Empty)));
    }
}
"""
```

### be aware

TODO MERGE START
-   only a subset of elm is currently supported. not supported:
    - 🚧 accessing record fields before their type is concretely known can sometimes not be inferred by swift
    - 🚧 constructing variants of a generic type before their type is concretely known can sometimes not be inferred by swift
    -   `elm/regex`, `elm/file`, `elm/bytes`, `elm/http`, `elm/random`, `elm/url`, `elm/json`, `elm/parser`, `elm/virtual-dom`,
        `elm/html`, `elm/svg`, `elm/browser`, `elm/time`, `elm-explorations/markdown`, `elm-explorations/webgl`, `elm-explorations/benchmark`, `elm-explorations/linear-algebra`
    -   `Platform`, `Platform.Cmd`, `Platform.Sub`, `Task`, `Process`
    -   **record update**, currying, ports, glsl, `(>>)` and `(<<)`, extensible records of any kind inferred or annotated
      When I finish implementing type inference, this will be fixed.
    -   `++` will default to `List.append` unless one of the arguments is a string literal. So e.g. use `a ++ b ++ ""` to append string variables (which is also faster in elm)
    - `List.minimum`, `List.maximum`, `Basics.min`, `Basics.max` will only work with numbers.
    -   potential future candidates: `Basics.clamp`, `Basics.degrees`, `Basics.turns`,
        `Basics.radians`, `Basics.logBase`, `Basics.toPolar`, `Basics.fromPolar`, `Basics.never`, `Basics.sin`, `Basics.cos`, `Basics.tan`, `Basics.asin`, `Basics.acos`, `Basics.atan`, `Basics.atan2`, `Basics.e`, `Basics.pi`,
        `List.map3/4/5`, `List.head`, `List.tail`, `List.partition`, `Char.toLocaleLower`, `Char.toLocaleUpper`, `Char.isAlpha`, `Char.isAlphaNum`, `Char.isDigit`, `Char.isOctDigit`, `Bitwise`, `Array`.
        Any help appreciated!
TODO MERGE END

-   not supported are
    -   ports that use non-json values like `port sendMessage : String -> Cmd msg`, glsl
    -   `elm/file`, `elm/http`, `elm/browser`, `elm-explorations/markdown`, `elm-explorations/webgl`, `elm-explorations/benchmark`
    -   `Task`, `Process`, `Platform.Task`, `Platform.ProcessId`, `Platform.Router`, `Platform.sendToApp`, `Platform.sendToSelf`, `Random.generate`, `Time.now`, `Time.every`, `Time.here`, `Time.getZoneName`, `Bytes.getHostEndianness`, `Math.Matrix4.inverseOrthonormal`, `Math.Matrix4.mulAffine`
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
-   elm-exploration/linear-algebra's `Vec2`, `Vec3`, `Vec4`, `Mat4` components have 64-bit precision but their swift counterparts only have 32
-   dependencies cannot internally use the same module names as the transpiled project
-   the resulting code might not be readable or even conventionally formatted and comments are not preserved

Please [report any issues](https://github.com/lue-bird/elm-syntax-to-swift/issues/new) you notice <3

### why swift?
-   it runs decently fast natively (and not-quite-officially as wasm)
-   it's pretty much a superset of elm which makes transpiling easy

### how do I use the transpiled output?
An example can be found in [`example-hello-world/`](https://github.com/lue-bird/elm-syntax-to-swift/tree/main/example-hello-world).


where `Elm.YourModule_yourFunction` is the transpiled elm function `Your.Module.yourFunction`. (If the value/function contains `number` type variables or extensible records, search for `Elm.YourModule_yourFunction__` to see the different specialized options)

You will find these types:
  - elm `Bool` (`True` or `False`) → swift `Bool` (`true` or `false`), `String` → `String`, `()` → `()`, `Array Float` → `Array<Double>`, `Set Float` -> `Set<Double>`, `Dict Float Char` → `Map<Double, char>`, `Never` → [`Never`](https://developer.apple.com/documentation/swift/never)
  - elm `Float`s, `Int`s and `number-` variable typed values will be of type [`Double`](https://developer.apple.com/documentation/swift/double)
  - elm `Char`s will be of type [`UnicodeScalar`](https://developer.apple.com/documentation/swift/unicode/scalar)
  - elm tuples/triples like `( Float, Float )`
    will be of type `( Double, Double )`
  - elm records like `{ y : Float, x : Float }` will be of type `( x: Double, y: Double )` with the fields sorted. Single-field records like `{ min : Float }` will have an extra field because swift does not support single-field anonymous records/tuples: `( min: Double, unusedDummyFieldBecauseSwiftDoesNotSupportSingleFieldRecord: () )`.
    If you'd like a shorter name or an alternative solution, please open an issue
  - elm `Json.Encode.Value`/`Json.Decode.Value` will be of type
    [`System.Text.Json.Nodes.JsonNode`](https://learn.microsoft.com/en-us/dotnet/api/system.text.json.nodes.jsonnode?view=net-9.0).
    Encode and decode them like you would in elm, like `Elm.JsonEncode_float 2.2`
  - a transpiled elm app does not run itself.
    An elm main `Platform.worker` program type will literally just consist of fields `Init`, `Update` and `Subscriptions` where
    subscriptions/commands are returned as a list of `Elm.PlatformSub_SubSingle`/`Elm.PlatformCmd_CmdSingle` with possible elm subscriptions/commands in a choice type.
    It's then your responsibility as "the platform" to perform effects, create events and manage the state. For an example see [example-worker/](https://github.com/lue-bird/elm-syntax-to-swift/tree/main/example-worker)
  - elm `Regex` will be of type [`System.Text.RegularExpressions.Regex`](https://learn.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=net-9.0).
    Create them like you would in elm with `Elm.Regex_fromString`, `Elm.Regex_fromStringWith` or `Elm.Regex_never`
  - elm-exploration/linear-algebra's `Math.Matrix2.Vec2`, `Math.Matrix3.Vec3`, `Math.Matrix4.Vec4`, `Math.Matrix4.Mat4` will be of type [`System.Numerics.Vector2`](https://learn.microsoft.com/en-us/dotnet/api/system.numerics.vector2?view=net-9.0), [`System.Numerics.Vector3`](https://learn.microsoft.com/en-us/dotnet/api/system.numerics.vector3?view=net-9.0), [`System.Numerics.Vector4`](https://learn.microsoft.com/en-us/dotnet/api/system.numerics.vector4?view=net-9.0), [`System.Numerics.Matrix4x4`](https://learn.microsoft.com/en-us/dotnet/api/system.numerics.matrix4x4?view=net-9.0)

Compile the resulting swift to an executable:
```bash
```
The built executable can now be found at ``.

Or build and run it once:
```bash
dotnet run
```

If something unexpected happened,
please [report an issue](https://github.com/lue-bird/elm-syntax-to-swift/issues/new).
