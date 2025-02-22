port module Main exposing (main)

import Ansi.Color
import Ansi.Cursor
import Ansi.Font
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Elm.Package
import Elm.Parser
import Elm.Project
import Elm.Syntax.Declaration
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Version
import ElmSyntaxToSwift
import FastDict
import FastSet
import Json.Decode
import Json.Encode
import Node


type State
    = WaitingForElmJson
    | Running RunningState
    | ElmJsonReadFailed String
    | Finished (Result { code : String, message : String } ())


type alias RunningState =
    { sourceDirectoriesToRead : FastSet.Set String
    , sourceFilesToRead : FastSet.Set String
    , parsedModules : List Elm.Syntax.File.File
    , sourceDirectoryReadErrors : List { path : String, message : String }
    , sourceFileReadErrors : List { path : String, message : String }
    }


initialState : State
initialState =
    WaitingForElmJson


packageSourceDirectoryPath : { name : String, version : String } -> String
packageSourceDirectoryPath packageMeta =
    "/home/pascal/.elm/0.19.1/packages/"
        ++ packageMeta.name
        ++ "/"
        ++ packageMeta.version
        ++ "/src"


interface : State -> Node.Interface State
interface state =
    case state of
        WaitingForElmJson ->
            nodeElmJsonRequest
                |> Node.interfaceFutureMap
                    (\elmJsonBytesOrError ->
                        case elmJsonBytesOrError of
                            Err elmJsonReadError ->
                                ElmJsonReadFailed elmJsonReadError

                            Ok elmJson ->
                                case elmJson of
                                    Elm.Project.Application application ->
                                        Running
                                            { sourceDirectoriesToRead =
                                                application.dirs
                                                    ++ ((application.depsDirect
                                                            ++ application.depsIndirect
                                                        )
                                                            |> List.filter
                                                                (\( dependencyName, _ ) ->
                                                                    case dependencyName |> Elm.Package.toString of
                                                                        "rtfeldman/elm-hex" ->
                                                                            False

                                                                        "stil4m/structured-writer" ->
                                                                            False

                                                                        _ ->
                                                                            True
                                                                )
                                                            |> List.map
                                                                (\( dependencyName, dependencyVersion ) ->
                                                                    packageSourceDirectoryPath
                                                                        { name = dependencyName |> Elm.Package.toString
                                                                        , version = dependencyVersion |> Elm.Version.toString
                                                                        }
                                                                )
                                                       )
                                                    |> FastSet.fromList
                                            , sourceFilesToRead = FastSet.empty
                                            , parsedModules = []
                                            , sourceFileReadErrors = []
                                            , sourceDirectoryReadErrors = []
                                            }

                                    Elm.Project.Package _ ->
                                        ElmJsonReadFailed
                                            "You're in a package but elm-to-swift only works for applications."
                    )

        Running runningState ->
            runningInterface runningState

        Finished result ->
            case result of
                Err error ->
                    errorInterface "Failed to write the bundled code into Sources/elm.swift."

                Ok () ->
                    Node.standardOutWrite
                        "Successfully wrote the bundled code into Sources/elm.swift.\n"

        ElmJsonReadFailed elmJsonDecodeError ->
            errorInterface elmJsonDecodeError


errorInterface : String -> Node.Interface future_
errorInterface message =
    [ Node.standardErrWrite (message ++ "\n")
    , Node.exit 1
    ]
        |> Node.interfaceBatch


nodeElmJsonRequest : Node.Interface (Result String Elm.Project.Project)
nodeElmJsonRequest =
    Node.fileRequest "elm.json"
        |> Node.interfaceFutureMap
            (\elmJsonBytesOrError ->
                case elmJsonBytesOrError of
                    Err fileReadError ->
                        Err
                            ("elm.json couldn't be read because "
                                ++ fileReadError.message
                            )

                    Ok elmJsonBytes ->
                        case elmJsonBytes |> Bytes.Decode.decode (Bytes.Decode.string (Bytes.width elmJsonBytes)) of
                            Nothing ->
                                Err "elm.json bytes could not be decoded into UTF-8 String"

                            Just elmJsonString ->
                                case elmJsonString |> Json.Decode.decodeString Elm.Project.decoder of
                                    Err jsonDecodeError ->
                                        Err
                                            ("elm.json failed to parse due to "
                                                ++ (jsonDecodeError |> Json.Decode.errorToString)
                                            )

                                    Ok elmJson ->
                                        Ok elmJson
            )


runningInterface : RunningState -> Node.Interface State
runningInterface state =
    [ if
        (state.sourceDirectoriesToRead |> FastSet.isEmpty)
            && (state.sourceFilesToRead |> FastSet.isEmpty)
      then
        let transpiledDeclarationsAndErrors = state.parsedModules
                    |> ElmSyntaxToSwift.modules
        in
        [ Node.standardOutWrite (
                (transpiledDeclarationsAndErrors.errors
                |> List.filter (\error ->
                    -- TODO remove filter for production use
                    error /= "could not find module origin of the type reference Decoder"
                    && error /= "could not find module origin of the type reference Value"
                )
                |> String.join "\n")
                ++ "\n")
        , Node.fileWrite
            { path = "Sources/elm.swift"
            , content =
                transpiledDeclarationsAndErrors.declarations
                    |> ElmSyntaxToSwift.swiftDeclarationsToFileString
                    |> Bytes.Encode.string
                    |> Bytes.Encode.encode
            }
            |> Node.interfaceFutureMap
                Finished
        ]
            |> Node.interfaceBatch

      else
        Node.interfaceNone
    , state.sourceDirectoriesToRead
        |> fastSetToListAndMap
            (\sourceDirectoryPath ->
                Node.directorySubPathsRequest sourceDirectoryPath
                    |> Node.interfaceFutureMap
                        (\subPathsOrError ->
                            case subPathsOrError of
                                Err sourceDirectoryReadError ->
                                    Running
                                        { state
                                            | sourceDirectoryReadErrors =
                                                { path = sourceDirectoryPath
                                                , message = sourceDirectoryReadError.message
                                                }
                                                    :: state.sourceDirectoryReadErrors
                                        }

                                Ok subPaths ->
                                    Running
                                        { sourceDirectoriesToRead =
                                            state.sourceDirectoriesToRead
                                                |> FastSet.remove sourceDirectoryPath
                                        , sourceFilesToRead =
                                            subPaths
                                                |> List.filter
                                                    (\subPath ->
                                                        (subPath |> String.endsWith ".elm")
                                                            && -- TODO remove this filter for general use
                                                               Basics.not
                                                                ((sourceDirectoryPath |> String.contains "stil4m/elm-syntax")
                                                                    && ((subPath |> String.contains "Elm/Writer")
                                                                            || (subPath |> String.contains "Elm/Processing")
                                                                            || (subPath |> String.contains "Elm/RawFile")
                                                                            || (subPath |> String.contains "Elm/Parser")
                                                                            || (subPath |> String.contains "ParserFast")
                                                                            || (subPath |> String.contains "ParserWithComments")
                                                                            || (subPath |> String.contains "Rope")
                                                                            || (subPath |> String.contains "Elm/Interface")
                                                                            || (subPath |> String.contains "Elm/Internal/RawFile")
                                                                       )
                                                                )
                                                    )
                                                |> List.foldl
                                                    (\subPath soFar ->
                                                        soFar
                                                            |> FastSet.insert
                                                                (sourceDirectoryPath ++ "/" ++ subPath)
                                                    )
                                                    state.sourceFilesToRead
                                        , parsedModules = state.parsedModules
                                        , sourceDirectoryReadErrors = state.sourceDirectoryReadErrors
                                        , sourceFileReadErrors = state.sourceFileReadErrors
                                        }
                        )
            )
        |> Node.interfaceBatch
    , state.sourceFilesToRead
        |> fastSetToListAndMap
            (\sourceFilePath ->
                Node.fileRequest sourceFilePath
                    |> Node.interfaceFutureMap
                        (\sourceBytesOrError ->
                            let
                                sourceBytesOrReadError : Result String Elm.Syntax.File.File
                                sourceBytesOrReadError =
                                    case sourceBytesOrError of
                                        Err sourceFileReadError ->
                                            Err sourceFileReadError.message

                                        Ok sourceBytes ->
                                            sourceBytes |> bytesToElmSyntaxModule
                            in
                            case sourceBytesOrReadError of
                                Err readError ->
                                    Running
                                        { state
                                            | sourceFileReadErrors =
                                                { path = sourceFilePath
                                                , message = readError
                                                }
                                                    :: state.sourceFileReadErrors
                                        }

                                Ok syntaxModule ->
                                    Running
                                        { sourceDirectoriesToRead = state.sourceDirectoriesToRead
                                        , sourceFileReadErrors = state.sourceFileReadErrors
                                        , sourceDirectoryReadErrors = state.sourceDirectoryReadErrors
                                        , sourceFilesToRead =
                                            state.sourceFilesToRead
                                                |> FastSet.remove sourceFilePath
                                        , parsedModules =
                                            syntaxModule
                                                :: state.parsedModules
                                        }
                        )
            )
        |> Node.interfaceBatch
    , state.sourceDirectoryReadErrors
        |> List.map
            (\directoryReadError ->
                Node.standardOutWrite
                    ("failed to read the source directory "
                        ++ directoryReadError.path
                        ++ ": "
                        ++ directoryReadError.message
                        ++ "\n"
                    )
            )
        |> Node.interfaceBatch
    , state.sourceFileReadErrors
        |> List.map
            (\fileReadError ->
                Node.standardOutWrite
                    ("failed to read the source file "
                        ++ fileReadError.path
                        ++ ": "
                        ++ fileReadError.message
                        ++ "\n"
                    )
            )
        |> Node.interfaceBatch
    ]
        |> Node.interfaceBatch


bytesToElmSyntaxModule : Bytes -> Result String Elm.Syntax.File.File
bytesToElmSyntaxModule sourceBytes =
    case sourceBytes |> Bytes.Decode.decode (Bytes.Decode.string (sourceBytes |> Bytes.width)) of
        Nothing ->
            Err "source bytes couldn't be decoded into UTF-8"

        Just source ->
            case source |> Elm.Parser.parseToFile of
                Err _ ->
                    Err "source couldn't be parsed. Check for compiler errors."

                Ok syntax ->
                    Ok syntax


moduleHeaderName : Elm.Syntax.Module.Module -> Elm.Syntax.ModuleName.ModuleName
moduleHeaderName moduleHeader =
    case moduleHeader of
        Elm.Syntax.Module.NormalModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.PortModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.EffectModule header ->
            header.moduleName |> Elm.Syntax.Node.value


fastDictToListAndMap : (a -> b -> c) -> FastDict.Dict a b -> List c
fastDictToListAndMap keyValueToElement fastDict =
    fastDict
        |> FastDict.foldr
            (\key value soFar ->
                keyValueToElement key value :: soFar
            )
            []


fastSetToListAndMap : (a -> b) -> FastSet.Set a -> List b
fastSetToListAndMap keyToElement fastDict =
    fastDict
        |> FastSet.foldr
            (\key soFar ->
                keyToElement key :: soFar
            )
            []


main : Node.Program State
main =
    Node.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
