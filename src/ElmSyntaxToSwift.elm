module ElmSyntaxToSwift exposing
    ( modules, swiftDeclarationsToFileString
    , SwiftExpression(..), SwiftPattern(..), SwiftType(..)
    )

{-| Transpiling [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/)
declarations to swift.

@docs modules, swiftDeclarationsToFileString
@docs SwiftExpression, SwiftPattern, SwiftType

If you need more fine-grained helpers,
[open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new)

-}

import Data.Graph
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import FastDict
import FastSet
import Print exposing (Print)
import Unicode


{-| The sub-set of swift type syntax used in generated code
-}
type SwiftType
    = SwiftTypeConstruct
        { moduleOrigin : Maybe String
        , name : String
        , arguments : List SwiftType
        }
    | SwiftTypeRecord (FastDict.Dict String SwiftType)
    | SwiftTypeVariable String
    | SwiftTypeFunction
        { input : List SwiftType
        , output : SwiftType
        }


{-| The sub-set of swift pattern syntax used in generated code
-}
type SwiftPattern
    = SwiftPatternIgnore
    | SwiftPatternTrue
    | SwiftPatternFalse
    | SwiftPatternFloat Float
    | SwiftPatternString String
    | SwiftPatternVariable String
    | SwiftPatternVariant
        { originTypeName : String
        , name : String
        , values : List SwiftPattern
        }
    | SwiftPatternRecord (FastDict.Dict String SwiftPattern)


{-| The sub-set of swift expression syntax used in generated code
-}
type SwiftExpression
    = SwiftExpressionFloat Float
    | SwiftExpressionString String
    | SwiftExpressionReference
        { moduleOrigin : Maybe String
        , name : String
        }
    | SwiftExpressionVariant
        { originTypeName : String
        , name : String
        }
    | SwiftExpressionRecordAccess
        { record : SwiftExpression
        , field : String
        }
    | SwiftExpressionRecord (FastDict.Dict String SwiftExpression)
    | SwiftExpressionCall
        { called : SwiftExpression
        , arguments : List SwiftExpression
        }
    | SwiftExpressionLambda
        { parameter0 : Maybe String
        , parameter1Up : List (Maybe String)
        , result : SwiftExpression
        }
    | SwiftExpressionSwitch
        { matched : SwiftExpression
        , case0 :
            { pattern : SwiftPattern
            , result : SwiftExpression
            }
        , case1Up :
            List
                { pattern : SwiftPattern
                , result : SwiftExpression
                }
        }
    | SwiftExpressionWithLocalDeclaration
        { declaration :
            { name : String
            , parameters : List (Maybe String)
            , result : SwiftExpression
            , type_ : Maybe SwiftType
            }
        , result : SwiftExpression
        }


type SwiftValueOrFunctionDeclarationOrDestructuring
    = SwiftDestructuring
        { pattern : SwiftPattern
        , expression : SwiftExpression
        }
    | SwiftLocalDeclarationValueOrFunction
        { name : String
        , parameters : List (Maybe String)
        , result : SwiftExpression
        , type_ : Maybe SwiftType
        }


{-| How do references used in a module map to their origin module?

Contains variants, variant function and value declaration names.

-}
type alias ModuleContext =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            , originTypeName : String
            }
    }


{-| Calculate valid mappings of qualifications + name
to origin module based on a module's imports.

Requires all exposed names
so we can resolve `exposing (..)` and `EnumType(..)`.

-}
importsToModuleContext :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        { valueOrFunctionOrTypeAliasNames : FastSet.Set String
        , enumTypesExposingVariants :
            FastDict.Dict String (FastDict.Dict String { valueCount : Int })
        }
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import)
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                , originTypeName : String
                }
        }
importsToModuleContext moduleExposes imports =
    let
        importsNormal :
            List
                { moduleName : Elm.Syntax.ModuleName.ModuleName
                , alias : Maybe String
                , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                    FastSet.Set String
                , exposedVariants :
                    FastDict.Dict
                        String
                        { valueCount : Int
                        , originTypeName : String
                        }
                }
        importsNormal =
            implicitImports
                ++ (imports
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ syntaxImport) ->
                                let
                                    importModuleName : Elm.Syntax.ModuleName.ModuleName
                                    importModuleName =
                                        syntaxImport.moduleName |> Elm.Syntax.Node.value

                                    exposes :
                                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes :
                                            FastSet.Set String
                                        , variants :
                                            FastDict.Dict
                                                String
                                                { valueCount : Int
                                                , originTypeName : String
                                                }
                                        }
                                    exposes =
                                        case syntaxImport.exposingList of
                                            Nothing ->
                                                { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                    FastSet.empty
                                                , variants = FastDict.empty
                                                }

                                            Just (Elm.Syntax.Node.Node _ syntaxExposing) ->
                                                case moduleExposes |> FastDict.get importModuleName of
                                                    Nothing ->
                                                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                            FastSet.empty
                                                        , variants = FastDict.empty
                                                        }

                                                    Just moduleExposedNames ->
                                                        case syntaxExposing of
                                                            Elm.Syntax.Exposing.All _ ->
                                                                { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                    moduleExposedNames.enumTypesExposingVariants
                                                                        |> FastDict.foldl
                                                                            (\enumTypeName _ soFar ->
                                                                                soFar |> FastSet.insert enumTypeName
                                                                            )
                                                                            moduleExposedNames.valueOrFunctionOrTypeAliasNames
                                                                , variants =
                                                                    moduleExposedNames.enumTypesExposingVariants
                                                                        |> FastDict.foldl
                                                                            (\originTypeName variantNames soFar ->
                                                                                FastDict.union
                                                                                    (variantNames
                                                                                        |> FastDict.map
                                                                                            (\_ info ->
                                                                                                { originTypeName = originTypeName
                                                                                                , valueCount = info.valueCount
                                                                                                }
                                                                                            )
                                                                                    )
                                                                                    soFar
                                                                            )
                                                                            FastDict.empty
                                                                }

                                                            Elm.Syntax.Exposing.Explicit explicitEposes ->
                                                                explicitEposes
                                                                    |> List.foldl
                                                                        (\(Elm.Syntax.Node.Node _ expose) soFar ->
                                                                            case expose of
                                                                                Elm.Syntax.Exposing.InfixExpose _ ->
                                                                                    soFar

                                                                                Elm.Syntax.Exposing.TypeOrAliasExpose name ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                                                                            |> FastSet.insert name
                                                                                    , variants = soFar.variants
                                                                                    }

                                                                                Elm.Syntax.Exposing.FunctionExpose name ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                                                                            |> FastSet.insert name
                                                                                    , variants = soFar.variants
                                                                                    }

                                                                                Elm.Syntax.Exposing.TypeExpose enumTypeExpose ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                                                                            |> FastSet.insert enumTypeExpose.name
                                                                                    , variants =
                                                                                        case enumTypeExpose.open of
                                                                                            Nothing ->
                                                                                                soFar.variants

                                                                                            Just _ ->
                                                                                                case
                                                                                                    moduleExposedNames.enumTypesExposingVariants
                                                                                                        |> FastDict.get enumTypeExpose.name
                                                                                                of
                                                                                                    Nothing ->
                                                                                                        soFar.variants

                                                                                                    Just enumTypeDeclared ->
                                                                                                        FastDict.union
                                                                                                            soFar.variants
                                                                                                            (enumTypeDeclared
                                                                                                                |> FastDict.map
                                                                                                                    (\_ info ->
                                                                                                                        { originTypeName = enumTypeExpose.name
                                                                                                                        , valueCount = info.valueCount
                                                                                                                        }
                                                                                                                    )
                                                                                                            )
                                                                                    }
                                                                        )
                                                                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                            FastSet.empty
                                                                        , variants = FastDict.empty
                                                                        }
                                in
                                { moduleName = importModuleName
                                , alias =
                                    syntaxImport.moduleAlias
                                        |> Maybe.map
                                            (\(Elm.Syntax.Node.Node _ syntaxAlias) ->
                                                syntaxAlias |> String.join "."
                                            )
                                , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                    exposes.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                , exposedVariants = exposes.variants
                                }
                            )
                   )
                |> importsCombine
    in
    importsNormal
        |> List.foldl
            (\syntaxImport soFar ->
                let
                    importedModuleMembers :
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
                            FastSet.Set String
                        , variants :
                            FastDict.Dict
                                String
                                { valueCount : Int
                                , originTypeName : String
                                }
                        }
                    importedModuleMembers =
                        case moduleExposes |> FastDict.get syntaxImport.moduleName of
                            Nothing ->
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    FastSet.empty
                                , variants = FastDict.empty
                                }

                            Just moduleExposedNames ->
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    moduleExposedNames.enumTypesExposingVariants
                                        |> FastDict.foldl
                                            (\enumTypeName _ namesSoFar ->
                                                namesSoFar
                                                    |> FastSet.insert enumTypeName
                                            )
                                            moduleExposedNames.valueOrFunctionOrTypeAliasNames
                                , variants =
                                    moduleExposedNames.enumTypesExposingVariants
                                        |> FastDict.foldl
                                            (\originTypeName variantNames variantsSoFar ->
                                                FastDict.union
                                                    (variantNames
                                                        |> FastDict.map
                                                            (\_ info ->
                                                                { originTypeName = originTypeName
                                                                , valueCount = info.valueCount
                                                                }
                                                            )
                                                    )
                                                    variantsSoFar
                                            )
                                            FastDict.empty
                                }
                in
                moduleImportsContextMerge
                    (moduleImportsContextMerge
                        { variantLookup =
                            syntaxImport.exposedVariants
                                |> FastDict.foldl
                                    (\variantName variantInfo dictSoFar ->
                                        dictSoFar
                                            |> FastDict.insert ( [], variantName )
                                                { moduleOrigin = syntaxImport.moduleName
                                                , valueCount = variantInfo.valueCount
                                                , originTypeName = variantInfo.originTypeName
                                                }
                                    )
                                    FastDict.empty
                        , valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            syntaxImport.exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes
                                |> FastSet.foldl
                                    (\expose dictSoFar ->
                                        dictSoFar
                                            |> FastDict.insert ( [], expose )
                                                syntaxImport.moduleName
                                    )
                                    FastDict.empty
                        }
                        (case syntaxImport.alias of
                            Nothing ->
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    importedModuleMembers.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                                        |> FastSet.foldl
                                            (\exposeFromImportedModule dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( syntaxImport.moduleName, exposeFromImportedModule )
                                                        syntaxImport.moduleName
                                            )
                                            FastDict.empty
                                , variantLookup =
                                    importedModuleMembers.variants
                                        |> FastDict.foldl
                                            (\exposeFromImportedModule variantInfo dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( syntaxImport.moduleName, exposeFromImportedModule )
                                                        { moduleOrigin = syntaxImport.moduleName
                                                        , valueCount = variantInfo.valueCount
                                                        , originTypeName = variantInfo.originTypeName
                                                        }
                                            )
                                            FastDict.empty
                                }

                            Just importAlias ->
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    importedModuleMembers.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                                        |> FastSet.foldl
                                            (\exposeFromImportedModule dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( [ importAlias ], exposeFromImportedModule )
                                                        syntaxImport.moduleName
                                            )
                                            FastDict.empty
                                , variantLookup =
                                    importedModuleMembers.variants
                                        |> FastDict.foldl
                                            (\exposeFromImportedModule variantInfo dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( [ importAlias ], exposeFromImportedModule )
                                                        { moduleOrigin = syntaxImport.moduleName
                                                        , valueCount = variantInfo.valueCount
                                                        , originTypeName = variantInfo.originTypeName
                                                        }
                                            )
                                            FastDict.empty
                                }
                        )
                    )
                    soFar
            )
            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                FastDict.empty
            , variantLookup = FastDict.empty
            }


moduleImportsContextMerge :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            , originTypeName : String
            }
    }
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                , originTypeName : String
                }
        }
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                , originTypeName : String
                }
        }
moduleImportsContextMerge a b =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
        FastDict.union
            a.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
            b.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
    , variantLookup =
        FastDict.union
            a.variantLookup
            b.variantLookup
    }


moduleContextMerge : ModuleContext -> ModuleContext -> ModuleContext
moduleContextMerge a b =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
        FastDict.union
            a.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
            b.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
    , variantLookup =
        FastDict.union
            a.variantLookup
            b.variantLookup
    }


implicitImports :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict
                String
                { valueCount : Int
                , originTypeName : String
                }
        }
implicitImports =
    [ { moduleName = [ "Basics" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "EQ", { valueCount = 0, originTypeName = "Order" } )
                , ( "LT", { valueCount = 0, originTypeName = "Order" } )
                , ( "GT", { valueCount = 0, originTypeName = "Order" } )
                , ( "True", { valueCount = 0, originTypeName = "Bool" } )
                , ( "False", { valueCount = 0, originTypeName = "Bool" } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList
                [ "Int"
                , "Float"
                , "toFloat"
                , "round"
                , "floor"
                , "ceiling"
                , "truncate"
                , "max"
                , "min"
                , "compare"
                , "Order"
                , "Bool"
                , "not"
                , "xor"
                , "modBy"
                , "remainderBy"
                , "negate"
                , "abs"
                , "clamp"
                , "sqrt"
                , "logBase"
                , "e"
                , "pi"
                , "cos"
                , "sin"
                , "tan"
                , "acos"
                , "asin"
                , "atan"
                , "atan2"
                , "degrees"
                , "radians"
                , "turns"
                , "toPolar"
                , "fromPolar"
                , "isNaN"
                , "isInfinite"
                , "identity"
                , "always"
                , "Never"
                , "never"
                ]
      }
    , { moduleName = [ "List" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "List" ]
      }
    , { moduleName = [ "Maybe" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "Just", { valueCount = 1, originTypeName = "Maybe" } )
                , ( "Nothing", { valueCount = 0, originTypeName = "Maybe" } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Maybe" ]
      }
    , { moduleName = [ "Result" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "Ok", { valueCount = 1, originTypeName = "Result" } )
                , ( "Err", { valueCount = 1, originTypeName = "Result" } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Result" ]
      }
    , { moduleName = [ "String" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "String" ]
      }
    , { moduleName = [ "Char" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Char" ]
      }
    , { moduleName = [ "Tuple" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.empty
      }
    , { moduleName = [ "Debug" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.empty
      }
    , { moduleName = [ "Platform" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Program" ]
      }
    , { moduleName = [ "Platform", "Cmd" ]
      , alias = Just "Cmd"
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Cmd" ]
      }
    , { moduleName = [ "Platform", "Sub" ]
      , alias = Just "Sub"
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Sub" ]
      }
    ]


importsCombine :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict
                String
                { valueCount : Int
                , originTypeName : String
                }
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict
                    String
                    { valueCount : Int
                    , originTypeName : String
                    }
            }
importsCombine syntaxImports =
    importsCombineFrom [] syntaxImports


importsCombineFrom :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict
                String
                { valueCount : Int
                , originTypeName : String
                }
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict
                    String
                    { valueCount : Int
                    , originTypeName : String
                    }
            }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict
                    String
                    { valueCount : Int
                    , originTypeName : String
                    }
            }
importsCombineFrom soFar syntaxImports =
    case syntaxImports of
        [] ->
            soFar

        [ onlyImport ] ->
            onlyImport :: soFar

        import0 :: import1 :: import2Up ->
            if import0.moduleName == import1.moduleName then
                importsCombineFrom soFar
                    (importsMerge import0 import1
                        :: import2Up
                    )

            else
                importsCombineFrom
                    (import0 :: soFar)
                    (import1 :: import2Up)


importsMerge :
    { moduleName : Elm.Syntax.ModuleName.ModuleName
    , alias : Maybe String
    , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
        FastSet.Set String
    , exposedVariants :
        FastDict.Dict
            String
            { valueCount : Int
            , originTypeName : String
            }
    }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict
                String
                { valueCount : Int
                , originTypeName : String
                }
        }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict
                String
                { valueCount : Int
                , originTypeName : String
                }
        }
importsMerge earlier later =
    { moduleName = earlier.moduleName
    , alias =
        case earlier.alias of
            Just alias ->
                alias |> Just

            Nothing ->
                later.alias
    , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
        FastSet.union
            earlier.exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes
            later.exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes
    , exposedVariants =
        FastDict.union
            earlier.exposedVariants
            later.exposedVariants
    }


enumTypeDeclaration :
    ModuleContext
    -> Elm.Syntax.Type.Type
    ->
        Result
            String
            { name : String
            , parameters : List String
            , variants : FastDict.Dict String (List SwiftType)
            }
enumTypeDeclaration moduleOriginLookup syntaxEnumType =
    Result.map
        (\variants ->
            { name =
                syntaxEnumType.name
                    |> Elm.Syntax.Node.value
            , parameters =
                syntaxEnumType.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            parameter |> uppercaseNameSanitizeForSwift
                        )
            , variants = variants |> FastDict.fromList
            }
        )
        (syntaxEnumType.constructors
            |> listMapAndCombineOk
                (\(Elm.Syntax.Node.Node _ syntaxVariant) ->
                    Result.map
                        (\values ->
                            ( syntaxVariant.name |> Elm.Syntax.Node.value
                            , values
                            )
                        )
                        (syntaxVariant.arguments
                            |> listMapAndCombineOk
                                (\value ->
                                    value |> type_ moduleOriginLookup
                                )
                        )
                )
        )


swiftTypeParametersToString : List String -> String
swiftTypeParametersToString swiftTypeParameters =
    case swiftTypeParameters of
        [] ->
            ""

        parameter0 :: parameter1Up ->
            "<"
                ++ ((parameter0 :: parameter1Up)
                        |> String.join ", "
                   )
                ++ ">"


printSwiftEnumDeclaration :
    { name : String
    , parameters : List String
    , variants : FastDict.Dict String (List SwiftType)
    }
    -> Print
printSwiftEnumDeclaration swiftEnumType =
    Print.exactly
        ("public indirect enum "
            ++ swiftEnumType.name
            ++ (case swiftEnumType.parameters of
                    [] ->
                        ""

                    parameter0 :: parameter1Up ->
                        "<"
                            ++ ((parameter0 :: parameter1Up)
                                    |> List.map (\parameter -> parameter ++ ": Sendable")
                                    |> String.join ", "
                               )
                            ++ ">"
               )
            ++ ": Sendable {"
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (swiftEnumType.variants
                            |> FastDict.toList
                            |> Print.listMapAndIntersperseAndFlatten
                                (\( name, values ) ->
                                    printSwiftVariant
                                        { name = name
                                        , values = values
                                        }
                                )
                                Print.linebreakIndented
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "}")


printSwiftVariant : { name : String, values : List SwiftType } -> Print
printSwiftVariant swiftVariant =
    Print.exactly ("case " ++ swiftVariant.name)
        |> Print.followedBy
            (case swiftVariant.values of
                [] ->
                    Print.empty

                value0 :: value1Up ->
                    let
                        valuePrints : List Print
                        valuePrints =
                            (value0 :: value1Up)
                                |> List.indexedMap
                                    (\index value ->
                                        let
                                            valuePrint : Print
                                            valuePrint =
                                                value |> printSwiftTypeNotParenthesized
                                        in
                                        Print.exactly
                                            ("_ value" ++ String.fromInt index ++ ":")
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented
                                                    (valuePrint |> Print.lineSpread)
                                                )
                                            |> Print.followedBy
                                                valuePrint
                                    )

                        fullLineSpread : Print.LineSpread
                        fullLineSpread =
                            valuePrints
                                |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    in
                    Print.exactly "("
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.emptyOrLinebreakIndented fullLineSpread
                                    |> Print.followedBy
                                        (valuePrints
                                            |> Print.listMapAndIntersperseAndFlatten
                                                (\valuePrint ->
                                                    Print.withIndentAtNextMultipleOf4
                                                        valuePrint
                                                )
                                                (Print.exactly ","
                                                    |> Print.followedBy
                                                        (Print.spaceOrLinebreakIndented fullLineSpread)
                                                )
                                        )
                                )
                            )
                        |> Print.followedBy
                            (Print.emptyOrLinebreakIndented fullLineSpread)
                        |> Print.followedBy (Print.exactly ")")
            )
        |> Print.followedBy (Print.exactly ";")


typeAliasDeclaration :
    ModuleContext
    -> Elm.Syntax.TypeAlias.TypeAlias
    ->
        Result
            String
            { name : String
            , parameters : List String
            , type_ : SwiftType
            }
typeAliasDeclaration moduleOriginLookup syntaxTypeAlias =
    Result.map
        (\aliasedType ->
            { name =
                syntaxTypeAlias.name
                    |> Elm.Syntax.Node.value
            , parameters =
                syntaxTypeAlias.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            parameter |> uppercaseNameSanitizeForSwift
                        )
            , type_ = aliasedType
            }
        )
        (syntaxTypeAlias.typeAnnotation
            |> type_ moduleOriginLookup
        )


printSwiftTypealiasDeclaration :
    { name : String
    , parameters : List String
    , type_ : SwiftType
    }
    -> Print
printSwiftTypealiasDeclaration swiftTypeAliasDeclaration =
    Print.exactly
        ("public typealias "
            ++ swiftTypeAliasDeclaration.name
            ++ (swiftTypeAliasDeclaration.parameters
                    |> swiftTypeParametersToString
               )
            ++ " ="
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (swiftTypeAliasDeclaration.type_
                            |> printSwiftTypeNotParenthesized
                        )
                )
            )
        |> Print.followedBy
            (Print.exactly ";")


type_ :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Result String SwiftType
type_ moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxType) =
    -- IGNORE TCO
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Unit ->
            Ok swiftTypeUnit

        Elm.Syntax.TypeAnnotation.GenericType variable ->
            if variable |> String.startsWith "number" then
                Ok (SwiftTypeConstruct { moduleOrigin = Nothing, name = "Double", arguments = [] })

            else
                Ok (SwiftTypeVariable (variable |> uppercaseNameSanitizeForSwift))

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ reference) typedArguments ->
            let
                ( qualification, name ) =
                    reference
            in
            case moduleOriginLookup.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup |> FastDict.get reference of
                Nothing ->
                    Err
                        ("could not find module origin of the type reference "
                            ++ qualifiedToString
                                { qualification = qualification
                                , name = name
                                }
                        )

                Just moduleOrigin ->
                    Result.map
                        (\arguments ->
                            let
                                swiftReference : String
                                swiftReference =
                                    case
                                        { moduleOrigin = moduleOrigin
                                        , name = name
                                        }
                                            |> referenceToCoreSwift
                                    of
                                        Just coreSwift ->
                                            coreSwift

                                        Nothing ->
                                            { moduleOrigin = moduleOrigin
                                            , name = name
                                            }
                                                |> uppercaseReferenceToSwiftName
                            in
                            SwiftTypeConstruct
                                { moduleOrigin = Nothing
                                , name = swiftReference
                                , arguments = arguments
                                }
                        )
                        (typedArguments
                            |> listMapAndCombineOk
                                (\argument -> argument |> type_ moduleOriginLookup)
                        )

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    Ok swiftTypeUnit

                [ inParens ] ->
                    type_ moduleOriginLookup inParens

                [ tuplePart0, tuplePart1 ] ->
                    Result.map2
                        (\part0 part1 ->
                            SwiftTypeRecord
                                (FastDict.fromList
                                    [ ( "first", part0 )
                                    , ( "second", part1 )
                                    ]
                                )
                        )
                        (tuplePart0 |> type_ moduleOriginLookup)
                        (tuplePart1 |> type_ moduleOriginLookup)

                [ tuplePart0, tuplePart1, tuplePart2 ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            SwiftTypeRecord
                                (FastDict.fromList
                                    [ ( "first", part0 )
                                    , ( "second", part1 )
                                    , ( "third", part2 )
                                    ]
                                )
                        )
                        (tuplePart0 |> type_ moduleOriginLookup)
                        (tuplePart1 |> type_ moduleOriginLookup)
                        (tuplePart2 |> type_ moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.TypeAnnotation.Record recordFields ->
            Result.map
                (\fields ->
                    SwiftTypeRecord (FastDict.fromList fields)
                )
                (recordFields
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, valueNode )) ->
                            Result.map
                                (\value ->
                                    ( fieldName |> lowercaseNameSanitizeForSwift
                                    , value
                                    )
                                )
                                (valueNode |> type_ moduleOriginLookup)
                        )
                )

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputNode outputNode ->
            Result.map2
                (\input0 outputExpandedReverse ->
                    case outputExpandedReverse of
                        output :: inputLastTo1 ->
                            SwiftTypeFunction
                                { input = input0 :: (inputLastTo1 |> List.reverse)
                                , output = output
                                }

                        -- too lazy to make it non-empty
                        [] ->
                            input0
                )
                (inputNode |> type_ moduleOriginLookup)
                (outputNode
                    |> typeExpandFunctionOutputReverse
                    |> listMapAndCombineOk
                        (\partOfOutput ->
                            type_ moduleOriginLookup partOfOutput
                        )
                )

        Elm.Syntax.TypeAnnotation.GenericRecord _ (Elm.Syntax.Node.Node _ fields) ->
            Err
                ("extensible record types are not supported: { _ | "
                    ++ (fields
                            |> List.map
                                (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ name, _ )) ->
                                    name
                                )
                            |> String.join ", "
                       )
                    ++ " }"
                )


swiftTypeContainedVariables : SwiftType -> FastSet.Set String
swiftTypeContainedVariables swiftType =
    --IGNORE TCO
    case swiftType of
        SwiftTypeVariable name ->
            name |> FastSet.singleton

        SwiftTypeConstruct swiftTypeConstruct ->
            swiftTypeConstruct.arguments
                |> listMapToFastSetsAndUnify swiftTypeContainedVariables

        SwiftTypeRecord fields ->
            fields
                |> FastDict.values
                |> listMapToFastSetsAndUnify swiftTypeContainedVariables

        SwiftTypeFunction swiftTypeFunction ->
            FastSet.union
                (swiftTypeFunction.input
                    |> listMapToFastSetsAndUnify swiftTypeContainedVariables
                )
                (swiftTypeFunction.output |> swiftTypeContainedVariables)


typeExpandFunctionOutputReverse :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeExpandFunctionOutputReverse typeNode =
    typeExpandFunctionOutputIntoReverse [] typeNode


typeExpandFunctionOutputIntoReverse :
    List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeExpandFunctionOutputIntoReverse soFarReverse (Elm.Syntax.Node.Node fullRange syntaxType) =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputNode outputNode ->
            typeExpandFunctionOutputIntoReverse
                (inputNode :: soFarReverse)
                outputNode

        otherType ->
            Elm.Syntax.Node.Node fullRange otherType :: soFarReverse


swiftTypeUnit : SwiftType
swiftTypeUnit =
    SwiftTypeRecord FastDict.empty


printSwiftTypeNotParenthesized : SwiftType -> Print
printSwiftTypeNotParenthesized swiftType =
    -- IGNORE TCO
    case swiftType of
        SwiftTypeVariable variable ->
            Print.exactly variable

        SwiftTypeConstruct typeConstruct ->
            printSwiftTypeConstruct typeConstruct

        SwiftTypeRecord fields ->
            printSwiftTypeRecord fields

        SwiftTypeFunction typeFunction ->
            printSwiftTypeFunction typeFunction


printSwiftTypeFunction :
    { input : List SwiftType, output : SwiftType }
    -> Print
printSwiftTypeFunction typeFunction =
    let
        inputPrints : List Print
        inputPrints =
            typeFunction.input
                |> List.map printSwiftTypeNotParenthesized

        outputPrint : Print
        outputPrint =
            printSwiftTypeNotParenthesized
                typeFunction.output

        inputLineSpread : Print.LineSpread
        inputLineSpread =
            inputPrints
                |> Print.lineSpreadListMapAndCombine
                    Print.lineSpread

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            inputLineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        outputPrint |> Print.lineSpread
                    )
    in
    Print.exactly "@Sendable ("
        |> Print.followedBy
            (inputPrints
                |> Print.listMapAndIntersperseAndFlatten
                    (\inputPrint -> Print.withIndentIncreasedBy 3 inputPrint)
                    (Print.exactly ","
                        |> Print.followedBy
                            (Print.spaceOrLinebreakIndented inputLineSpread)
                    )
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented inputLineSpread)
        |> Print.followedBy (Print.exactly ") ->")
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented fullLineSpread)
        |> Print.followedBy outputPrint


printSwiftTypeRecord : FastDict.Dict String SwiftType -> Print
printSwiftTypeRecord fields =
    case fields |> FastDict.toList of
        [] ->
            Print.exactly "()"

        field0 :: field1Up ->
            Print.exactly "("
                |> Print.followedBy
                    (Print.withIndentIncreasedBy 1
                        ((case field1Up of
                            [] ->
                                [ field0
                                , ( "unusedDummyFieldToMakeSwiftHappy", swiftTypeUnit )
                                ]

                            field1 :: field2Up ->
                                field0 :: field1 :: field2Up
                         )
                            |> Print.listMapAndIntersperseAndFlatten
                                (\( fieldName, fieldValue ) ->
                                    let
                                        fieldValuePrint : Print
                                        fieldValuePrint =
                                            fieldValue |> printSwiftTypeNotParenthesized
                                    in
                                    Print.exactly (fieldName ++ ":")
                                        |> Print.followedBy
                                            (Print.withIndentAtNextMultipleOf4
                                                (Print.spaceOrLinebreakIndented
                                                    (fieldValuePrint |> Print.lineSpread)
                                                    |> Print.followedBy fieldValuePrint
                                                )
                                            )
                                )
                                (Print.exactly ","
                                    |> Print.followedBy Print.linebreakIndented
                                )
                        )
                    )
                |> Print.followedBy Print.linebreakIndented
                |> Print.followedBy (Print.exactly ")")


printSwiftTypeConstruct :
    { moduleOrigin : Maybe String
    , name : String
    , arguments : List SwiftType
    }
    -> Print
printSwiftTypeConstruct typeConstruct =
    let
        referencePrint : Print
        referencePrint =
            Print.exactly
                (swiftReferenceToString
                    { moduleOrigin = typeConstruct.moduleOrigin
                    , name = typeConstruct.name
                    }
                )
    in
    case typeConstruct.arguments of
        [] ->
            referencePrint

        argument0 :: argument1Up ->
            let
                argumentPrints : List Print
                argumentPrints =
                    (argument0 :: argument1Up)
                        |> List.map printSwiftTypeNotParenthesized

                fullLineSpread : Print.LineSpread
                fullLineSpread =
                    argumentPrints
                        |> Print.lineSpreadListMapAndCombine Print.lineSpread
            in
            referencePrint
                |> Print.followedBy (Print.exactly "<")
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.emptyOrLinebreakIndented fullLineSpread
                            |> Print.followedBy
                                (argumentPrints
                                    |> Print.listIntersperseAndFlatten
                                        (Print.exactly ","
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented fullLineSpread)
                                        )
                                )
                        )
                    )
                |> Print.followedBy
                    (Print.emptyOrLinebreakIndented fullLineSpread)
                |> Print.followedBy
                    (Print.exactly ">")


swiftReferenceToString :
    { moduleOrigin : Maybe String
    , name : String
    }
    -> String
swiftReferenceToString reference =
    case reference.moduleOrigin of
        Nothing ->
            reference.name

        Just moduleName ->
            moduleName
                ++ "."
                ++ reference.name


qualifiedToString :
    { qualification : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
qualifiedToString reference =
    case reference.qualification of
        [] ->
            reference.name

        qualificationPart0 :: qualificationPart1Up ->
            ((qualificationPart0 :: qualificationPart1Up)
                |> String.join "."
            )
                ++ "."
                ++ reference.name


stringFirstCharToLower : String -> String
stringFirstCharToLower string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toLower headChar) tailString


stringFirstCharToUpper : String -> String
stringFirstCharToUpper string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toUpper headChar) tailString


printSwiftString : String -> Print
printSwiftString stringContent =
    let
        singleDoubleQuotedStringContentEscaped : String
        singleDoubleQuotedStringContentEscaped =
            stringContent
                |> String.foldl
                    (\contentChar soFar ->
                        soFar
                            ++ singleDoubleQuotedStringCharToEscaped contentChar
                            ++ ""
                    )
                    ""
    in
    Print.exactly
        ("\""
            ++ singleDoubleQuotedStringContentEscaped
            ++ "\""
        )


singleDoubleQuotedStringCharToEscaped : Char -> String
singleDoubleQuotedStringCharToEscaped character =
    case character of
        '"' ->
            "\\\""

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '$' ->
            "\\$"

        '\u{000D}' ->
            "\\r"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


intToHexString : Int -> String
intToHexString int =
    -- IGNORE TCO
    if int < 16 then
        unsafeHexDigitIntToString int

    else
        intToHexString (int // 16)
            ++ unsafeHexDigitIntToString (int |> Basics.remainderBy 16)
            ++ ""


unsafeHexDigitIntToString : Int -> String
unsafeHexDigitIntToString int =
    case int of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "A"

        11 ->
            "B"

        12 ->
            "C"

        13 ->
            "D"

        14 ->
            "E"

        -- 15
        _ ->
            "F"


characterHex : Char -> String
characterHex character =
    String.toUpper
        (intToHexString (Char.toCode character)
            |> String.padLeft 8 '0'
        )


characterIsNotPrint : Char -> Bool
characterIsNotPrint character =
    if
        -- Unicode.getCategory is very expensive so we shortcut if at all possible
        charIsLatinAlphaNumOrUnderscoreFast character
            || (character == ' ')
            || (character == '.')
            || (character == '!')
            || (character == '?')
            || (character == '-')
            || (character == ':')
    then
        False

    else
        case Unicode.getCategory character of
            Nothing ->
                True

            Just category ->
                case category of
                    Unicode.SeparatorLine ->
                        True

                    Unicode.SeparatorParagraph ->
                        True

                    Unicode.OtherControl ->
                        True

                    Unicode.OtherFormat ->
                        True

                    Unicode.OtherSurrogate ->
                        True

                    Unicode.OtherPrivateUse ->
                        True

                    Unicode.OtherNotAssigned ->
                        True

                    Unicode.LetterUppercase ->
                        False

                    Unicode.LetterLowercase ->
                        False

                    Unicode.LetterTitlecase ->
                        False

                    Unicode.MarkNonSpacing ->
                        False

                    Unicode.MarkSpacingCombining ->
                        False

                    Unicode.MarkEnclosing ->
                        False

                    Unicode.NumberDecimalDigit ->
                        False

                    Unicode.NumberLetter ->
                        False

                    Unicode.NumberOther ->
                        False

                    Unicode.SeparatorSpace ->
                        True

                    Unicode.LetterModifier ->
                        False

                    Unicode.LetterOther ->
                        False

                    Unicode.PunctuationConnector ->
                        False

                    Unicode.PunctuationDash ->
                        False

                    Unicode.PunctuationOpen ->
                        False

                    Unicode.PunctuationClose ->
                        False

                    Unicode.PunctuationInitialQuote ->
                        False

                    Unicode.PunctuationFinalQuote ->
                        False

                    Unicode.PunctuationOther ->
                        False

                    Unicode.SymbolMath ->
                        False

                    Unicode.SymbolCurrency ->
                        False

                    Unicode.SymbolModifier ->
                        False

                    Unicode.SymbolOther ->
                        False


charCodeIsLower : Int -> Bool
charCodeIsLower code =
    0x61 <= code && code <= 0x7A


charCodeIsUpper : Int -> Bool
charCodeIsUpper code =
    code <= 0x5A && 0x41 <= code


charCodeIsDigit : Int -> Bool
charCodeIsDigit code =
    code <= 0x39 && 0x30 <= code


charIsLatinAlphaNumOrUnderscoreFast : Char -> Bool
charIsLatinAlphaNumOrUnderscoreFast c =
    let
        code : Int
        code =
            Char.toCode c
    in
    charCodeIsLower code
        || charCodeIsUpper code
        || charCodeIsDigit code
        || -- (c == '_')
           (code == 95)


pattern :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        Result
            String
            { pattern : SwiftPattern
            , introducedVariables : FastSet.Set String
            , patternAliasesToAdd : FastDict.Dict String SwiftExpression
            }
pattern moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.AllPattern ->
            Ok
                { pattern = SwiftPatternIgnore
                , introducedVariables = FastSet.empty
                , patternAliasesToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.UnitPattern ->
            Ok
                { pattern = SwiftPatternIgnore
                , introducedVariables = FastSet.empty
                , patternAliasesToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.CharPattern charValue ->
            Ok
                { pattern = SwiftPatternString (charValue |> String.fromChar)
                , introducedVariables = FastSet.empty
                , patternAliasesToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.StringPattern stringValue ->
            Ok
                { pattern = SwiftPatternString stringValue
                , introducedVariables = FastSet.empty
                , patternAliasesToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.IntPattern intValue ->
            Ok
                { pattern = SwiftPatternFloat (intValue |> Basics.toFloat)
                , introducedVariables = FastSet.empty
                , patternAliasesToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.HexPattern intValue ->
            Ok
                { pattern = SwiftPatternFloat (intValue |> Basics.toFloat)
                , introducedVariables = FastSet.empty
                , patternAliasesToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.FloatPattern _ ->
            Err "float pattern is invalid syntax"

        Elm.Syntax.Pattern.VarPattern variableName ->
            let
                sanitizedVariableName : String
                sanitizedVariableName =
                    variableName |> lowercaseNameSanitizeForSwift
            in
            Ok
                { pattern =
                    SwiftPatternVariable sanitizedVariableName
                , introducedVariables =
                    FastSet.singleton sanitizedVariableName
                , patternAliasesToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            pattern moduleOriginLookup inParens

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [] ->
                    -- should be covered by UnitPattern
                    Ok
                        { pattern = swiftPatternUnit
                        , introducedVariables = FastSet.empty
                        , patternAliasesToAdd = FastDict.empty
                        }

                [ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    pattern moduleOriginLookup inParens

                [ part0Node, part1Node ] ->
                    Result.map2
                        (\part0 part1 ->
                            { pattern =
                                SwiftPatternRecord
                                    (FastDict.fromList
                                        [ ( "first", part0.pattern )
                                        , ( "second", part1.pattern )
                                        ]
                                    )
                            , introducedVariables =
                                FastSet.union
                                    part0.introducedVariables
                                    part1.introducedVariables
                            , patternAliasesToAdd =
                                FastDict.union
                                    part0.patternAliasesToAdd
                                    part1.patternAliasesToAdd
                            }
                        )
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)

                [ part0Node, part1Node, part2Node ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            { pattern =
                                SwiftPatternRecord
                                    (FastDict.fromList
                                        [ ( "first", part0.pattern )
                                        , ( "second", part1.pattern )
                                        , ( "third", part2.pattern )
                                        ]
                                    )
                            , introducedVariables =
                                FastSet.union
                                    part0.introducedVariables
                                    (FastSet.union
                                        part1.introducedVariables
                                        part2.introducedVariables
                                    )
                            , patternAliasesToAdd =
                                FastDict.union
                                    part0.patternAliasesToAdd
                                    (FastDict.union
                                        part1.patternAliasesToAdd
                                        part2.patternAliasesToAdd
                                    )
                            }
                        )
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)
                        (part2Node |> pattern moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    -- invalid syntax
                    Err "too many tuple parts"

        Elm.Syntax.Pattern.RecordPattern fields ->
            let
                fieldNames : List String
                fieldNames =
                    fields
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ fieldName) ->
                                fieldName |> lowercaseNameSanitizeForSwift
                            )
            in
            Ok
                { pattern =
                    SwiftPatternRecord
                        (fieldNames
                            |> listMapToFastDict
                                (\fieldName -> ( fieldName, SwiftPatternVariable fieldName ))
                        )
                , introducedVariables =
                    fieldNames |> FastSet.fromList
                , patternAliasesToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.UnConsPattern headPatternNode tailPatternNode ->
            resultAndThen2
                (\head tail ->
                    Ok
                        { pattern =
                            SwiftPatternVariant
                                { originTypeName = "List_List"
                                , name = "Cons"
                                , values = [ head.pattern, tail.pattern ]
                                }
                        , introducedVariables =
                            FastSet.union
                                head.introducedVariables
                                tail.introducedVariables
                        , patternAliasesToAdd =
                            FastDict.union
                                head.patternAliasesToAdd
                                tail.patternAliasesToAdd
                        }
                )
                (headPatternNode |> pattern moduleOriginLookup)
                (tailPatternNode |> pattern moduleOriginLookup)

        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            Result.map
                (\elements ->
                    { pattern =
                        elements
                            |> List.foldr
                                (\headElement tail ->
                                    SwiftPatternVariant
                                        { originTypeName = "List_List"
                                        , name = "Cons"
                                        , values = [ headElement.pattern, tail ]
                                        }
                                )
                                (SwiftPatternVariant
                                    { originTypeName = "List_List"
                                    , name = "Empty"
                                    , values = []
                                    }
                                )
                    , introducedVariables =
                        elements
                            |> listMapToFastSetsAndUnify .introducedVariables
                    , patternAliasesToAdd =
                        elements
                            |> listMapToFastDictsAndUnify .patternAliasesToAdd
                    }
                )
                (elementPatterns
                    |> listMapAndCombineOk
                        (\element -> element |> pattern moduleOriginLookup)
                )

        Elm.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns ->
            case moduleOriginLookup.variantLookup |> FastDict.get ( syntaxQualifiedNameRef.moduleName, syntaxQualifiedNameRef.name ) of
                Nothing ->
                    Err
                        ("could not find origin choice type for the variant "
                            ++ qualifiedToString
                                { qualification = syntaxQualifiedNameRef.moduleName
                                , name = syntaxQualifiedNameRef.name
                                }
                        )

                Just variantInfo ->
                    case ( variantInfo.moduleOrigin, syntaxQualifiedNameRef.name ) of
                        ( [ "Basics" ], "True" ) ->
                            Ok
                                { pattern = SwiftPatternTrue
                                , introducedVariables = FastSet.empty
                                , patternAliasesToAdd = FastDict.empty
                                }

                        ( [ "Basics" ], "False" ) ->
                            Ok
                                { pattern = SwiftPatternFalse
                                , introducedVariables = FastSet.empty
                                , patternAliasesToAdd = FastDict.empty
                                }

                        _ ->
                            Result.map
                                (\values ->
                                    { pattern =
                                        SwiftPatternVariant
                                            (case
                                                { moduleOrigin = variantInfo.moduleOrigin
                                                , name = syntaxQualifiedNameRef.name
                                                }
                                                    |> variantToCoreSwift
                                             of
                                                Just swiftReference ->
                                                    { originTypeName = swiftReference.originTypeName
                                                    , name = swiftReference.name
                                                    , values = values |> List.map .pattern
                                                    }

                                                Nothing ->
                                                    let
                                                        originTypeReference : { moduleOrigin : Elm.Syntax.ModuleName.ModuleName, name : String }
                                                        originTypeReference =
                                                            { moduleOrigin = variantInfo.moduleOrigin
                                                            , name = variantInfo.originTypeName
                                                            }
                                                    in
                                                    { originTypeName =
                                                        case
                                                            originTypeReference
                                                                |> referenceToCoreSwift
                                                        of
                                                            Just swiftTypeName ->
                                                                swiftTypeName

                                                            Nothing ->
                                                                uppercaseReferenceToSwiftName originTypeReference
                                                    , name =
                                                        syntaxQualifiedNameRef.name
                                                            |> uppercaseNameSanitizeForSwift
                                                    , values = values |> List.map .pattern
                                                    }
                                            )
                                    , introducedVariables =
                                        values
                                            |> listMapToFastSetsAndUnify .introducedVariables
                                    , patternAliasesToAdd =
                                        values
                                            |> listMapToFastDictsAndUnify .patternAliasesToAdd
                                    }
                                )
                                (argumentPatterns
                                    |> listMapAndCombineOk
                                        (\argument -> argument |> pattern moduleOriginLookup)
                                )

        Elm.Syntax.Pattern.AsPattern aliasedPatternNode (Elm.Syntax.Node.Node _ variable) ->
            Result.map
                (\aliasedPattern ->
                    let
                        variableDisambiguated : String
                        variableDisambiguated =
                            variable |> lowercaseNameSanitizeForSwift

                        aliasedPatternWithoutIgnored : SwiftPattern
                        aliasedPatternWithoutIgnored =
                            aliasedPattern.pattern
                                |> swiftPatternSubstituteIgnored
                                    (\ignoredIndex ->
                                        SwiftPatternVariable
                                            ("generated_"
                                                ++ variableDisambiguated
                                                ++ (ignoredIndex |> String.fromInt)
                                            )
                                    )
                    in
                    { pattern = aliasedPatternWithoutIgnored
                    , introducedVariables =
                        aliasedPattern.introducedVariables
                            |> FastSet.insert variableDisambiguated
                    , patternAliasesToAdd =
                        aliasedPattern.patternAliasesToAdd
                            |> FastDict.insert variableDisambiguated
                                (aliasedPatternWithoutIgnored
                                    |> swiftPatternAsExpression
                                )
                    }
                )
                (aliasedPatternNode |> pattern moduleOriginLookup)


swiftPatternSubstituteIgnored : (Int -> SwiftPattern) -> SwiftPattern -> SwiftPattern
swiftPatternSubstituteIgnored ignoreIndexToReplacementPattern swiftPattern =
    swiftPatternSubstituteIgnoredFromIndex 0 ignoreIndexToReplacementPattern swiftPattern


swiftPatternSubstituteIgnoredFromIndex : Int -> (Int -> SwiftPattern) -> SwiftPattern -> SwiftPattern
swiftPatternSubstituteIgnoredFromIndex currentIndex ignoreIndexToReplacementPattern swiftPattern =
    case swiftPattern of
        SwiftPatternIgnore ->
            ignoreIndexToReplacementPattern currentIndex

        SwiftPatternTrue ->
            SwiftPatternTrue

        SwiftPatternFalse ->
            SwiftPatternFalse

        SwiftPatternFloat float ->
            SwiftPatternFloat float

        SwiftPatternString string ->
            SwiftPatternString string

        SwiftPatternVariable name ->
            SwiftPatternVariable name

        SwiftPatternVariant variant ->
            SwiftPatternVariant
                { originTypeName = variant.originTypeName
                , name = variant.name
                , values =
                    variant.values
                        |> List.indexedMap
                            (\valueIndex value ->
                                value
                                    |> swiftPatternSubstituteIgnoredFromIndex
                                        (currentIndex + valueIndex)
                                        ignoreIndexToReplacementPattern
                            )
                }

        SwiftPatternRecord fields ->
            SwiftPatternRecord
                (fields
                    |> FastDict.foldl
                        (\key value soFar ->
                            { index = soFar.index + 1
                            , substituted =
                                soFar.substituted
                                    |> FastDict.insert key
                                        (value
                                            |> swiftPatternSubstituteIgnoredFromIndex
                                                soFar.index
                                                ignoreIndexToReplacementPattern
                                        )
                            }
                        )
                        { index = currentIndex
                        , substituted = FastDict.empty
                        }
                    |> .substituted
                )


variantToCoreSwift :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> Maybe { originTypeName : String, name : String }
variantToCoreSwift reference =
    case reference.moduleOrigin of
        [ "Debug" ] ->
            case reference.name of
                "LT" ->
                    Just { originTypeName = "Basics_Order", name = "LT" }

                "EQ" ->
                    Just { originTypeName = "Basics_Order", name = "EQ" }

                "GT" ->
                    Just { originTypeName = "Basics_Order", name = "GT" }

                _ ->
                    Nothing

        [ "Maybe" ] ->
            case reference.name of
                "Nothing" ->
                    Just { originTypeName = "Maybe_Maybe", name = "Nothing" }

                "Just" ->
                    Just { originTypeName = "Maybe_Maybe", name = "Just" }

                _ ->
                    Nothing

        [ "Parser" ] ->
            case reference.name of
                "Problem" ->
                    Just { originTypeName = "Parser_Problem", name = "Problem" }

                "Expecting" ->
                    Just { originTypeName = "Parser_Problem", name = "Expecting" }

                "ExpectingInt" ->
                    Just { originTypeName = "Parser_Problem", name = "ExpectingInt" }

                "ExpectingHex" ->
                    Just { originTypeName = "Parser_Problem", name = "ExpectingHex" }

                "ExpectingOctal" ->
                    Just { originTypeName = "Parser_Problem", name = "ExpectingOctal" }

                "ExpectingBinary" ->
                    Just { originTypeName = "Parser_Problem", name = "ExpectingBinary" }

                "ExpectingFloat" ->
                    Just { originTypeName = "Parser_Problem", name = "ExpectingFloat" }

                "ExpectingNumber" ->
                    Just { originTypeName = "Parser_Problem", name = "ExpectingNumber" }

                "ExpectingVariable" ->
                    Just { originTypeName = "Parser_Problem", name = "ExpectingVariable" }

                "ExpectingSymbol" ->
                    Just { originTypeName = "Parser_Problem", name = "ExpectingSymbol" }

                "ExpectingKeyword" ->
                    Just { originTypeName = "Parser_Problem", name = "ExpectingKeyword" }

                "ExpectingEnd" ->
                    Just { originTypeName = "Parser_Problem", name = "ExpectingEnd" }

                "UnexpectedChar" ->
                    Just { originTypeName = "Parser_Problem", name = "UnexpectedChar" }

                "BadRepeat" ->
                    Just { originTypeName = "Parser_Problem", name = "BadRepeat" }

                _ ->
                    Nothing

        _ ->
            Nothing


referenceToCoreSwift :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> Maybe String
referenceToCoreSwift reference =
    case reference.moduleOrigin of
        [ "Debug" ] ->
            case reference.name of
                "toString" ->
                    Just "debug_toString"

                "log" ->
                    Just "debug_log"

                "todo" ->
                    Just "debug_todo"

                _ ->
                    Nothing

        [ "Basics" ] ->
            case reference.name of
                "identity" ->
                    Just "basics_identity"

                "always" ->
                    Just "basics_always"

                "compare" ->
                    Just "basics_compare"

                "max" ->
                    Just "basics_max"

                "min" ->
                    Just "basics_min"

                "Order" ->
                    Just "Basics_Order"

                "Bool" ->
                    Just "Bool"

                "True" ->
                    Just "true"

                "False" ->
                    Just "false"

                "not" ->
                    Just "basics_not"

                "xor" ->
                    Just "basics_neq"

                "Int" ->
                    Just "Double"

                "Float" ->
                    Just "Double"

                "ceiling" ->
                    Just "basics_ceiling"

                "floor" ->
                    Just "basics_floor"

                "round" ->
                    Just "basics_round"

                "truncate" ->
                    Just "basics_truncate"

                "negate" ->
                    Just "basics_negate"

                "abs" ->
                    Just "basics_abs"

                "toFloat" ->
                    Just "basics_identity"

                "isNaN" ->
                    Just "basics_isNaN"

                "isInfinite" ->
                    Just "basics_isInfinite"

                "remainderBy" ->
                    Just "basics_remainderBy"

                "modBy" ->
                    Just "basics_modBy"

                "sqrt" ->
                    Just "basics_sqrt"

                _ ->
                    Nothing

        [ "String" ] ->
            case reference.name of
                "String" ->
                    Just "String"

                "isEmpty" ->
                    Just "string_isEmpty"

                "length" ->
                    Just "string_length"

                "append" ->
                    Just "string_append"

                "trim" ->
                    Just "string_trim"

                "trimLeft" ->
                    Just "string_trimLeft"

                "trimRight" ->
                    Just "string_trimRight"

                "left" ->
                    Just "string_left"

                "right" ->
                    Just "string_right"

                "dropLeft" ->
                    Just "string_dropLeft"

                "dropRight" ->
                    Just "string_dropRight"

                "padLeft" ->
                    Just "string_padLeft"

                "padRight" ->
                    Just "string_padRight"

                "toList" ->
                    Just "string_toList"

                "fromList" ->
                    Just "string_fromList"

                "concat" ->
                    Just "string_concat"

                "join" ->
                    Just "string_join"

                "filter" ->
                    Just "string_filter"

                "any" ->
                    Just "string_any"

                "all" ->
                    Just "string_all"

                "map" ->
                    Just "string_map"

                "repeat" ->
                    Just "string_repeat"

                "replace" ->
                    Just "string_replace"

                "lines" ->
                    Just "string_lines"

                "startsWith" ->
                    Just "string_startsWith"

                "endsWith" ->
                    Just "string_endsWith"

                "toInt" ->
                    Just "string_toInt"

                "toFloat" ->
                    Just "string_toFloat"

                "fromInt" ->
                    Just "string_fromFloat"

                "fromFloat" ->
                    Just "string_fromFloat"

                "fromChar" ->
                    Just "string_fromChar"

                "cons" ->
                    Just "string_cons"

                "slice" ->
                    Just "string_slice"

                "split" ->
                    Just "string_split"

                "contains" ->
                    Just "string_contains"

                "reverse" ->
                    Just "string_reverse"

                "toLower" ->
                    Just "string_toLower"

                "toUpper" ->
                    Just "string_toUpper"

                _ ->
                    Nothing

        [ "Char" ] ->
            -- represented as String in swift
            case reference.name of
                "Char" ->
                    Just "Character"

                "toCode" ->
                    Just "char_toCode"

                "fromCode" ->
                    Just "char_fromCode"

                "toLower" ->
                    Just "char_toLower"

                "toUpper" ->
                    Just "char_toUpper"

                "isHexDigit" ->
                    Just "char_isHexDigit"

                _ ->
                    Nothing

        [ "List" ] ->
            case reference.name of
                "List" ->
                    Just "List_List"

                "singleton" ->
                    Just "list_singleton"

                "isEmpty" ->
                    Just "list_isEmpty"

                "length" ->
                    Just "list_length"

                "member" ->
                    Just "list_member"

                "minimum" ->
                    Just "list_minimum"

                "maximum" ->
                    Just "list_maximum"

                "sum" ->
                    Just "list_sum"

                "product" ->
                    Just "list_product"

                "append" ->
                    Just "list_append"

                "concat" ->
                    Just "list_concat"

                "reverse" ->
                    Just "list_reverse"

                "repeat" ->
                    Just "list_repeat"

                "all" ->
                    Just "list_all"

                "any" ->
                    Just "list_any"

                "filter" ->
                    Just "list_filter"

                "filterMap" ->
                    Just "list_filterMap"

                "map" ->
                    Just "list_map"

                "map2" ->
                    Just "list_map2"

                "zip" ->
                    Just "list_zip"

                "unzip" ->
                    Just "list_unzip"

                "concatMap" ->
                    Just "list_concatMap"

                "sort" ->
                    Just "list_sort"

                "sortWith" ->
                    Just "list_sortWith"

                "range" ->
                    Just "list_range"

                "take" ->
                    Just "list_take"

                "drop" ->
                    Just "list_drop"

                "intersperse" ->
                    Just "list_intersperse"

                "foldl" ->
                    Just "list_foldl"

                "foldr" ->
                    Just "list_foldr"

                _ ->
                    Nothing

        [ "Maybe" ] ->
            case reference.name of
                "Maybe" ->
                    Just "Maybe_Maybe"

                _ ->
                    Nothing

        [ "Parser" ] ->
            case reference.name of
                -- refers to either a type or variant
                "Problem" ->
                    Just "Parser_Problem"

                _ ->
                    Nothing

        _ ->
            Nothing


lowercaseReferenceToSwiftName :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
lowercaseReferenceToSwiftName reference =
    (reference.moduleOrigin
        |> String.concat
        |> stringFirstCharToLower
    )
        ++ "_"
        ++ reference.name
        |> lowercaseNameSanitizeForSwift


uppercaseReferenceToSwiftName :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
uppercaseReferenceToSwiftName reference =
    (reference.moduleOrigin
        |> String.concat
    )
        ++ "_"
        ++ (reference.name |> stringFirstCharToUpper)
        |> uppercaseNameSanitizeForSwift


printSwiftPatternNotParenthesized : SwiftPattern -> Print
printSwiftPatternNotParenthesized swiftPattern =
    -- IGNORE TCO
    case swiftPattern of
        SwiftPatternIgnore ->
            Print.exactly "_"

        SwiftPatternTrue ->
            Print.exactly "true"

        SwiftPatternFalse ->
            Print.exactly "false"

        SwiftPatternFloat floatValue ->
            Print.exactly (swiftNumberLiteralToString floatValue)

        SwiftPatternString string ->
            printSwiftString string

        SwiftPatternVariable name ->
            Print.exactly name

        SwiftPatternVariant patternVariant ->
            Print.exactly
                (patternVariant.originTypeName
                    ++ "."
                    ++ patternVariant.name
                )
                |> Print.followedBy
                    (case patternVariant.values of
                        [] ->
                            Print.empty

                        variantValue0 :: variantValue1Up ->
                            Print.exactly "("
                                |> Print.followedBy
                                    ((variantValue0 :: variantValue1Up)
                                        |> Print.listMapAndIntersperseAndFlatten
                                            printSwiftPatternNotParenthesized
                                            (Print.exactly ", ")
                                    )
                                |> Print.followedBy (Print.exactly ")")
                    )

        SwiftPatternRecord fields ->
            printSwiftPatternRecord fields


printSwiftPatternRecord : FastDict.Dict String SwiftPattern -> Print
printSwiftPatternRecord fields =
    Print.exactly "("
        |> Print.followedBy
            ((case fields |> FastDict.toList of
                [] ->
                    []

                [ singleField ] ->
                    [ singleField
                    , ( "unusedDummyFieldToMakeSwiftHappy", swiftPatternUnit )
                    ]

                field0 :: field1 :: field2Up ->
                    field0 :: field1 :: field2Up
             )
                |> Print.listMapAndIntersperseAndFlatten
                    (\( fieldName, fieldValue ) ->
                        Print.exactly (fieldName ++ ": ")
                            |> Print.followedBy
                                (printSwiftPatternNotParenthesized fieldValue)
                    )
                    (Print.exactly ", ")
            )
        |> Print.followedBy (Print.exactly ")")


swiftPatternUnit : SwiftPattern
swiftPatternUnit =
    SwiftPatternRecord FastDict.empty


printSwiftExpressionRecord : FastDict.Dict String SwiftExpression -> Print
printSwiftExpressionRecord syntaxRecordFields =
    case syntaxRecordFields |> FastDict.toList of
        [] ->
            Print.exactly "()"

        field0 :: field1Up ->
            let
                fieldsPrints : List Print
                fieldsPrints =
                    (case field1Up of
                        [] ->
                            [ field0
                            , ( "unusedDummyFieldToMakeSwiftHappy", swiftExpressionUnit )
                            ]

                        field1 :: field2Up ->
                            field0 :: field1 :: field2Up
                    )
                        |> List.map
                            (\( fieldName, fieldValue ) ->
                                let
                                    fieldValuePrint : Print
                                    fieldValuePrint =
                                        printSwiftExpressionNotParenthesized fieldValue
                                in
                                Print.exactly (fieldName ++ ":")
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.spaceOrLinebreakIndented
                                                (fieldValuePrint |> Print.lineSpread)
                                                |> Print.followedBy fieldValuePrint
                                            )
                                        )
                            )

                fullLineSpread : Print.LineSpread
                fullLineSpread =
                    fieldsPrints
                        |> Print.lineSpreadListMapAndCombine Print.lineSpread
            in
            Print.exactly "("
                |> Print.followedBy
                    (Print.withIndentIncreasedBy 1
                        (fieldsPrints
                            |> Print.listIntersperseAndFlatten
                                (Print.exactly ","
                                    |> Print.followedBy
                                        (Print.spaceOrLinebreakIndented fullLineSpread)
                                )
                        )
                    )
                |> Print.followedBy
                    (Print.emptyOrLinebreakIndented fullLineSpread)
                |> Print.followedBy (Print.exactly ")")


printParenthesized : { opening : String, closing : String, inner : Print } -> Print
printParenthesized config =
    Print.exactly config.opening
        |> Print.followedBy
            (Print.withIndentIncreasedBy 1
                config.inner
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented
                (config.inner |> Print.lineSpread)
            )
        |> Print.followedBy (Print.exactly config.closing)


{-| Transpile a list of [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)s
across multiple modules to value, function and type declarations.
Declarations that use unsupported stuff like parser kernel code (directly or indirectly)
will not be present in the final declarations.
Their errors can be found alongside the valid transpiled declarations.

The given list of files must also include files from used dependencies
including `elm/core`.

-}
modules :
    List Elm.Syntax.File.File
    ->
        { errors : List String
        , declarations :
            { valuesAndFunctions :
                FastDict.Dict
                    String
                    { parameters : List (Maybe String)
                    , result : SwiftExpression
                    , type_ : Maybe SwiftType
                    }
            , typeAliases :
                FastDict.Dict
                    String
                    { parameters : List String
                    , type_ : SwiftType
                    }
            , choiceTypes :
                FastDict.Dict
                    String
                    { parameters : List String
                    , variants : FastDict.Dict String (List SwiftType)
                    }
            }
        }
modules syntaxDeclarationsIncludingOverwrittenOnes =
    let
        syntaxModules : List Elm.Syntax.File.File
        syntaxModules =
            syntaxDeclarationsIncludingOverwrittenOnes
                |> List.filter
                    (\syntaxModule ->
                        case
                            syntaxModule.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> moduleHeaderName
                        of
                            [ "Basics" ] ->
                                False

                            [ "Array" ] ->
                                False

                            -- https://github.com/elm/core/blob/1.0.5/src/Elm/JsArray.elm
                            [ "Elm", "JsArray" ] ->
                                False

                            [ "Bitwise" ] ->
                                False

                            [ "Debug" ] ->
                                False

                            [ "Char" ] ->
                                False

                            [ "String" ] ->
                                False

                            [ "List" ] ->
                                False

                            [ "Dict" ] ->
                                False

                            [ "Set" ] ->
                                False

                            [ "Platform" ] ->
                                False

                            [ "Platform", "Cmd" ] ->
                                False

                            [ "Platform", "Sub" ] ->
                                False

                            [ "Process" ] ->
                                False

                            [ "Task" ] ->
                                False

                            [ "Json", "Decode" ] ->
                                False

                            [ "Json", "Encode" ] ->
                                False

                            [ "Parser" ] ->
                                False

                            [ "Parser", "Advanced" ] ->
                                False

                            [ "Regex" ] ->
                                False

                            [ "File" ] ->
                                False

                            [ "Bytes" ] ->
                                False

                            [ "Bytes", "Encode" ] ->
                                False

                            [ "Bytes", "Decode" ] ->
                                False

                            [ "Http" ] ->
                                False

                            [ "VirtualDom" ] ->
                                False

                            [ "Browser" ] ->
                                False

                            [ "Browser", "Events" ] ->
                                False

                            [ "Browser", "Navigation" ] ->
                                False

                            [ "Browser", "Dom" ] ->
                                False

                            -- https://github.com/elm/browser/blob/master/src/Browser/AnimationManager.elm
                            [ "Browser", "AnimationManager" ] ->
                                False

                            -- https://github.com/elm/browser/tree/master/src/Debugger
                            [ "Debugger", "Expando" ] ->
                                False

                            [ "Debugger", "History" ] ->
                                False

                            [ "Debugger", "Main" ] ->
                                False

                            [ "Debugger", "Metadata" ] ->
                                False

                            [ "Debugger", "Overlay" ] ->
                                False

                            [ "Debugger", "Report" ] ->
                                False

                            [ "Html" ] ->
                                False

                            [ "Html", "Attributes" ] ->
                                False

                            [ "Html", "Events" ] ->
                                False

                            [ "Html", "Keyed" ] ->
                                False

                            [ "Html", "Lazy" ] ->
                                False

                            [ "Svg" ] ->
                                False

                            [ "Svg", "Attributes" ] ->
                                False

                            [ "Svg", "Events" ] ->
                                False

                            [ "Svg", "Keyed" ] ->
                                False

                            [ "Svg", "Lazy" ] ->
                                False

                            [ "Time" ] ->
                                False

                            [ "Random" ] ->
                                False

                            [ "Url" ] ->
                                False

                            [ "Url", "Builder" ] ->
                                False

                            [ "Url", "Parser" ] ->
                                False

                            [ "Url", "Parser", "Query" ] ->
                                False

                            [ "Markdown" ] ->
                                False

                            [ "Benchmark" ] ->
                                False

                            [ "WebGL" ] ->
                                False

                            [ "WebGL", "Settings" ] ->
                                False

                            [ "WebGL", "Settings", "Blend" ] ->
                                False

                            [ "WebGL", "Settings", "DepthTest" ] ->
                                False

                            [ "WebGL", "Settings", "StencilTest" ] ->
                                False

                            [ "WebGL", "Texture" ] ->
                                False

                            [ "Math", "Matrix4" ] ->
                                False

                            [ "Math", "Vector2" ] ->
                                False

                            [ "Math", "Vector3" ] ->
                                False

                            [ "Math", "Vector4" ] ->
                                False

                            _ ->
                                True
                    )

        moduleMembers :
            FastDict.Dict
                Elm.Syntax.ModuleName.ModuleName
                { valueOrFunctionOrTypeAliasNames : FastSet.Set String
                , enumTypesExposingVariants :
                    FastDict.Dict String (FastDict.Dict String { valueCount : Int })
                }
        moduleMembers =
            syntaxDeclarationsIncludingOverwrittenOnes
                |> List.filter
                    (\syntaxModule ->
                        -- remove those modules we don't have a replacement for, yet
                        case
                            syntaxModule.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> moduleHeaderName
                        of
                            [ "Array" ] ->
                                False

                            -- https://github.com/elm/core/blob/1.0.5/src/Elm/JsArray.elm
                            [ "Elm", "JsArray" ] ->
                                False

                            [ "Bitwise" ] ->
                                False

                            [ "Debug" ] ->
                                False

                            [ "Set" ] ->
                                False

                            [ "Platform" ] ->
                                False

                            [ "Platform", "Cmd" ] ->
                                False

                            [ "Platform", "Sub" ] ->
                                False

                            [ "Process" ] ->
                                False

                            [ "Task" ] ->
                                False

                            [ "Json", "Decode" ] ->
                                False

                            [ "Json", "Encode" ] ->
                                False

                            [ "Parser" ] ->
                                False

                            [ "Parser", "Advanced" ] ->
                                False

                            [ "Regex" ] ->
                                False

                            [ "File" ] ->
                                False

                            [ "Bytes" ] ->
                                False

                            [ "Bytes", "Encode" ] ->
                                False

                            [ "Bytes", "Decode" ] ->
                                False

                            [ "Http" ] ->
                                False

                            [ "VirtualDom" ] ->
                                False

                            [ "Browser" ] ->
                                False

                            [ "Browser", "Events" ] ->
                                False

                            [ "Browser", "Navigation" ] ->
                                False

                            [ "Browser", "Dom" ] ->
                                False

                            -- https://github.com/elm/browser/blob/master/src/Browser/AnimationManager.elm
                            [ "Browser", "AnimationManager" ] ->
                                False

                            -- https://github.com/elm/browser/tree/master/src/Debugger
                            [ "Debugger", "Expando" ] ->
                                False

                            [ "Debugger", "History" ] ->
                                False

                            [ "Debugger", "Main" ] ->
                                False

                            [ "Debugger", "Metadata" ] ->
                                False

                            [ "Debugger", "Overlay" ] ->
                                False

                            [ "Debugger", "Report" ] ->
                                False

                            [ "Html" ] ->
                                False

                            [ "Html", "Attributes" ] ->
                                False

                            [ "Html", "Events" ] ->
                                False

                            [ "Html", "Keyed" ] ->
                                False

                            [ "Html", "Lazy" ] ->
                                False

                            [ "Svg" ] ->
                                False

                            [ "Svg", "Attributes" ] ->
                                False

                            [ "Svg", "Events" ] ->
                                False

                            [ "Svg", "Keyed" ] ->
                                False

                            [ "Svg", "Lazy" ] ->
                                False

                            [ "Time" ] ->
                                False

                            [ "Random" ] ->
                                False

                            [ "Url" ] ->
                                False

                            [ "Url", "Builder" ] ->
                                False

                            [ "Url", "Parser" ] ->
                                False

                            [ "Url", "Parser", "Query" ] ->
                                False

                            [ "Markdown" ] ->
                                False

                            [ "Benchmark" ] ->
                                False

                            [ "WebGL" ] ->
                                False

                            [ "WebGL", "Settings" ] ->
                                False

                            [ "WebGL", "Settings", "Blend" ] ->
                                False

                            [ "WebGL", "Settings", "DepthTest" ] ->
                                False

                            [ "WebGL", "Settings", "StencilTest" ] ->
                                False

                            [ "WebGL", "Texture" ] ->
                                False

                            [ "Math", "Matrix4" ] ->
                                False

                            [ "Math", "Vector2" ] ->
                                False

                            [ "Math", "Vector3" ] ->
                                False

                            [ "Math", "Vector4" ] ->
                                False

                            _ ->
                                True
                    )
                |> List.foldl
                    (\syntaxModule acrossModulesSoFar ->
                        acrossModulesSoFar
                            |> FastDict.insert
                                (syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName
                                )
                                (syntaxModule.declarations
                                    |> List.foldl
                                        (\(Elm.Syntax.Node.Node _ declaration) membersSoFar ->
                                            case declaration of
                                                Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                            |> FastSet.insert
                                                                (syntaxValueOrFunctionDeclaration.declaration
                                                                    |> Elm.Syntax.Node.value
                                                                    |> .name
                                                                    |> Elm.Syntax.Node.value
                                                                )
                                                    , enumTypesExposingVariants =
                                                        membersSoFar.enumTypesExposingVariants
                                                    }

                                                Elm.Syntax.Declaration.CustomTypeDeclaration syntaxEnumTypeDeclaration ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                    , enumTypesExposingVariants =
                                                        membersSoFar.enumTypesExposingVariants
                                                            |> FastDict.insert
                                                                (syntaxEnumTypeDeclaration.name |> Elm.Syntax.Node.value)
                                                                (syntaxEnumTypeDeclaration.constructors
                                                                    |> List.foldl
                                                                        (\(Elm.Syntax.Node.Node _ variant) variantNamesSoFar ->
                                                                            variantNamesSoFar
                                                                                |> FastDict.insert
                                                                                    (variant.name
                                                                                        |> Elm.Syntax.Node.value
                                                                                    )
                                                                                    { valueCount =
                                                                                        variant.arguments |> List.length
                                                                                    }
                                                                        )
                                                                        FastDict.empty
                                                                )
                                                    }

                                                Elm.Syntax.Declaration.AliasDeclaration typeAlias ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                            |> FastSet.insert
                                                                (typeAlias.name
                                                                    |> Elm.Syntax.Node.value
                                                                )
                                                    , enumTypesExposingVariants =
                                                        membersSoFar.enumTypesExposingVariants
                                                    }

                                                Elm.Syntax.Declaration.PortDeclaration _ ->
                                                    -- not supported
                                                    membersSoFar

                                                Elm.Syntax.Declaration.InfixDeclaration _ ->
                                                    membersSoFar

                                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                                    -- invalid syntax
                                                    membersSoFar
                                        )
                                        { valueOrFunctionOrTypeAliasNames = FastSet.empty
                                        , enumTypesExposingVariants = FastDict.empty
                                        }
                                )
                    )
                    FastDict.empty

        valuesThatNeedToBeLazilyConstructed : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
        valuesThatNeedToBeLazilyConstructed =
            syntaxModules
                |> listMapToFastSetsAndUnify
                    (\syntaxModule ->
                        let
                            moduleName : Elm.Syntax.ModuleName.ModuleName
                            moduleName =
                                syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName
                        in
                        syntaxModule.declarations
                            |> List.filterMap
                                (\(Elm.Syntax.Node.Node _ declaration) ->
                                    case declaration of
                                        Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                            let
                                                implementation : Elm.Syntax.Expression.FunctionImplementation
                                                implementation =
                                                    syntaxValueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value
                                            in
                                            case implementation.arguments of
                                                _ :: _ ->
                                                    Nothing

                                                [] ->
                                                    case syntaxValueOrFunctionDeclaration.signature of
                                                        Nothing ->
                                                            Nothing

                                                        Just (Elm.Syntax.Node.Node _ syntaxType) ->
                                                            if
                                                                syntaxType.typeAnnotation
                                                                    |> typeContainedVariables
                                                                    |> FastSet.isEmpty
                                                            then
                                                                Nothing

                                                            else
                                                                Just
                                                                    ( moduleName
                                                                    , implementation.name
                                                                        |> Elm.Syntax.Node.value
                                                                    )

                                        _ ->
                                            Nothing
                                )
                            |> FastSet.fromList
                    )

        swiftDeclarations :
            { errors : List String
            , declarations :
                { valuesAndFunctions :
                    FastDict.Dict
                        String
                        { parameters : List (Maybe String)
                        , result : SwiftExpression
                        , type_ : Maybe SwiftType
                        }
                , typeAliases :
                    FastDict.Dict
                        String
                        { parameters : List String
                        , type_ : SwiftType
                        }
                , choiceTypes :
                    FastDict.Dict
                        String
                        { parameters : List String
                        , variants : FastDict.Dict String (List SwiftType)
                        }
                }
            }
        swiftDeclarations =
            syntaxModules
                |> List.foldr
                    (\syntaxModule soFarAcrossModules ->
                        let
                            moduleName : Elm.Syntax.ModuleName.ModuleName
                            moduleName =
                                syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName

                            importContext :
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
                                    FastDict.Dict
                                        ( Elm.Syntax.ModuleName.ModuleName, String )
                                        Elm.Syntax.ModuleName.ModuleName
                                , variantLookup :
                                    FastDict.Dict
                                        ( Elm.Syntax.ModuleName.ModuleName, String )
                                        { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                                        , valueCount : Int
                                        , originTypeName : String
                                        }
                                }
                            importContext =
                                syntaxModule.imports
                                    |> importsToModuleContext moduleMembers

                            createdModuleContext : ModuleContext
                            createdModuleContext =
                                moduleContextMerge
                                    importContext
                                    (case moduleMembers |> FastDict.get moduleName of
                                        Nothing ->
                                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                                FastDict.empty
                                            , variantLookup = FastDict.empty
                                            }

                                        Just moduleLocalNames ->
                                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                                FastSet.union
                                                    moduleLocalNames.valueOrFunctionOrTypeAliasNames
                                                    (moduleLocalNames.enumTypesExposingVariants
                                                        |> FastDict.foldl
                                                            (\enumTypeName _ soFar ->
                                                                soFar |> FastSet.insert enumTypeName
                                                            )
                                                            FastSet.empty
                                                    )
                                                    |> FastSet.foldl
                                                        (\name soFar ->
                                                            soFar
                                                                |> FastDict.insert ( [], name )
                                                                    moduleName
                                                        )
                                                        FastDict.empty
                                            , variantLookup =
                                                moduleLocalNames.enumTypesExposingVariants
                                                    |> FastDict.foldl
                                                        (\originTypeName variantNames soFarAcrossEnumTypes ->
                                                            variantNames
                                                                |> FastDict.foldl
                                                                    (\name info soFar ->
                                                                        soFar
                                                                            |> FastDict.insert ( [], name )
                                                                                { moduleOrigin = moduleName
                                                                                , valueCount = info.valueCount
                                                                                , originTypeName = originTypeName
                                                                                }
                                                                    )
                                                                    soFarAcrossEnumTypes
                                                        )
                                                        FastDict.empty
                                            }
                                    )
                        in
                        syntaxModule.declarations
                            |> List.foldr
                                (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                                    case declaration of
                                        Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                            case
                                                syntaxValueOrFunctionDeclaration
                                                    |> valueOrFunctionDeclaration
                                                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                                            createdModuleContext.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                                                        , variantLookup = createdModuleContext.variantLookup
                                                        , valuesThatNeedToBeLazilyConstructed = valuesThatNeedToBeLazilyConstructed
                                                        }
                                            of
                                                Ok swiftValueOrFunctionDeclaration ->
                                                    { errors = soFar.errors
                                                    , declarations =
                                                        { typeAliases = soFar.declarations.typeAliases
                                                        , choiceTypes = soFar.declarations.choiceTypes
                                                        , valuesAndFunctions =
                                                            soFar.declarations.valuesAndFunctions
                                                                |> FastDict.insert
                                                                    ({ moduleOrigin = moduleName
                                                                     , name = swiftValueOrFunctionDeclaration.name
                                                                     }
                                                                        |> lowercaseReferenceToSwiftName
                                                                    )
                                                                    { parameters = swiftValueOrFunctionDeclaration.parameters
                                                                    , result = swiftValueOrFunctionDeclaration.result
                                                                    , type_ = swiftValueOrFunctionDeclaration.type_
                                                                    }
                                                        }
                                                    }

                                                Err error ->
                                                    { declarations = soFar.declarations
                                                    , errors = error :: soFar.errors
                                                    }

                                        Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAliasDeclaration ->
                                            case syntaxTypeAliasDeclaration |> typeAliasDeclaration createdModuleContext of
                                                Ok swiftTypeAliasDeclaration ->
                                                    { errors = soFar.errors
                                                    , declarations =
                                                        { valuesAndFunctions = soFar.declarations.valuesAndFunctions
                                                        , choiceTypes = soFar.declarations.choiceTypes
                                                        , typeAliases =
                                                            soFar.declarations.typeAliases
                                                                |> FastDict.insert
                                                                    ({ moduleOrigin = moduleName
                                                                     , name = swiftTypeAliasDeclaration.name
                                                                     }
                                                                        |> uppercaseReferenceToSwiftName
                                                                    )
                                                                    { parameters = swiftTypeAliasDeclaration.parameters
                                                                    , type_ = swiftTypeAliasDeclaration.type_
                                                                    }
                                                        }
                                                    }

                                                Err error ->
                                                    { declarations = soFar.declarations
                                                    , errors = error :: soFar.errors
                                                    }

                                        Elm.Syntax.Declaration.CustomTypeDeclaration syntaxEnumTypeDeclaration ->
                                            case syntaxEnumTypeDeclaration.name |> Elm.Syntax.Node.value of
                                                "Maybe" ->
                                                    soFar

                                                _ ->
                                                    case syntaxEnumTypeDeclaration |> enumTypeDeclaration createdModuleContext of
                                                        Ok swiftTypeAliasDeclaration ->
                                                            { errors = soFar.errors
                                                            , declarations =
                                                                { valuesAndFunctions = soFar.declarations.valuesAndFunctions
                                                                , typeAliases = soFar.declarations.typeAliases
                                                                , choiceTypes =
                                                                    soFar.declarations.choiceTypes
                                                                        |> FastDict.insert
                                                                            ({ moduleOrigin = moduleName
                                                                             , name = swiftTypeAliasDeclaration.name
                                                                             }
                                                                                |> uppercaseReferenceToSwiftName
                                                                            )
                                                                            { parameters = swiftTypeAliasDeclaration.parameters
                                                                            , variants =
                                                                                swiftTypeAliasDeclaration.variants
                                                                                    |> FastDict.foldl
                                                                                        (\variantName maybeValue variantsSoFar ->
                                                                                            variantsSoFar
                                                                                                |> FastDict.insert
                                                                                                    (variantName
                                                                                                        |> uppercaseNameSanitizeForSwift
                                                                                                    )
                                                                                                    maybeValue
                                                                                        )
                                                                                        FastDict.empty
                                                                            }
                                                                }
                                                            }

                                                        Err error ->
                                                            { declarations = soFar.declarations
                                                            , errors = error :: soFar.errors
                                                            }

                                        Elm.Syntax.Declaration.PortDeclaration _ ->
                                            soFar

                                        Elm.Syntax.Declaration.InfixDeclaration _ ->
                                            soFar

                                        Elm.Syntax.Declaration.Destructuring _ _ ->
                                            soFar
                                )
                                soFarAcrossModules
                    )
                    { errors = []
                    , declarations =
                        { valuesAndFunctions = FastDict.empty
                        , typeAliases = FastDict.empty
                        , choiceTypes = FastDict.empty
                        }
                    }
    in
    { declarations =
        { valuesAndFunctions =
            swiftDeclarations.declarations.valuesAndFunctions
                |> FastDict.map
                    (\_ valueOrFunctionInfo ->
                        { type_ = valueOrFunctionInfo.type_
                        , parameters = valueOrFunctionInfo.parameters
                        , result = valueOrFunctionInfo.result
                        }
                    )
        , choiceTypes =
            swiftDeclarations.declarations.choiceTypes
                |> FastDict.map
                    (\_ typeAliasInfo ->
                        { parameters = typeAliasInfo.parameters
                        , variants = typeAliasInfo.variants
                        }
                    )
        , typeAliases =
            swiftDeclarations.declarations.typeAliases
                |> FastDict.map
                    (\_ typeAliasInfo ->
                        { parameters = typeAliasInfo.parameters
                        , type_ = typeAliasInfo.type_
                        }
                    )
        }
    , errors = swiftDeclarations.errors
    }


typeContainedVariables :
    Elm.Syntax.Node.Node
        Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> FastSet.Set String
typeContainedVariables (Elm.Syntax.Node.Node _ syntaxType) =
    -- IGNORE TCO
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Unit ->
            FastSet.empty

        Elm.Syntax.TypeAnnotation.GenericType variableName ->
            if variableName |> String.startsWith "number" then
                FastSet.empty

            else
                variableName |> FastSet.singleton

        Elm.Syntax.TypeAnnotation.Typed _ argumentNodes ->
            argumentNodes
                |> listMapToFastSetsAndUnify typeContainedVariables

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            parts
                |> listMapToFastSetsAndUnify typeContainedVariables

        Elm.Syntax.TypeAnnotation.Record fields ->
            fields
                |> listMapToFastSetsAndUnify
                    (\(Elm.Syntax.Node.Node _ ( _, value )) ->
                        value |> typeContainedVariables
                    )

        Elm.Syntax.TypeAnnotation.GenericRecord (Elm.Syntax.Node.Node _ recordVariableName) (Elm.Syntax.Node.Node _ fields) ->
            FastSet.insert recordVariableName
                (fields
                    |> listMapToFastSetsAndUnify
                        (\(Elm.Syntax.Node.Node _ ( _, value )) ->
                            value |> typeContainedVariables
                        )
                )

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
            FastSet.union
                (input |> typeContainedVariables)
                (output |> typeContainedVariables)


moduleHeaderName : Elm.Syntax.Module.Module -> Elm.Syntax.ModuleName.ModuleName
moduleHeaderName moduleHeader =
    case moduleHeader of
        Elm.Syntax.Module.NormalModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.PortModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.EffectModule header ->
            header.moduleName |> Elm.Syntax.Node.value


valueOrFunctionDeclaration :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            , originTypeName : String
            }
    }
    -> Elm.Syntax.Expression.Function
    ->
        Result
            String
            { name : String
            , parameters : List (Maybe String)
            , result : SwiftExpression
            , type_ : Maybe SwiftType
            }
valueOrFunctionDeclaration context syntaxDeclarationValueOrFunction =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationValueOrFunction.declaration |> Elm.Syntax.Node.value
    in
    resultAndThen2
        (\parameters maybeType ->
            Result.map
                (\result ->
                    let
                        resultIncludingAliasPatternAssignments : SwiftExpression
                        resultIncludingAliasPatternAssignments =
                            parameters
                                |> listMapToFastDictsAndUnify .patternAliasesToAdd
                                |> FastDict.foldl
                                    (\variableName expressionToAssign soFar ->
                                        SwiftExpressionWithLocalDeclaration
                                            { declaration =
                                                { name = variableName
                                                , result = expressionToAssign
                                                , parameters = []
                                                , type_ = Nothing
                                                }
                                            , result = soFar
                                            }
                                    )
                                    result

                        parametersAndDestructuring :
                            { parameters : List (Maybe String)
                            , destructuring :
                                Maybe
                                    { expression : SwiftExpression
                                    , pattern : SwiftPattern
                                    }
                            }
                        parametersAndDestructuring =
                            parametersToSwiftAndDestructuring
                                (parameters |> List.map .pattern)
                    in
                    { name =
                        implementation.name
                            |> Elm.Syntax.Node.value
                    , type_ = maybeType
                    , parameters = parametersAndDestructuring.parameters
                    , result =
                        case parametersAndDestructuring.destructuring of
                            Nothing ->
                                resultIncludingAliasPatternAssignments

                            Just destructuring ->
                                SwiftExpressionSwitch
                                    { matched = destructuring.expression
                                    , case0 =
                                        { pattern = destructuring.pattern
                                        , result = resultIncludingAliasPatternAssignments
                                        }
                                    , case1Up = []
                                    }
                    }
                )
                (implementation.expression
                    |> expression
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                        , valuesThatNeedToBeLazilyConstructed =
                            context.valuesThatNeedToBeLazilyConstructed
                        , variantLookup = context.variantLookup
                        , variablesFromWithinDeclarationInScope =
                            parameters
                                |> listMapToFastSetsAndUnify .introducedVariables
                        }
                )
        )
        (implementation.arguments
            |> listMapAndCombineOk
                (\p ->
                    p
                        |> pattern
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                )
        )
        (case syntaxDeclarationValueOrFunction.signature of
            Nothing ->
                Ok Nothing

            Just (Elm.Syntax.Node.Node _ signature) ->
                Result.map Just
                    (signature.typeAnnotation
                        |> type_
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                    )
        )


lowercaseNameSanitizeForSwift : String -> String
lowercaseNameSanitizeForSwift lowercaseName =
    let
        lowercaseNameWithValidCharacters : String
        lowercaseNameWithValidCharacters =
            lowercaseName
                |> stringFirstCharToLower
    in
    if swiftReservedWords |> FastSet.member lowercaseNameWithValidCharacters then
        lowercaseNameWithValidCharacters ++ "_"

    else
        lowercaseNameWithValidCharacters


uppercaseNameSanitizeForSwift : String -> String
uppercaseNameSanitizeForSwift uppercaseName =
    let
        uppercaseNameWithValidCharacters : String
        uppercaseNameWithValidCharacters =
            uppercaseName
                |> stringFirstCharToUpper
    in
    if swiftReservedWords |> FastSet.member uppercaseNameWithValidCharacters then
        uppercaseNameWithValidCharacters ++ "_"

    else
        uppercaseNameWithValidCharacters


swiftReservedWords : FastSet.Set String
swiftReservedWords =
    -- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/lexicalstructure/#Keywords-and-Punctuation
    FastSet.fromList
        [ -- Keywords used in declarations
          "associatedtype"
        , "borrowing"
        , "class"
        , "consuming"
        , "deinit"
        , "enum"
        , "extension"
        , "fileprivate"
        , "func"
        , "import"
        , "init"
        , "inout"
        , "internal"
        , "let"
        , "nonisolated"
        , "open"
        , "operator"
        , "private"
        , "precedencegroup"
        , "protocol"
        , "public"
        , "rethrows"
        , "static"
        , "struct"
        , "subscript"
        , "typealias"
        , "var"

        -- Keywords used in statements
        , "break"
        , "case"
        , "catch"
        , "continue"
        , "default"
        , "defer"
        , "do"
        , "else"
        , "fallthrough"
        , "for"
        , "guard"
        , "if"
        , "in"
        , "repeat"
        , "return"
        , "throw"
        , "switch"
        , "where"
        , "while"

        -- Keywords used in expressions and types
        , "Any"
        , "as"
        , "await"
        , "catch"
        , "false"
        , "is"
        , "nil"
        , "rethrows"
        , "self"
        , "Self"
        , "super"
        , "throw"
        , "throws"
        , "true"
        , "try"
        ]


expressionContextAddVariablesInScope :
    FastSet.Set String
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , valuesThatNeedToBeLazilyConstructed :
            FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                , originTypeName : String
                }
        , variablesFromWithinDeclarationInScope : FastSet.Set String
        }
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , valuesThatNeedToBeLazilyConstructed :
            FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                , originTypeName : String
                }
        , variablesFromWithinDeclarationInScope : FastSet.Set String
        }
expressionContextAddVariablesInScope additionalVariablesInScope context =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
        context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
    , valuesThatNeedToBeLazilyConstructed =
        context.valuesThatNeedToBeLazilyConstructed
    , variantLookup =
        context.variantLookup
    , variablesFromWithinDeclarationInScope =
        FastSet.union
            additionalVariablesInScope
            context.variablesFromWithinDeclarationInScope
    }


expression :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            , originTypeName : String
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Result String SwiftExpression
expression context (Elm.Syntax.Node.Node _ syntaxExpression) =
    -- IGNORE TCO
    case syntaxExpression of
        Elm.Syntax.Expression.UnitExpr ->
            Ok swiftExpressionUnit

        Elm.Syntax.Expression.Integer intValue ->
            Ok (SwiftExpressionFloat (intValue |> Basics.toFloat))

        Elm.Syntax.Expression.Hex intValue ->
            Ok (SwiftExpressionFloat (intValue |> Basics.toFloat))

        Elm.Syntax.Expression.Floatable floatValue ->
            Ok (SwiftExpressionFloat floatValue)

        Elm.Syntax.Expression.CharLiteral charValue ->
            Ok (SwiftExpressionString (charValue |> String.fromChar))

        Elm.Syntax.Expression.Literal stringValue ->
            Ok (SwiftExpressionString stringValue)

        Elm.Syntax.Expression.RecordAccessFunction fieldName ->
            let
                recordVariableName : String
                recordVariableName =
                    "generated_record"
            in
            Ok
                (SwiftExpressionLambda
                    { parameter0 = Just recordVariableName
                    , parameter1Up = []
                    , result =
                        SwiftExpressionRecordAccess
                            { record =
                                SwiftExpressionReference
                                    { moduleOrigin = Nothing
                                    , name = recordVariableName
                                    }
                            , field =
                                fieldName
                                    |> String.replace "." ""
                                    |> lowercaseNameSanitizeForSwift
                            }
                    }
                )

        Elm.Syntax.Expression.Operator _ ->
            -- invalid syntax
            Err "operator is invalid syntax"

        Elm.Syntax.Expression.PrefixOperator operatorSymbol ->
            Result.map
                (\operationFunctionReference ->
                    SwiftExpressionReference operationFunctionReference
                )
                (expressionOperatorToSwiftFunctionReference operatorSymbol)

        Elm.Syntax.Expression.GLSLExpression _ ->
            Err "glsl not supported"

        Elm.Syntax.Expression.Application application ->
            case application of
                [] ->
                    Err "application without any parts is invalid"

                [ inParens ] ->
                    -- invalid syntax
                    expression context inParens

                calledNode :: argument0Node :: argument1UpNodes ->
                    Result.map3
                        (\called argument0 argument1Up ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument0
                                , argument1Up = argument1Up
                                }
                        )
                        (calledNode |> expression context)
                        (argument0Node |> expression context)
                        (argument1UpNodes
                            |> listMapAndCombineOk
                                (\argument -> argument |> expression context)
                        )

        Elm.Syntax.Expression.OperatorApplication operatorSymbol _ leftNode rightNode ->
            case operatorSymbol of
                "|>" ->
                    Result.map2
                        (\argument called ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                "<|" ->
                    Result.map2
                        (\called argument ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                "++" ->
                    Result.map2
                        (\left right ->
                            if
                                (left |> swiftExpressionIsDefinitelyOfTypeString)
                                    || (right |> swiftExpressionIsDefinitelyOfTypeString)
                            then
                                SwiftExpressionCall
                                    { called =
                                        SwiftExpressionReference
                                            { moduleOrigin = Nothing
                                            , name = "string_append"
                                            }
                                    , arguments = [ left, right ]
                                    }

                            else
                                SwiftExpressionCall
                                    { called =
                                        SwiftExpressionReference
                                            { moduleOrigin = Nothing
                                            , name = "list_append"
                                            }
                                    , arguments = [ left, right ]
                                    }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                otherOperatorSymbol ->
                    Result.map3
                        (\operationFunctionReference left right ->
                            SwiftExpressionCall
                                { called =
                                    SwiftExpressionReference operationFunctionReference
                                , arguments = [ left, right ]
                                }
                        )
                        (expressionOperatorToSwiftFunctionReference otherOperatorSymbol)
                        (leftNode |> expression context)
                        (rightNode |> expression context)

        Elm.Syntax.Expression.FunctionOrValue qualification name ->
            case context.variantLookup |> FastDict.get ( qualification, name ) of
                Just variantInfo ->
                    case ( variantInfo.moduleOrigin, name ) of
                        ( [ "Basics" ], "True" ) ->
                            Ok
                                (SwiftExpressionReference
                                    { moduleOrigin = Nothing
                                    , name = "true"
                                    }
                                )

                        ( [ "Basics" ], "False" ) ->
                            Ok
                                (SwiftExpressionReference
                                    { moduleOrigin = Nothing
                                    , name = "false"
                                    }
                                )

                        _ ->
                            let
                                swiftVariantIdentifier : { originTypeName : String, name : String }
                                swiftVariantIdentifier =
                                    case { moduleOrigin = variantInfo.moduleOrigin, name = name } |> variantToCoreSwift of
                                        Just swiftReference ->
                                            swiftReference

                                        Nothing ->
                                            let
                                                originTypeReference : { moduleOrigin : Elm.Syntax.ModuleName.ModuleName, name : String }
                                                originTypeReference =
                                                    { moduleOrigin = variantInfo.moduleOrigin
                                                    , name = variantInfo.originTypeName
                                                    }
                                            in
                                            { originTypeName =
                                                case
                                                    originTypeReference
                                                        |> referenceToCoreSwift
                                                of
                                                    Just swiftTypeName ->
                                                        swiftTypeName

                                                    Nothing ->
                                                        uppercaseReferenceToSwiftName originTypeReference
                                            , name =
                                                name |> uppercaseNameSanitizeForSwift
                                            }
                            in
                            Ok (SwiftExpressionVariant swiftVariantIdentifier)

                -- not a variant
                Nothing ->
                    let
                        asVariableFromWithinDeclaration : Maybe String
                        asVariableFromWithinDeclaration =
                            case qualification of
                                _ :: _ ->
                                    Nothing

                                [] ->
                                    let
                                        swiftName : String
                                        swiftName =
                                            name |> lowercaseNameSanitizeForSwift
                                    in
                                    if
                                        context.variablesFromWithinDeclarationInScope
                                            |> FastSet.member swiftName
                                    then
                                        Just swiftName

                                    else
                                        Nothing
                    in
                    case asVariableFromWithinDeclaration of
                        Just variableFromWithinDeclaration ->
                            Ok
                                (SwiftExpressionReference
                                    { moduleOrigin = Nothing
                                    , name = variableFromWithinDeclaration
                                    }
                                )

                        Nothing ->
                            case context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup |> FastDict.get ( qualification, name ) of
                                Just moduleOrigin ->
                                    Ok
                                        (case { moduleOrigin = moduleOrigin, name = name } |> referenceToCoreSwift of
                                            Just swiftReference ->
                                                SwiftExpressionReference
                                                    { moduleOrigin = Nothing
                                                    , name = swiftReference
                                                    }

                                            Nothing ->
                                                if
                                                    context.valuesThatNeedToBeLazilyConstructed
                                                        |> FastSet.member
                                                            ( moduleOrigin, name )
                                                then
                                                    SwiftExpressionCall
                                                        { called =
                                                            SwiftExpressionReference
                                                                { moduleOrigin = Nothing
                                                                , name =
                                                                    lowercaseReferenceToSwiftName
                                                                        { moduleOrigin = moduleOrigin
                                                                        , name = name
                                                                        }
                                                                }
                                                        , arguments = []
                                                        }

                                                else
                                                    SwiftExpressionReference
                                                        { moduleOrigin = Nothing
                                                        , name =
                                                            lowercaseReferenceToSwiftName
                                                                { moduleOrigin = moduleOrigin
                                                                , name = name
                                                                }
                                                        }
                                        )

                                -- not a reference that was declared in elm
                                Nothing ->
                                    case qualification of
                                        qualificationPart0 :: qualificationPart1Up ->
                                            Err
                                                ("could not find module origin of the qualified reference "
                                                    ++ (((qualificationPart0 :: qualificationPart1Up) |> String.join ".")
                                                            ++ "."
                                                            ++ name
                                                       )
                                                )

                                        [] ->
                                            -- TODO convert to error
                                            Ok
                                                (SwiftExpressionReference
                                                    { moduleOrigin = Nothing
                                                    , name = name |> lowercaseNameSanitizeForSwift
                                                    }
                                                )

        Elm.Syntax.Expression.IfBlock conditionNode onTrueNode onFalseNode ->
            Result.map3
                (\condition onTrue onFalse ->
                    SwiftExpressionSwitch
                        { matched = condition
                        , case0 = { pattern = SwiftPatternTrue, result = onTrue }
                        , case1Up = [ { pattern = SwiftPatternFalse, result = onFalse } ]
                        }
                )
                (conditionNode |> expression context)
                (onTrueNode |> expression context)
                (onFalseNode |> expression context)

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            inParens |> expression context

        Elm.Syntax.Expression.Negation inNegationNode ->
            Result.map
                (\inNegation ->
                    SwiftExpressionCall
                        { called =
                            SwiftExpressionReference
                                { moduleOrigin = Nothing, name = "basics_negate" }
                        , arguments = [ inNegation ]
                        }
                )
                (inNegationNode |> expression context)

        Elm.Syntax.Expression.RecordAccess recordNode (Elm.Syntax.Node.Node _ fieldName) ->
            Result.map
                (\record ->
                    SwiftExpressionRecordAccess
                        { record = record
                        , field =
                            fieldName
                                |> String.replace "." ""
                                |> lowercaseNameSanitizeForSwift
                        }
                )
                (recordNode |> expression context)

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.UnitExpr
                    Ok swiftExpressionUnit

                [ inParens ] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.ParenthesizedExpression
                    expression context inParens

                [ part0Node, part1Node ] ->
                    Result.map2
                        (\part0 part1 ->
                            SwiftExpressionRecord
                                (FastDict.fromList
                                    [ ( "first", part0 )
                                    , ( "second", part1 )
                                    ]
                                )
                        )
                        (part0Node |> expression context)
                        (part1Node |> expression context)

                [ part0Node, part1Node, part2Node ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            SwiftExpressionRecord
                                (FastDict.fromList
                                    [ ( "first", part0 )
                                    , ( "second", part1 )
                                    , ( "third", part2 )
                                    ]
                                )
                        )
                        (part0Node |> expression context)
                        (part1Node |> expression context)
                        (part2Node |> expression context)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.Expression.ListExpr elementNodes ->
            Result.map
                (\elements ->
                    elements
                        |> List.foldr
                            (\head tail ->
                                SwiftExpressionCall
                                    { called =
                                        SwiftExpressionVariant
                                            { originTypeName = "List_List"
                                            , name = "Cons"
                                            }
                                    , arguments = [ head, tail ]
                                    }
                            )
                            swiftExpressionListEmpty
                )
                (elementNodes
                    |> listMapAndCombineOk
                        (\element -> element |> expression context)
                )

        Elm.Syntax.Expression.RecordExpr fieldNodes ->
            Result.map (\fields -> SwiftExpressionRecord fields)
                (fieldNodes
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValueNode )) ->
                            Result.map
                                (\fieldValue ->
                                    ( fieldName
                                        |> lowercaseNameSanitizeForSwift
                                    , fieldValue
                                    )
                                )
                                (fieldValueNode |> expression context)
                        )
                    |> Result.map FastDict.fromList
                )

        Elm.Syntax.Expression.RecordUpdateExpression _ _ ->
            Err "record update not supported"

        Elm.Syntax.Expression.LambdaExpression lambda ->
            case lambda.args of
                [] ->
                    Err "lambda without parameters is invalid syntax"

                parameter0Node :: parameter1UpNodes ->
                    lambdaExpression context
                        { result = lambda.expression
                        , parameter0 = parameter0Node
                        , parameter1Up = parameter1UpNodes
                        }

        Elm.Syntax.Expression.CaseExpression caseOf ->
            case caseOf.cases of
                [] ->
                    Err "case-of without cases invalid syntax"

                case0Node :: case1Node ->
                    Result.map3
                        (\matched case0 case1Up ->
                            SwiftExpressionSwitch
                                { matched = matched
                                , case0 = case0
                                , case1Up = case1Up
                                }
                        )
                        (caseOf.expression |> expression context)
                        (case0Node |> case_ context)
                        (case1Node
                            |> listMapAndCombineOk
                                (\parameter ->
                                    parameter |> case_ context
                                )
                        )

        Elm.Syntax.Expression.LetExpression letIn ->
            case letIn.declarations of
                [] ->
                    Err "let-in without declarations is invalid syntax"

                declaration0Node :: declaration1UpNode ->
                    expressionWithLocalDeclarations context
                        { declaration0Node = declaration0Node
                        , declaration1UpNode = declaration1UpNode
                        , expression = letIn.expression
                        }


swiftExpressionListEmpty : SwiftExpression
swiftExpressionListEmpty =
    SwiftExpressionVariant
        { originTypeName = "List_List"
        , name = "Empty"
        }


parametersToSwiftAndDestructuring :
    List SwiftPattern
    ->
        { parameters : List (Maybe String)
        , destructuring :
            Maybe
                { expression : SwiftExpression
                , pattern : SwiftPattern
                }
        }
parametersToSwiftAndDestructuring parameterPatterns =
    let
        parametersAndDestructurings :
            { parameters : List (Maybe String)
            , destructurings : FastDict.Dict String SwiftPattern
            }
        parametersAndDestructurings =
            parameterPatterns
                |> List.foldr
                    (\parameterPattern soFar ->
                        case parameterPattern of
                            SwiftPatternVariable variableName ->
                                { parameters =
                                    Just variableName :: soFar.parameters
                                , destructurings = soFar.destructurings
                                }

                            parameterPatternNotVariable ->
                                case parameterPatternNotVariable |> swiftPatternIntroducedVariables of
                                    [] ->
                                        { parameters =
                                            Nothing :: soFar.parameters
                                        , destructurings = soFar.destructurings
                                        }

                                    parameterPatternIntroducedVariable0 :: parameterPatternIntroducedVariable1Up ->
                                        let
                                            generatedVariableName : String
                                            generatedVariableName =
                                                "generated_"
                                                    ++ ((parameterPatternIntroducedVariable0 :: parameterPatternIntroducedVariable1Up)
                                                            |> String.join "_"
                                                       )
                                        in
                                        { parameters =
                                            Just generatedVariableName
                                                :: soFar.parameters
                                        , destructurings =
                                            soFar.destructurings
                                                |> FastDict.insert generatedVariableName
                                                    parameterPatternNotVariable
                                        }
                    )
                    { parameters = []
                    , destructurings = FastDict.empty
                    }
    in
    { parameters = parametersAndDestructurings.parameters
    , destructuring =
        if parametersAndDestructurings.destructurings |> FastDict.isEmpty then
            Nothing

        else
            Just
                { expression =
                    SwiftExpressionRecord
                        (parametersAndDestructurings.destructurings
                            |> FastDict.keys
                            |> List.map
                                (\generatedVariableName ->
                                    ( generatedVariableName
                                    , SwiftExpressionReference
                                        { moduleOrigin = Nothing
                                        , name = generatedVariableName
                                        }
                                    )
                                )
                            |> FastDict.fromList
                        )
                , pattern =
                    SwiftPatternRecord
                        parametersAndDestructurings.destructurings
                }
    }


lambdaExpression :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            , originTypeName : String
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    ->
        { result : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , parameter0 : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        , parameter1Up : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        }
    -> Result String SwiftExpression
lambdaExpression context lambda =
    resultAndThen2
        (\parameterPattern0 parameterPattern1Up ->
            Result.map
                (\result ->
                    let
                        parametersAndDestructuring :
                            { parameters : List (Maybe String)
                            , destructuring :
                                Maybe
                                    { expression : SwiftExpression
                                    , pattern : SwiftPattern
                                    }
                            }
                        parametersAndDestructuring =
                            parametersToSwiftAndDestructuring
                                (parameterPattern0.pattern
                                    :: (parameterPattern1Up |> List.map .pattern)
                                )
                    in
                    case parametersAndDestructuring.parameters of
                        [] ->
                            SwiftExpressionReference
                                { moduleOrigin = Nothing, name = "bugIn_parametersToSwiftAndDestructuring" }

                        parameter0 :: parameter1Up ->
                            let
                                resultIncludingAliasPatternAssignments : SwiftExpression
                                resultIncludingAliasPatternAssignments =
                                    (parameterPattern0 :: parameterPattern1Up)
                                        |> listMapToFastDictsAndUnify .patternAliasesToAdd
                                        |> FastDict.foldl
                                            (\variableName expressionToAssign soFar ->
                                                SwiftExpressionWithLocalDeclaration
                                                    { declaration =
                                                        { name = variableName
                                                        , result = expressionToAssign
                                                        , parameters = []
                                                        , type_ = Nothing
                                                        }
                                                    , result = soFar
                                                    }
                                            )
                                            result
                            in
                            SwiftExpressionLambda
                                { parameter0 = parameter0
                                , parameter1Up = parameter1Up
                                , result =
                                    case parametersAndDestructuring.destructuring of
                                        Nothing ->
                                            resultIncludingAliasPatternAssignments

                                        Just destructuring ->
                                            SwiftExpressionSwitch
                                                { matched = destructuring.expression
                                                , case0 =
                                                    { pattern = destructuring.pattern
                                                    , result = resultIncludingAliasPatternAssignments
                                                    }
                                                , case1Up = []
                                                }
                                }
                )
                (lambda.result
                    |> expression
                        (context
                            |> expressionContextAddVariablesInScope
                                (FastSet.union
                                    parameterPattern0.introducedVariables
                                    (parameterPattern1Up
                                        |> listMapToFastSetsAndUnify .introducedVariables
                                    )
                                )
                        )
                )
        )
        (lambda.parameter0
            |> pattern
                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                    context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                , variantLookup = context.variantLookup
                }
        )
        (lambda.parameter1Up
            |> listMapAndCombineOk
                (\parameter ->
                    parameter
                        |> pattern
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                )
        )


expressionWithLocalDeclarations :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            , originTypeName : String
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    ->
        { declaration0Node : Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
        , declaration1UpNode : List (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration)
        , expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        }
    -> Result String SwiftExpression
expressionWithLocalDeclarations context letIn =
    let
        variablesForWholeLetIn : FastSet.Set String
        variablesForWholeLetIn =
            (letIn.declaration0Node :: letIn.declaration1UpNode)
                |> listMapToFastSetsAndUnify
                    (\(Elm.Syntax.Node.Node _ syntaxLetDeclaration) ->
                        case syntaxLetDeclaration of
                            Elm.Syntax.Expression.LetFunction letFunction ->
                                FastSet.singleton
                                    (letFunction.declaration
                                        |> Elm.Syntax.Node.value
                                        |> .name
                                        |> Elm.Syntax.Node.value
                                        |> lowercaseNameSanitizeForSwift
                                    )

                            Elm.Syntax.Expression.LetDestructuring patternNode _ ->
                                patternNode
                                    |> patternBindings
                                    |> listMapAndToFastSet
                                        lowercaseNameSanitizeForSwift
                    )
    in
    Result.map3
        (\declaration0 declaration1Up result ->
            let
                valueOrFunctionDeclarations :
                    List
                        { name : String
                        , parameters : List (Maybe String)
                        , result : SwiftExpression
                        , type_ : Maybe SwiftType
                        }
                valueOrFunctionDeclarations =
                    (declaration0 :: declaration1Up)
                        |> List.filterMap
                            (\declaration ->
                                case declaration.main of
                                    SwiftDestructuring _ ->
                                        Nothing

                                    SwiftLocalDeclarationValueOrFunction swiftLetValueOrFunction ->
                                        Just swiftLetValueOrFunction
                            )

                destructuringDeclarations :
                    List
                        { pattern : SwiftPattern
                        , expression : SwiftExpression
                        }
                destructuringDeclarations =
                    (declaration0 :: declaration1Up)
                        |> List.filterMap
                            (\declaration ->
                                case declaration.main of
                                    SwiftLocalDeclarationValueOrFunction _ ->
                                        Nothing

                                    SwiftDestructuring swiftLetDestructuring ->
                                        Just swiftLetDestructuring
                            )

                valueAndFunctionDeclarationsMostToLeastDependedUpon :
                    List
                        (Data.Graph.SCC
                            { parameters : List (Maybe String)
                            , type_ : Maybe SwiftType
                            , name : String
                            , result : SwiftExpression
                            }
                        )
                valueAndFunctionDeclarationsMostToLeastDependedUpon =
                    valueOrFunctionDeclarations
                        |> List.map
                            (\declaration ->
                                ( declaration
                                , declaration.name
                                , declaration.result
                                    |> swiftExpressionContainedLocalReferences
                                    |> FastSet.toList
                                )
                            )
                        |> Data.Graph.stronglyConnComp
            in
            includeDestructuringsIntoSwiftValueAndFunctionDeclarations
                { valueAndFunctionDeclarationsMostToLeastDependedUpon =
                    valueAndFunctionDeclarationsMostToLeastDependedUpon
                , destructuringDeclarationsMostToLeastDependedUpon =
                    destructuringDeclarations
                        |> destructuringDeclarationsSortMostToLeastDependedUpon
                , result =
                    (declaration0 :: declaration1Up)
                        |> listMapToFastDictsAndUnify .patternAliasesToAdd
                        |> FastDict.foldl
                            (\variableName expressionToAssign soFar ->
                                SwiftExpressionWithLocalDeclaration
                                    { declaration =
                                        { name = variableName
                                        , result = expressionToAssign
                                        , parameters = []
                                        , type_ = Nothing
                                        }
                                    , result = soFar
                                    }
                            )
                            result
                }
        )
        (letIn.declaration0Node
            |> letDeclaration
                (context
                    |> expressionContextAddVariablesInScope
                        variablesForWholeLetIn
                )
        )
        (letIn.declaration1UpNode
            |> listMapAndCombineOk
                (\letDecl ->
                    letDecl
                        |> letDeclaration
                            (context
                                |> expressionContextAddVariablesInScope
                                    variablesForWholeLetIn
                            )
                )
        )
        (letIn.expression
            |> expression
                (context
                    |> expressionContextAddVariablesInScope
                        variablesForWholeLetIn
                )
        )


destructuringDeclarationsSortMostToLeastDependedUpon :
    List
        { pattern : SwiftPattern
        , expression : SwiftExpression
        }
    ->
        List
            { pattern : SwiftPattern
            , expression : SwiftExpression
            }
destructuringDeclarationsSortMostToLeastDependedUpon destructuringDeclarations =
    destructuringDeclarations
        |> List.sortWith destructuringDeclarationDependenceOrder


destructuringDeclarationDependenceOrder :
    { pattern : SwiftPattern
    , expression : SwiftExpression
    }
    ->
        { pattern : SwiftPattern
        , expression : SwiftExpression
        }
    -> Order
destructuringDeclarationDependenceOrder a b =
    let
        aIntroducedVariables : FastSet.Set String
        aIntroducedVariables =
            a.pattern
                |> swiftPatternIntroducedVariables
                |> FastSet.fromList

        bUsedLocalReferences : FastSet.Set String
        bUsedLocalReferences =
            b.expression |> swiftExpressionContainedLocalReferences
    in
    if fastSetsIntersect aIntroducedVariables bUsedLocalReferences then
        LT

    else
        let
            bIntroducedVariables : FastSet.Set String
            bIntroducedVariables =
                b.pattern
                    |> swiftPatternIntroducedVariables
                    |> FastSet.fromList

            aUsedLocalReferences : FastSet.Set String
            aUsedLocalReferences =
                a.expression |> swiftExpressionContainedLocalReferences
        in
        if fastSetsIntersect bIntroducedVariables aUsedLocalReferences then
            GT

        else
            EQ


includeDestructuringsIntoSwiftValueAndFunctionDeclarations :
    { destructuringDeclarationsMostToLeastDependedUpon :
        List
            { pattern : SwiftPattern
            , expression : SwiftExpression
            }
    , valueAndFunctionDeclarationsMostToLeastDependedUpon :
        List
            (Data.Graph.SCC
                { name : String
                , parameters : List (Maybe String)
                , result : SwiftExpression
                , type_ : Maybe SwiftType
                }
            )
    , result : SwiftExpression
    }
    -> SwiftExpression
includeDestructuringsIntoSwiftValueAndFunctionDeclarations state =
    includeDestructuringsIntoSwiftValueAndFunctionDeclarationsFrom []
        { destructuringDeclarationsMostToLeastDependedUpon =
            state.destructuringDeclarationsMostToLeastDependedUpon
        , valueAndFunctionDeclarationsMostToLeastDependedUpon =
            state.valueAndFunctionDeclarationsMostToLeastDependedUpon
        }
        |> List.foldl
            (\valueOrFunctionDeclarationOrDestructuring soFar ->
                case valueOrFunctionDeclarationOrDestructuring of
                    SwiftDestructuring destructuring ->
                        SwiftExpressionSwitch
                            { matched = destructuring.expression
                            , case0 =
                                { pattern = destructuring.pattern
                                , result = soFar
                                }
                            , case1Up = []
                            }

                    SwiftLocalDeclarationValueOrFunction localValueOrFunctionDeclaration ->
                        SwiftExpressionWithLocalDeclaration
                            { declaration = localValueOrFunctionDeclaration
                            , result = soFar
                            }
            )
            state.result


includeDestructuringsIntoSwiftValueAndFunctionDeclarationsFrom :
    List SwiftValueOrFunctionDeclarationOrDestructuring
    ->
        { destructuringDeclarationsMostToLeastDependedUpon :
            List
                { pattern : SwiftPattern
                , expression : SwiftExpression
                }
        , valueAndFunctionDeclarationsMostToLeastDependedUpon :
            List
                (Data.Graph.SCC
                    { name : String
                    , parameters : List (Maybe String)
                    , result : SwiftExpression
                    , type_ : Maybe SwiftType
                    }
                )
        }
    -> List SwiftValueOrFunctionDeclarationOrDestructuring
includeDestructuringsIntoSwiftValueAndFunctionDeclarationsFrom valueOrFunctionOrDestructuringsSoFar state =
    case state.destructuringDeclarationsMostToLeastDependedUpon of
        [] ->
            (state.valueAndFunctionDeclarationsMostToLeastDependedUpon
                |> List.concatMap
                    (\group ->
                        case group of
                            Data.Graph.CyclicSCC cycleMembers ->
                                cycleMembers

                            Data.Graph.AcyclicSCC member ->
                                [ member ]
                    )
                |> List.map
                    (\declaration ->
                        SwiftLocalDeclarationValueOrFunction declaration
                    )
                |> List.reverse
            )
                ++ valueOrFunctionOrDestructuringsSoFar

        mostDependedUponDestructuringDeclaration :: destructuringDeclarationsWithoutMostDependedUpon ->
            case state.valueAndFunctionDeclarationsMostToLeastDependedUpon of
                [] ->
                    ((mostDependedUponDestructuringDeclaration
                        :: destructuringDeclarationsWithoutMostDependedUpon
                     )
                        |> List.map SwiftDestructuring
                        |> List.reverse
                    )
                        ++ valueOrFunctionOrDestructuringsSoFar

                mostDependedUponGroup :: remainingWithoutGroup ->
                    -- TODO avoid duplicate swiftExpressionContainedLocalReferences
                    let
                        destructuringIntroducedVariables : FastSet.Set String
                        destructuringIntroducedVariables =
                            mostDependedUponDestructuringDeclaration.pattern
                                |> swiftPatternIntroducedVariables
                                |> FastSet.fromList

                        groupLocalReferences : FastSet.Set String
                        groupLocalReferences =
                            case mostDependedUponGroup of
                                Data.Graph.CyclicSCC cycleMembers ->
                                    cycleMembers
                                        |> listMapToFastSetsAndUnify
                                            (\declaration ->
                                                declaration.result
                                                    |> swiftExpressionContainedLocalReferences
                                            )

                                Data.Graph.AcyclicSCC member ->
                                    member.result |> swiftExpressionContainedLocalReferences
                    in
                    if
                        fastSetsIntersect
                            destructuringIntroducedVariables
                            groupLocalReferences
                    then
                        includeDestructuringsIntoSwiftValueAndFunctionDeclarationsFrom
                            (SwiftDestructuring mostDependedUponDestructuringDeclaration
                                :: valueOrFunctionOrDestructuringsSoFar
                            )
                            { valueAndFunctionDeclarationsMostToLeastDependedUpon =
                                mostDependedUponGroup :: remainingWithoutGroup
                            , destructuringDeclarationsMostToLeastDependedUpon =
                                destructuringDeclarationsWithoutMostDependedUpon
                            }

                    else
                        includeDestructuringsIntoSwiftValueAndFunctionDeclarationsFrom
                            (case mostDependedUponGroup of
                                Data.Graph.CyclicSCC cycleMembers ->
                                    (cycleMembers
                                        |> List.map SwiftLocalDeclarationValueOrFunction
                                    )
                                        ++ valueOrFunctionOrDestructuringsSoFar

                                Data.Graph.AcyclicSCC member ->
                                    SwiftLocalDeclarationValueOrFunction member
                                        :: valueOrFunctionOrDestructuringsSoFar
                            )
                            { valueAndFunctionDeclarationsMostToLeastDependedUpon =
                                remainingWithoutGroup
                            , destructuringDeclarationsMostToLeastDependedUpon =
                                mostDependedUponDestructuringDeclaration :: destructuringDeclarationsWithoutMostDependedUpon
                            }


fastSetsIntersect : FastSet.Set comparable -> FastSet.Set comparable -> Bool
fastSetsIntersect a b =
    -- TODO optimize
    Basics.not (FastSet.isEmpty (FastSet.intersect a b))


swiftExpressionUnit : SwiftExpression
swiftExpressionUnit =
    SwiftExpressionRecord FastDict.empty


swiftExpressionContainedLocalReferences : SwiftExpression -> FastSet.Set String
swiftExpressionContainedLocalReferences swiftExpression =
    --IGNORE TCO
    case swiftExpression of
        SwiftExpressionFloat _ ->
            FastSet.empty

        SwiftExpressionString _ ->
            FastSet.empty

        SwiftExpressionVariant _ ->
            FastSet.empty

        SwiftExpressionReference reference ->
            case reference.moduleOrigin of
                Nothing ->
                    reference.name |> FastSet.singleton

                Just _ ->
                    FastSet.empty

        SwiftExpressionRecordAccess swiftRecordAccess ->
            swiftExpressionContainedLocalReferences swiftRecordAccess.record

        SwiftExpressionRecord fields ->
            fields
                |> FastDict.values
                |> listMapToFastSetsAndUnify
                    swiftExpressionContainedLocalReferences

        SwiftExpressionCall call ->
            FastSet.union
                (call.called |> swiftExpressionContainedLocalReferences)
                (call.arguments
                    |> listMapToFastSetsAndUnify
                        swiftExpressionContainedLocalReferences
                )

        SwiftExpressionLambda lambda ->
            swiftExpressionContainedLocalReferences lambda.result

        SwiftExpressionSwitch switch ->
            FastSet.union
                (switch.matched |> swiftExpressionContainedLocalReferences)
                (FastSet.union
                    (switch.case0.result
                        |> swiftExpressionContainedLocalReferences
                    )
                    (switch.case1Up
                        |> listMapToFastSetsAndUnify
                            (\swiftCase ->
                                swiftCase.result
                                    |> swiftExpressionContainedLocalReferences
                            )
                    )
                )

        SwiftExpressionWithLocalDeclaration withLocalDeclarations ->
            FastSet.union
                (withLocalDeclarations.result
                    |> swiftExpressionContainedLocalReferences
                )
                (withLocalDeclarations.declaration.result
                    |> swiftExpressionContainedLocalReferences
                )


{-| Recursively find all introduced variables
in the [pattern](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern)
(like `a` and `b` in `( Just a, { b } )`)
-}
patternBindings : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern -> List String
patternBindings (Elm.Syntax.Node.Node _ syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.VarPattern name ->
            [ name ]

        Elm.Syntax.Pattern.AsPattern afterAsPattern (Elm.Syntax.Node.Node _ name) ->
            name :: (afterAsPattern |> patternBindings)

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            inParens |> patternBindings

        Elm.Syntax.Pattern.ListPattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.TuplePattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.RecordPattern fields ->
            fields |> List.map Elm.Syntax.Node.value

        Elm.Syntax.Pattern.NamedPattern _ patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            (tailPattern |> patternBindings) ++ (headPattern |> patternBindings)

        Elm.Syntax.Pattern.AllPattern ->
            []

        Elm.Syntax.Pattern.UnitPattern ->
            []

        Elm.Syntax.Pattern.CharPattern _ ->
            []

        Elm.Syntax.Pattern.StringPattern _ ->
            []

        Elm.Syntax.Pattern.IntPattern _ ->
            []

        Elm.Syntax.Pattern.HexPattern _ ->
            []

        Elm.Syntax.Pattern.FloatPattern _ ->
            []


resultAndThen2 :
    (a -> b -> Result error c)
    -> Result error a
    -> Result error b
    -> Result error c
resultAndThen2 abToResult aResult bResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    abToResult a b


listMapToFastSetsAndUnify :
    (listElement -> FastSet.Set comparableFastSetElement)
    -> List listElement
    -> FastSet.Set comparableFastSetElement
listMapToFastSetsAndUnify elementToSet list =
    list
        |> List.foldl
            (\element soFar ->
                FastSet.union
                    (element |> elementToSet)
                    soFar
            )
            FastSet.empty


listMapToFastDictsAndUnify :
    (listElement -> FastDict.Dict comparableKey value)
    -> List listElement
    -> FastDict.Dict comparableKey value
listMapToFastDictsAndUnify elementToDict list =
    list
        |> List.foldl
            (\element soFar ->
                FastDict.union
                    (element |> elementToDict)
                    soFar
            )
            FastDict.empty


listMapToFastDict :
    (listElement -> ( comparableKey, value ))
    -> List listElement
    -> FastDict.Dict comparableKey value
listMapToFastDict elementToKeyValue list =
    list
        |> List.foldl
            (\element soFar ->
                let
                    ( key, value ) =
                        elementToKeyValue element
                in
                FastDict.insert key value soFar
            )
            FastDict.empty


listMapAndToFastSet :
    (a -> comparable)
    -> List a
    -> FastSet.Set comparable
listMapAndToFastSet elementToSetElement list =
    list
        |> List.foldl
            (\element soFar ->
                soFar
                    |> FastSet.insert
                        (element |> elementToSetElement)
            )
            FastSet.empty


condenseExpressionCall :
    { called : SwiftExpression
    , argument0 : SwiftExpression
    , argument1Up : List SwiftExpression
    }
    -> SwiftExpression
condenseExpressionCall call =
    case call.called of
        SwiftExpressionCall calledCall ->
            case calledCall.arguments of
                [] ->
                    SwiftExpressionCall calledCall

                calledCallArgument0 :: calledCallArgument1Up ->
                    condenseExpressionCall
                        { called = calledCall.called
                        , argument0 = calledCallArgument0
                        , argument1Up =
                            calledCallArgument1Up
                                ++ (call.argument0 :: call.argument1Up)
                        }

        SwiftExpressionLambda calledLambda ->
            case ( calledLambda.parameter0, calledLambda.result ) of
                ( Just "generated_record", SwiftExpressionRecordAccess recordAccess ) ->
                    case call.argument1Up of
                        [] ->
                            SwiftExpressionRecordAccess
                                { record = call.argument0
                                , field = recordAccess.field
                                }

                        argument1 :: argument2Up ->
                            SwiftExpressionCall
                                { called =
                                    SwiftExpressionRecordAccess
                                        { record = call.argument0
                                        , field = recordAccess.field
                                        }
                                , arguments = argument1 :: argument2Up
                                }

                _ ->
                    SwiftExpressionCall
                        { called = SwiftExpressionLambda calledLambda
                        , arguments = call.argument0 :: call.argument1Up
                        }

        calledNotCall ->
            SwiftExpressionCall
                { called = calledNotCall
                , arguments = call.argument0 :: call.argument1Up
                }


swiftExpressionIsDefinitelyOfTypeString : SwiftExpression -> Bool
swiftExpressionIsDefinitelyOfTypeString swiftExpression =
    case swiftExpression of
        SwiftExpressionString _ ->
            True

        SwiftExpressionVariant _ ->
            False

        SwiftExpressionCall call ->
            call.called
                == SwiftExpressionReference { moduleOrigin = Nothing, name = "string_append" }
                && ((call.arguments |> List.length) == 2)

        SwiftExpressionFloat _ ->
            False

        SwiftExpressionReference _ ->
            False

        SwiftExpressionRecordAccess _ ->
            False

        SwiftExpressionRecord _ ->
            False

        SwiftExpressionLambda _ ->
            False

        SwiftExpressionSwitch _ ->
            False

        SwiftExpressionWithLocalDeclaration _ ->
            False


case_ :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            , originTypeName : String
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Expression.Case
    ->
        Result
            String
            { pattern : SwiftPattern, result : SwiftExpression }
case_ context ( patternNode, resultNode ) =
    Result.andThen
        (\casePattern ->
            Result.map
                (\result ->
                    { pattern = casePattern.pattern
                    , result =
                        casePattern.patternAliasesToAdd
                            |> FastDict.foldl
                                (\variableName expressionToAssign soFar ->
                                    SwiftExpressionWithLocalDeclaration
                                        { declaration =
                                            { name = variableName
                                            , result = expressionToAssign
                                            , parameters = []
                                            , type_ = Nothing
                                            }
                                        , result = soFar
                                        }
                                )
                                result
                    }
                )
                (resultNode
                    |> expression
                        (context
                            |> expressionContextAddVariablesInScope
                                casePattern.introducedVariables
                        )
                )
        )
        (patternNode
            |> pattern
                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                    context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                , variantLookup = context.variantLookup
                }
        )


letDeclaration :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            , originTypeName : String
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
    ->
        Result
            String
            { main : SwiftValueOrFunctionDeclarationOrDestructuring
            , patternAliasesToAdd : FastDict.Dict String SwiftExpression
            }
letDeclaration context (Elm.Syntax.Node.Node _ syntaxLetDeclaration) =
    case syntaxLetDeclaration of
        Elm.Syntax.Expression.LetDestructuring destructuringPatternNode destructuringExpressionNode ->
            Result.map2
                (\destructuringPattern destructuringExpression ->
                    { main =
                        SwiftDestructuring
                            { pattern = destructuringPattern.pattern
                            , expression = destructuringExpression
                            }
                    , patternAliasesToAdd =
                        destructuringPattern.patternAliasesToAdd
                    }
                )
                (destructuringPatternNode
                    |> pattern
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                        , variantLookup = context.variantLookup
                        }
                )
                (destructuringExpressionNode |> expression context)

        Elm.Syntax.Expression.LetFunction letValueOrFunction ->
            Result.map
                (\swiftLetDeclarationValueOrFunction ->
                    { main =
                        SwiftLocalDeclarationValueOrFunction
                            swiftLetDeclarationValueOrFunction
                    , patternAliasesToAdd = FastDict.empty
                    }
                )
                (letValueOrFunction
                    |> letValueOrFunctionDeclaration context
                )


letValueOrFunctionDeclaration :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            , originTypeName : String
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Expression.Function
    ->
        Result
            String
            { name : String
            , parameters : List (Maybe String)
            , type_ : Maybe SwiftType
            , result : SwiftExpression
            }
letValueOrFunctionDeclaration context syntaxDeclarationValueOrFunction =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationValueOrFunction.declaration |> Elm.Syntax.Node.value
    in
    resultAndThen2
        (\parameters maybeType ->
            Result.map
                (\result ->
                    let
                        resultIncludingAliasPatternAssignments : SwiftExpression
                        resultIncludingAliasPatternAssignments =
                            parameters
                                |> listMapToFastDictsAndUnify .patternAliasesToAdd
                                |> FastDict.foldl
                                    (\variableName expressionToAssign soFar ->
                                        SwiftExpressionWithLocalDeclaration
                                            { declaration =
                                                { name = variableName
                                                , result = expressionToAssign
                                                , parameters = []
                                                , type_ = Nothing
                                                }
                                            , result = soFar
                                            }
                                    )
                                    result

                        parametersAndDestructuring :
                            { parameters : List (Maybe String)
                            , destructuring :
                                Maybe
                                    { expression : SwiftExpression
                                    , pattern : SwiftPattern
                                    }
                            }
                        parametersAndDestructuring =
                            parametersToSwiftAndDestructuring
                                (parameters |> List.map .pattern)
                    in
                    { name =
                        implementation.name
                            |> Elm.Syntax.Node.value
                            |> lowercaseNameSanitizeForSwift
                    , parameters = parametersAndDestructuring.parameters
                    , type_ = maybeType
                    , result =
                        case parametersAndDestructuring.destructuring of
                            Nothing ->
                                resultIncludingAliasPatternAssignments

                            Just destructuring ->
                                SwiftExpressionSwitch
                                    { matched = destructuring.expression
                                    , case0 =
                                        { pattern = destructuring.pattern
                                        , result = resultIncludingAliasPatternAssignments
                                        }
                                    , case1Up = []
                                    }
                    }
                )
                (implementation.expression
                    |> expression
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                        , valuesThatNeedToBeLazilyConstructed =
                            context.valuesThatNeedToBeLazilyConstructed
                        , variantLookup = context.variantLookup
                        , variablesFromWithinDeclarationInScope =
                            FastSet.union
                                (parameters
                                    |> listMapToFastSetsAndUnify .introducedVariables
                                )
                                context.variablesFromWithinDeclarationInScope
                        }
                )
        )
        (implementation.arguments
            |> listMapAndCombineOk
                (\p ->
                    p
                        |> pattern
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                )
        )
        (case syntaxDeclarationValueOrFunction.signature of
            Nothing ->
                Ok Nothing

            Just (Elm.Syntax.Node.Node _ signature) ->
                Result.map Just
                    (signature.typeAnnotation
                        |> type_
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                    )
        )


expressionOperatorToSwiftFunctionReference :
    String
    -> Result String { moduleOrigin : Maybe String, name : String }
expressionOperatorToSwiftFunctionReference operatorSymbol =
    case operatorSymbol of
        "+" ->
            Ok { moduleOrigin = Nothing, name = "basics_add" }

        "-" ->
            Ok { moduleOrigin = Nothing, name = "basics_sub" }

        "*" ->
            Ok { moduleOrigin = Nothing, name = "basics_mul" }

        "/" ->
            Ok { moduleOrigin = Nothing, name = "basics_fdiv" }

        "//" ->
            Ok { moduleOrigin = Nothing, name = "basics_idiv" }

        "^" ->
            Ok { moduleOrigin = Nothing, name = "basics_pow" }

        "==" ->
            Ok { moduleOrigin = Nothing, name = "basics_eq" }

        "/=" ->
            Ok { moduleOrigin = Nothing, name = "basics_neq" }

        "||" ->
            Ok { moduleOrigin = Nothing, name = "basics_or" }

        "&&" ->
            Ok { moduleOrigin = Nothing, name = "basics_and" }

        "<" ->
            Ok { moduleOrigin = Nothing, name = "basics_lt" }

        ">" ->
            Ok { moduleOrigin = Nothing, name = "basics_gt" }

        "<=" ->
            Ok { moduleOrigin = Nothing, name = "basics_le" }

        ">=" ->
            Ok { moduleOrigin = Nothing, name = "basics_ge" }

        "::" ->
            -- TODO use SwiftExpressionVariant instead
            Ok { moduleOrigin = Just "List_List", name = "Cons" }

        "++" ->
            Ok { moduleOrigin = Nothing, name = "list_append" }

        unknownOrUnsupportedOperator ->
            Err ("unknown/unsupported operator " ++ unknownOrUnsupportedOperator)


qualifiedSwiftReferenceToString :
    { moduleOrigin : Maybe String
    , name : String
    }
    -> String
qualifiedSwiftReferenceToString reference =
    case reference.moduleOrigin of
        Nothing ->
            reference.name

        Just moduleOrigin ->
            moduleOrigin
                ++ "."
                ++ reference.name


printSwiftExpressionParenthesizedIfSpaceSeparated : SwiftExpression -> Print
printSwiftExpressionParenthesizedIfSpaceSeparated swiftExpression =
    if swiftExpression |> swiftExpressionIsSpaceSeparated then
        printParenthesized
            { opening = "("
            , closing = ")"
            , inner = printSwiftExpressionNotParenthesized swiftExpression
            }

    else
        printSwiftExpressionNotParenthesized swiftExpression


swiftExpressionIsSpaceSeparated : SwiftExpression -> Bool
swiftExpressionIsSpaceSeparated swiftExpression =
    case swiftExpression of
        SwiftExpressionFloat _ ->
            False

        SwiftExpressionString _ ->
            False

        SwiftExpressionReference _ ->
            False

        SwiftExpressionVariant _ ->
            False

        SwiftExpressionRecordAccess _ ->
            False

        SwiftExpressionRecord _ ->
            False

        SwiftExpressionCall _ ->
            False

        SwiftExpressionLambda _ ->
            True

        SwiftExpressionSwitch _ ->
            True

        SwiftExpressionWithLocalDeclaration _ ->
            False


{-| Print a [`SwiftExpression`](#SwiftExpression)
-}
printSwiftExpressionNotParenthesized : SwiftExpression -> Print
printSwiftExpressionNotParenthesized swiftExpression =
    -- IGNORE TCO
    case swiftExpression of
        SwiftExpressionCall call ->
            printSwiftExpressionCall call

        SwiftExpressionVariant variant ->
            Print.exactly
                (variant.originTypeName
                    ++ "."
                    ++ variant.name
                )

        SwiftExpressionReference reference ->
            Print.exactly
                (reference |> qualifiedSwiftReferenceToString)

        SwiftExpressionFloat float ->
            Print.exactly (swiftNumberLiteralToString float)

        SwiftExpressionString string ->
            printSwiftString string

        SwiftExpressionWithLocalDeclaration expressionWithLetDeclarations ->
            printSwiftExpressionWithLocalDeclaration expressionWithLetDeclarations

        SwiftExpressionSwitch syntaxSwitch ->
            printSwiftExpressionSwitch syntaxSwitch

        SwiftExpressionLambda syntaxLambda ->
            printSwiftExpressionLambda syntaxLambda

        SwiftExpressionRecord fields ->
            printSwiftExpressionRecord fields

        SwiftExpressionRecordAccess syntaxRecordAccess ->
            printSwiftExpressionParenthesizedIfSpaceSeparated
                syntaxRecordAccess.record
                |> Print.followedBy
                    (Print.exactly
                        ("." ++ syntaxRecordAccess.field)
                    )


printSwiftExpressionCall :
    { called : SwiftExpression
    , arguments : List SwiftExpression
    }
    -> Print
printSwiftExpressionCall call =
    let
        calledPrint : Print
        calledPrint =
            printSwiftExpressionParenthesizedIfSpaceSeparated
                call.called

        argumentPrints : List Print
        argumentPrints =
            call.arguments
                |> List.map printSwiftExpressionNotParenthesized

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            argumentPrints
                |> Print.lineSpreadListMapAndCombine Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> calledPrint |> Print.lineSpread)
    in
    calledPrint
        |> Print.followedBy (Print.exactly "(")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.emptyOrLinebreakIndented fullLineSpread
                    |> Print.followedBy
                        (argumentPrints
                            |> Print.listIntersperseAndFlatten
                                (Print.exactly ","
                                    |> Print.followedBy
                                        (Print.spaceOrLinebreakIndented fullLineSpread)
                                )
                        )
                )
            )
        |> Print.followedBy (Print.emptyOrLinebreakIndented fullLineSpread)
        |> Print.followedBy (Print.exactly ")")


swiftNumberLiteralToString : Float -> String
swiftNumberLiteralToString float =
    let
        floatAsString : String
        floatAsString =
            float |> String.fromFloat
    in
    if floatAsString |> String.contains "." then
        floatAsString

    else
        floatAsString ++ ".0"


patternIsSpaceSeparated : SwiftPattern -> Bool
patternIsSpaceSeparated swiftPattern =
    case swiftPattern of
        SwiftPatternIgnore ->
            False

        SwiftPatternTrue ->
            False

        SwiftPatternFalse ->
            False

        SwiftPatternFloat _ ->
            False

        SwiftPatternString _ ->
            False

        SwiftPatternVariable _ ->
            False

        SwiftPatternVariant _ ->
            False

        SwiftPatternRecord _ ->
            False


printSwiftPatternParenthesizedIfSpaceSeparated : SwiftPattern -> Print
printSwiftPatternParenthesizedIfSpaceSeparated swiftPattern =
    if swiftPattern |> patternIsSpaceSeparated then
        printParenthesized
            { opening = "("
            , closing = ")"
            , inner = swiftPattern |> printSwiftPatternNotParenthesized
            }

    else
        swiftPattern |> printSwiftPatternNotParenthesized


swiftPatternIntroducedVariables : SwiftPattern -> List String
swiftPatternIntroducedVariables swiftPattern =
    -- IGNORE TCO
    case swiftPattern of
        SwiftPatternIgnore ->
            []

        SwiftPatternTrue ->
            []

        SwiftPatternFalse ->
            []

        SwiftPatternFloat _ ->
            []

        SwiftPatternString _ ->
            []

        SwiftPatternVariable name ->
            [ name ]

        SwiftPatternVariant swiftPatternVariant ->
            swiftPatternVariant.values
                |> List.concatMap swiftPatternIntroducedVariables

        SwiftPatternRecord fields ->
            fields
                |> FastDict.values
                |> List.concatMap swiftPatternIntroducedVariables


swiftPatternAsExpression : SwiftPattern -> SwiftExpression
swiftPatternAsExpression swiftPattern =
    -- IGNORE TCO
    case swiftPattern of
        SwiftPatternIgnore ->
            SwiftExpressionRecord FastDict.empty

        SwiftPatternFalse ->
            SwiftExpressionReference
                { moduleOrigin = Nothing
                , name = "false"
                }

        SwiftPatternTrue ->
            SwiftExpressionReference
                { moduleOrigin = Nothing
                , name = "true"
                }

        SwiftPatternFloat float ->
            SwiftExpressionFloat float

        SwiftPatternString string ->
            SwiftExpressionString string

        SwiftPatternVariable name ->
            SwiftExpressionReference
                { moduleOrigin = Nothing
                , name = name
                }

        SwiftPatternVariant variant ->
            case variant.values of
                [] ->
                    SwiftExpressionVariant
                        { originTypeName = variant.originTypeName
                        , name = variant.name
                        }

                value0 :: value1Up ->
                    SwiftExpressionCall
                        { called =
                            SwiftExpressionVariant
                                { originTypeName = variant.originTypeName
                                , name = variant.name
                                }
                        , arguments =
                            (value0 :: value1Up)
                                |> List.map swiftPatternAsExpression
                        }

        SwiftPatternRecord fields ->
            SwiftExpressionRecord
                (fields
                    |> FastDict.map
                        (\_ value ->
                            value |> swiftPatternAsExpression
                        )
                )


printSwiftExpressionLambda :
    { parameter0 : Maybe String
    , parameter1Up : List (Maybe String)
    , result : SwiftExpression
    }
    -> Print
printSwiftExpressionLambda syntaxLambda =
    Print.exactly "{ ("
        |> Print.followedBy
            ((syntaxLambda.parameter0 :: syntaxLambda.parameter1Up)
                |> Print.listMapAndIntersperseAndFlatten
                    printSwiftParameterForExpression
                    (Print.exactly ", ")
            )
        |> Print.followedBy (Print.exactly ") in")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printSwiftExpressionNotParenthesized
                            syntaxLambda.result
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "}")


printSwiftParameterForExpression : Maybe String -> Print
printSwiftParameterForExpression maybeVariable =
    case maybeVariable of
        Nothing ->
            Print.exactly "_"

        Just variableName ->
            Print.exactly variableName


printSwiftExpressionSwitch :
    { matched : SwiftExpression
    , case0 : { pattern : SwiftPattern, result : SwiftExpression }
    , case1Up : List { pattern : SwiftPattern, result : SwiftExpression }
    }
    -> Print
printSwiftExpressionSwitch swiftExpressionCase =
    let
        matchedPrint : Print
        matchedPrint =
            printSwiftExpressionNotParenthesized swiftExpressionCase.matched

        matchedPrintLineSpread : Print.LineSpread
        matchedPrintLineSpread =
            matchedPrint |> Print.lineSpread

        actualSwitch : Print
        actualSwitch =
            Print.exactly "switch "
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.emptyOrLinebreakIndented matchedPrintLineSpread
                            |> Print.followedBy matchedPrint
                        )
                    )
                |> Print.followedBy
                    (Print.emptyOrLinebreakIndented matchedPrintLineSpread)
                |> Print.followedBy (Print.exactly " {")
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.linebreakIndented
                            |> Print.followedBy
                                ((swiftExpressionCase.case0 :: swiftExpressionCase.case1Up)
                                    |> Print.listMapAndIntersperseAndFlatten
                                        printSwiftExpressionSingleCase
                                        (Print.linebreak
                                            |> Print.followedBy Print.linebreakIndented
                                        )
                                )
                        )
                    )
                |> Print.followedBy Print.linebreakIndented
                |> Print.followedBy (Print.exactly "}")
    in
    -- swift does not yet support using a switch as a value everywhere yet
    -- https://github.com/swiftlang/swift-evolution/blob/main/proposals/0380-if-switch-expressions.md#full-expressions
    printParenthesized
        { opening = "({ () in return "
        , closing = "}())"
        , inner = actualSwitch
        }


printSwiftExpressionWithLocalDeclaration :
    { declaration :
        { name : String
        , parameters : List (Maybe String)
        , result : SwiftExpression
        , type_ : Maybe SwiftType
        }
    , result : SwiftExpression
    }
    -> Print
printSwiftExpressionWithLocalDeclaration swiftExpressionWithLocalDeclaration =
    printParenthesized
        { opening = "({ () in "
        , closing = "}())"
        , inner =
            printSwiftExpressionWithSubsequentLocalDeclaration
                swiftExpressionWithLocalDeclaration
        }


printSwiftExpressionWithSubsequentLocalDeclaration :
    { declaration :
        { name : String
        , parameters : List (Maybe String)
        , result : SwiftExpression
        , type_ : Maybe SwiftType
        }
    , result : SwiftExpression
    }
    -> Print
printSwiftExpressionWithSubsequentLocalDeclaration swiftExpressionWithLocalDeclaration =
    -- IGNORE TCO
    (case swiftExpressionWithLocalDeclaration.declaration.parameters of
        [] ->
            -- if the value has generics, assume they originate from a module-level annotation
            -- and use a plain value instead of a function()
            printSwiftValueDeclaration
                { name = swiftExpressionWithLocalDeclaration.declaration.name
                , type_ = swiftExpressionWithLocalDeclaration.declaration.type_
                , result = swiftExpressionWithLocalDeclaration.declaration.result
                }

        parameter0 :: parameter1Up ->
            -- can't create a local function because we don't know types
            printSwiftValueDeclaration
                { name = swiftExpressionWithLocalDeclaration.declaration.name
                , type_ = swiftExpressionWithLocalDeclaration.declaration.type_
                , result =
                    SwiftExpressionLambda
                        { parameter0 = parameter0
                        , parameter1Up = parameter1Up
                        , result = swiftExpressionWithLocalDeclaration.declaration.result
                        }
                }
    )
        |> Print.followedBy Print.linebreak
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy
            (case swiftExpressionWithLocalDeclaration.result of
                SwiftExpressionWithLocalDeclaration subsequentSwiftExpressionWithLocalDeclaration ->
                    printSwiftExpressionWithSubsequentLocalDeclaration
                        subsequentSwiftExpressionWithLocalDeclaration

                resultNotWithLocalDeclarations ->
                    let
                        resultPrint : Print
                        resultPrint =
                            printSwiftExpressionNotParenthesized
                                resultNotWithLocalDeclarations
                    in
                    Print.exactly "return "
                        --|> Print.followedBy
                        --    (Print.spaceOrLinebreakIndented
                        --        (resultPrint |> Print.lineSpread)
                        --    )
                        |> Print.followedBy
                            resultPrint
            )


printSwiftExpressionSingleCase :
    { pattern : SwiftPattern, result : SwiftExpression }
    -> Print
printSwiftExpressionSingleCase branch =
    let
        patternPrint : Print
        patternPrint =
            printSwiftPatternNotParenthesized
                branch.pattern
    in
    Print.exactly
        ("case "
            ++ (case branch.pattern |> swiftPatternIntroducedVariables of
                    [] ->
                        ""

                    _ :: _ ->
                        "let "
               )
        )
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                patternPrint
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented
                (patternPrint |> Print.lineSpread)
            )
        |> Print.followedBy (Print.exactly ":")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printSwiftExpressionNotParenthesized
                            branch.result
                        )
                )
            )
        |> Print.followedBy (Print.exactly ";")


{-| Print value/function declarations into
an swift module called `Elm` in the global namespace that exposes all members.
Will also add some internal wrapper declarations.
-}
swiftDeclarationsToFileString :
    { valuesAndFunctions :
        FastDict.Dict
            String
            { parameters : List (Maybe String)
            , result : SwiftExpression
            , type_ : Maybe SwiftType
            }
    , typeAliases :
        FastDict.Dict
            String
            { parameters : List String
            , type_ : SwiftType
            }
    , choiceTypes :
        FastDict.Dict
            String
            { parameters : List String
            , variants : FastDict.Dict String (List SwiftType)
            }
    }
    -> String
swiftDeclarationsToFileString swiftDeclarations =
    let
        valueAndFunctionDeclarationsOrdered :
            List
                { name : String
                , parameters : List (Maybe String)
                , result : SwiftExpression
                , type_ : Maybe SwiftType
                }
        valueAndFunctionDeclarationsOrdered =
            swiftDeclarations.valuesAndFunctions
                |> fastDictMapAndToList
                    (\name valueOrFunctionInfo ->
                        { name = name
                        , type_ = valueOrFunctionInfo.type_
                        , parameters = valueOrFunctionInfo.parameters
                        , result = valueOrFunctionInfo.result
                        }
                    )

        typeAliasDeclarations :
            List
                { name : String
                , parameters : List String
                , type_ : SwiftType
                }
        typeAliasDeclarations =
            swiftDeclarations.typeAliases
                |> fastDictMapAndToList
                    (\name info ->
                        { name = name
                        , parameters = info.parameters
                        , type_ = info.type_
                        }
                    )

        choiceTypeDeclarations :
            List
                { name : String
                , parameters : List String
                , variants : FastDict.Dict String (List SwiftType)
                }
        choiceTypeDeclarations =
            swiftDeclarations.choiceTypes
                |> fastDictMapAndToList
                    (\name info ->
                        { name = name
                        , parameters = info.parameters
                        , variants = info.variants
                        }
                    )
    in
    """import Foundation

// using enum to create a namespace can't be instantiated or extend
public enum Elm {
"""
        ++ swiftDefaultDeclarations
        ++ """

"""
        ++ (typeAliasDeclarations
                |> Print.listMapAndIntersperseAndFlatten
                    (\swiftTypeAliasDeclaration ->
                        printSwiftTypealiasDeclaration swiftTypeAliasDeclaration
                    )
                    (Print.linebreak
                        |> Print.followedBy Print.linebreak
                    )
                |> Print.toString
           )
        ++ """

"""
        ++ (choiceTypeDeclarations
                |> Print.listMapAndIntersperseAndFlatten
                    (\swiftChoiceTypeDeclaration ->
                        printSwiftEnumDeclaration swiftChoiceTypeDeclaration
                    )
                    (Print.linebreak
                        |> Print.followedBy Print.linebreak
                    )
                |> Print.toString
           )
        ++ """

"""
        ++ (Print.withIndentAtNextMultipleOf4
                (valueAndFunctionDeclarationsOrdered
                    |> Print.listMapAndIntersperseAndFlatten
                        (\swiftValueOrFunctionDeclaration ->
                            Print.exactly "static public "
                                |> Print.followedBy
                                    (case swiftValueOrFunctionDeclaration.parameters of
                                        [] ->
                                            if swiftValueOrFunctionDeclaration.type_ |> maybeSwiftTypeContainsVariables then
                                                printSwiftFunctionDeclaration
                                                    { name = swiftValueOrFunctionDeclaration.name
                                                    , parameters = []
                                                    , type_ = swiftValueOrFunctionDeclaration.type_
                                                    , result = swiftValueOrFunctionDeclaration.result
                                                    }

                                            else
                                                printSwiftValueDeclaration
                                                    { name = swiftValueOrFunctionDeclaration.name
                                                    , type_ = swiftValueOrFunctionDeclaration.type_
                                                    , result = swiftValueOrFunctionDeclaration.result
                                                    }

                                        parameter0 :: parameter1Up ->
                                            printSwiftFunctionDeclaration
                                                { name = swiftValueOrFunctionDeclaration.name
                                                , parameters = parameter0 :: parameter1Up
                                                , type_ = swiftValueOrFunctionDeclaration.type_
                                                , result = swiftValueOrFunctionDeclaration.result
                                                }
                                    )
                        )
                        (Print.linebreak
                            |> Print.followedBy Print.linebreakIndented
                        )
                )
                |> Print.toString
           )
        ++ """
}
"""


printSwiftValueDeclaration :
    { name : String
    , type_ : Maybe SwiftType
    , result : SwiftExpression
    }
    -> Print
printSwiftValueDeclaration swiftValueOrFunctionDeclaration =
    Print.exactly
        ("let "
            ++ swiftValueOrFunctionDeclaration.name
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                ((case swiftValueOrFunctionDeclaration.type_ of
                    Nothing ->
                        Print.empty

                    Just declaredType ->
                        let
                            typePrint : Print
                            typePrint =
                                printSwiftTypeNotParenthesized
                                    declaredType

                            typeLineSpread : Print.LineSpread
                            typeLineSpread =
                                typePrint |> Print.lineSpread
                        in
                        Print.exactly ":"
                            |> Print.followedBy
                                (Print.emptyOrLinebreakIndented typeLineSpread
                                    |> Print.followedBy typePrint
                                )
                 )
                    |> Print.followedBy
                        (Print.exactly " =")
                    |> Print.followedBy Print.linebreakIndented
                    |> Print.followedBy
                        (printSwiftExpressionNotParenthesized
                            swiftValueOrFunctionDeclaration.result
                        )
                    |> Print.followedBy (Print.exactly ";")
                )
            )


maybeSwiftTypeContainsVariables : Maybe SwiftType -> Bool
maybeSwiftTypeContainsVariables maybeSwiftType =
    case maybeSwiftType of
        Nothing ->
            False

        Just declaredType ->
            Basics.not
                (FastSet.isEmpty
                    (declaredType
                        |> swiftTypeContainedVariables
                    )
                )


printSwiftFunctionDeclaration :
    { parameters : List (Maybe String)
    , name : String
    , type_ : Maybe SwiftType
    , result : SwiftExpression
    }
    -> Print
printSwiftFunctionDeclaration swiftFunctionDeclaration =
    (case swiftFunctionDeclaration.type_ of
        Nothing ->
            Print.exactly
                ("func "
                    ++ swiftFunctionDeclaration.name
                    ++ "("
                )
                |> Print.followedBy
                    (swiftFunctionDeclaration.parameters
                        |> Print.listMapAndIntersperseAndFlatten
                            printSwiftParameterForExpression
                            (Print.exactly ", ")
                    )
                |> Print.followedBy
                    (Print.exactly ")")

        Just declaredType ->
            let
                typedParametersAndResultType :
                    { parameters : List { name : Maybe String, type_ : SwiftType }
                    , result : SwiftType
                    }
                typedParametersAndResultType =
                    case declaredType of
                        SwiftTypeFunction swiftTypeFunction ->
                            { parameters =
                                List.map2
                                    (\parameterType parameterName ->
                                        { name = parameterName, type_ = parameterType }
                                    )
                                    swiftTypeFunction.input
                                    swiftFunctionDeclaration.parameters
                            , result = swiftTypeFunction.output
                            }

                        declaredTypeNotFunction ->
                            -- TODO fail or something
                            { parameters = []
                            , result = declaredTypeNotFunction
                            }

                resultTypePrint : Print
                resultTypePrint =
                    printSwiftTypeNotParenthesized
                        typedParametersAndResultType.result

                typeLineSpread : Print.LineSpread
                typeLineSpread =
                    resultTypePrint |> Print.lineSpread

                generics : List String
                generics =
                    declaredType
                        |> swiftTypeContainedVariables
                        |> FastSet.toList
            in
            Print.exactly
                ((case generics of
                    [] ->
                        "func "
                            ++ swiftFunctionDeclaration.name

                    generic0 :: generic1Up ->
                        -- @:generic
                        "func "
                            ++ swiftFunctionDeclaration.name
                            ++ "<"
                            ++ ((generic0 :: generic1Up)
                                    |> List.map (\parameter -> parameter ++ ": Sendable")
                                    |> String.join ", "
                               )
                            ++ ">"
                 )
                    ++ "("
                )
                |> Print.followedBy
                    (typedParametersAndResultType.parameters
                        |> Print.listMapAndIntersperseAndFlatten
                            (\typedParameter ->
                                let
                                    parameterTypePrint : Print
                                    parameterTypePrint =
                                        typedParameter.type_
                                            |> printSwiftTypeNotParenthesized
                                in
                                (case typedParameter.name of
                                    Nothing ->
                                        Print.exactly "_"

                                    Just variableName ->
                                        Print.exactly ("_ " ++ variableName)
                                )
                                    |> Print.followedBy
                                        (Print.exactly ":")
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.spaceOrLinebreakIndented
                                                (parameterTypePrint |> Print.lineSpread)
                                                |> Print.followedBy
                                                    parameterTypePrint
                                            )
                                        )
                            )
                            (Print.exactly ", ")
                    )
                |> Print.followedBy
                    (Print.exactly ") ->")
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.spaceOrLinebreakIndented typeLineSpread
                            |> Print.followedBy
                                resultTypePrint
                        )
                    )
    )
        |> Print.followedBy
            (Print.exactly " {")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printSwiftExpressionNotParenthesized
                            swiftFunctionDeclaration.result
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "}")


swiftDefaultDeclarations : String
swiftDefaultDeclarations =
    """
    // in theory Optional.none and Optional.some exist
    // and they even correctly say
    //     Optional<Optional<Int>>.none == Optional.some(Optional<Int>.none))
    //     is false
    // Since they are
    //   - both displayed as nil
    //   - Optional.some(x) has the same type as x (hand-wave)
    // I'm a bit worried about how shaky to use they might be though
    public enum Maybe_Maybe<A: Sendable>: Sendable {
        case Nothing
        case Just(_ value: A)
    }

    public indirect enum List_List<A: Sendable>: Sendable {
        case Empty
        case Cons(_ head: A, _ tail: List_List<A>)
    }

    public enum Basics_Order: Sendable {
        case LT
        case EQ
        case GT
    }

    static func debug_toString<A>(_ data: A) -> String {
        String(reflecting: data)
    }

    static func debug_log<A>(_ tag: String, _ data: A) -> A {
        print(tag, data)
        return data
    }

    static func debug_todo<A>(_ message: String) -> A {
        fatalError("TODO " + message)
    }

    static func basics_identity<A>(_ a: A) -> A {
        a
    }

    static func basics_always<Ignored, Kept>(_ kept: Kept, _: Ignored) -> Kept {
        kept
    }

    static func basics_not(_ bool: Bool) -> Bool {
        !bool
    }

    static func basics_or(_ a: Bool, _ b: Bool) -> Bool {
        a || b
    }

    static func basics_and(_ a: Bool, _ b: Bool) -> Bool {
        a && b
    }

    static func basics_eq<A>(_ a: A, _ b: A) -> Bool {
        if let a = a as? AnyHashable,
            let b = b as? AnyHashable
        {
            return a == b
        } else {
            fatalError("== on non-AnyHashable types")
        }

    }

    static func basics_neq<A>(_ a: A, _ b: A) -> Bool {
        guard let a = a as? AnyHashable,
            let b = b as? AnyHashable
        else { fatalError("== on non-AnyHashable types") }
        return a != b
    }

    static func basics_lt(_ a: Double, _ b: Double) -> Bool {
        a < b
    }

    static func basics_gt(_ a: Double, _ b: Double) -> Bool {
        a > b
    }

    static func basics_le(_ a: Double, _ b: Double) -> Bool {
        a <= b
    }

    static func basics_ge(_ a: Double, _ b: Double) -> Bool {
        a >= b
    }

    static func basics_compare<A: Comparable>(_ a: A, _ b: A) -> Basics_Order {
        if a < b {
            .LT
        } else if a > b {
            .GT
        } else {
            .EQ
        }
    }

    static func basics_compare<T: RawRepresentable>(_ a: T, _ b: T) -> Basics_Order
    where T.RawValue: Comparable {
        if a.rawValue < b.rawValue {
            .LT
        } else if a.rawValue > b.rawValue {
            .GT
        } else {
            .EQ
        }
    }


    static func basics_compare<A: Comparable>(_ aList: List_List<A>, _ bList: List_List<A>)
        -> Basics_Order
    {
        switch (aList, bList) {
        case (.Empty, .Empty): .EQ
        case (.Empty, .Cons(_, _)): .LT
        case (.Cons(_, _), .Empty): .GT
        case let (.Cons(aHead, aTail), .Cons(bHead, bTail)):
            if aHead < bHead {
                .LT
            } else if aHead > bHead {
                .GT
            } else {
                basics_compare(aTail, bTail)
            }
        }
    }

    static func basics_min(_ a: Double, _ b: Double) -> Double {
        Double.minimum(a, b)
    }

    static func basics_max(_ a: Double, _ b: Double) -> Double {
        Double.maximum(a, b)
    }

    static func basics_negate(_ float: Double) -> Double {
        -float
    }

    static func basics_abs(_ float: Double) -> Double {
        abs(float)
    }

    static func basics_sqrt(_ float: Double) -> Double {
        float.squareRoot()
    }

    static func basics_truncate(_ float: Double) -> Double {
        float.rounded(.towardZero)
    }

    static func basics_round(_ float: Double) -> Double {
        float.rounded()
    }

    static func basics_floor(_ float: Double) -> Double {
        float.rounded(.down)
    }

    static func basics_ceiling(_ float: Double) -> Double {
        float.rounded(.up)
    }

    static func basics_isInfinite(_ float: Double) -> Bool {
        float.isInfinite
    }

    static func basics_isNaN(_ float: Double) -> Bool {
        float.isNaN
    }

    static func basics_add(_ a: Double, _ b: Double) -> Double {
        a + b
    }

    static func basics_sub(_ base: Double, _ toSubtract: Double) -> Double {
        base - toSubtract
    }

    static func basics_mul(_ a: Double, _ b: Double) -> Double {
        a * b
    }

    static func basics_idiv(_ toDivide: Double, _ divisor: Double) -> Double {
        (toDivide / divisor).rounded(.towardZero)
    }

    static func basics_fdiv(_ toDivide: Double, _ divisor: Double) -> Double {
        toDivide / divisor
    }

    static func basics_remainderBy(_ divisor: Double, _ toDivide: Double) -> Double {
        toDivide.truncatingRemainder(dividingBy: divisor)
    }

    static func basics_modBy(_ divisor: Double, _ toDivide: Double) -> Double {
        toDivide.remainder(dividingBy: divisor)
    }

    static func basics_pow(_ base: Double, _ exponent: Double) -> Double {
        pow(base, exponent)
    }

    static func char_toCode(_ char: Character) -> Double {
        if let code = char.utf16.first {
            Double(code)
        } else {
            Double.nan
        }
    }

    static func char_fromCode(_ charCode: Double) -> Character {
        return if let scalar = UnicodeScalar(Int(charCode)) {
            Character(scalar)
        } else {
            "\\0"
        }
    }

    static func char_isHexDigit(_ char: Character) -> Bool {
        char.isHexDigit
    }

    static func char_toUpper(_ char: Character) -> Character {
        if let uppercasedChar = char.uppercased().first {
            uppercasedChar
        } else {
            "\\0"
        }
    }

    static func char_toLower(_ char: Character) -> Character {
        if let lowercasedChar = char.lowercased().first {
            lowercasedChar
        } else {
            "\\0"
        }
    }

    static func string_fromChar(_ char: Character) -> String {
        String(char)
    }

    static func string_fromFloat(_ float: Double) -> String {
        String(float)
    }

    static func string_toInt(_ str: String) -> Maybe_Maybe<Double> {
        if let parseResult = Int(str) {
            .Just(Double(parseResult))
        } else {
            .Nothing
        }
    }

    static func string_toFloat(_ str: String) -> Maybe_Maybe<Double> {
        if let parseResult = Double(str) {
            .Just(parseResult)
        } else {
            .Nothing
        }
    }

    static func string_toList(_ str: String) -> List_List<Character> {
        var chars: List_List<Character> = .Empty
        for char in str.reversed() {
            chars = .Cons(char, chars)
        }
        return chars
    }

    static func string_fromList(_ chars: List_List<Character>) -> String {
        var remainingChars = chars
        var stringBuffer = String()
        while true {
            switch remainingChars {
            case .Empty:
                return stringBuffer
            case .Cons(let head, let tail):
                stringBuffer.append(head)
                remainingChars = tail
            }
        }
    }

    static func string_length(_ str: String) -> Double {
        Double(str.utf16.count)
    }

    static func string_isEmpty(_ str: String) -> Bool {
        str.isEmpty
    }

    static func string_cons(_ headChar: Character, _ tailString: String) -> String {
        String(headChar) + tailString
    }

    static func string_append(_ earlier: String, _ later: String) -> String {
        earlier + later
    }

    static func string_contains(_ sub: String, _ str: String) -> Bool {
        str.contains(sub)
    }

    static func string_startsWith(_ start: String, _ str: String) -> Bool {
        str.hasPrefix(start)
    }

    static func string_endsWith(_ end: String, _ str: String) -> Bool {
        str.hasSuffix(end)
    }

    static func string_concat(_ segments: List_List<String>) -> String {
        var remainingSegments = segments
        var stringBuffer = String()
        while true {
            switch remainingSegments {
            case .Empty:
                return stringBuffer
            case .Cons(let head, let tail):
                stringBuffer.append(contentsOf: head)
                remainingSegments = tail
            }
        }
    }

    static func string_join(_ inBetween: String, _ segments: List_List<String>) -> String {
        switch segments {
        case .Empty:
            return ""
        case .Cons(let headSegment, let tailSegments):
            var remainingCharacters = tailSegments
            var stringBuffer = String()
            stringBuffer.append(contentsOf: headSegment)
            while true {
                switch remainingCharacters {
                case .Empty:
                    return stringBuffer
                case .Cons(let head, let tail):
                    stringBuffer.append(contentsOf: inBetween)
                    stringBuffer.append(contentsOf: head)
                    remainingCharacters = tail
                }
            }
        }
    }

    static func string_reverse(_ str: String) -> String {
        String(decoding: Array(str.utf16).reversed(), as: UTF16.self)
    }

    static func string_dropLeft(_ countToSkip: Double, _ str: String) -> String {
        String(decoding: Array(str.utf16.dropFirst(Int(countToSkip))), as: UTF16.self)
    }

    static func string_dropRight(_ countToSkip: Double, _ str: String) -> String {
        String(decoding: Array(str.utf16.dropLast(Int(countToSkip))), as: UTF16.self)
    }

    static func string_left(_ countToTake: Double, _ str: String) -> String {
        String(decoding: Array(str.utf16.prefix(Int(countToTake))), as: UTF16.self)
    }

    static func string_right(_ countToTake: Double, _ str: String) -> String {
        String(decoding: Array(str.utf16.suffix(Int(countToTake))), as: UTF16.self)
    }

    static func string_padRight(_ desiredLength: Double, _ padChar: String, _ str: String) -> String
    {
        str + String(repeating: padChar, count: Int(desiredLength) - str.utf16.count)
    }

    static func string_padLeft(_ desiredLength: Double, _ padChar: String, _ str: String) -> String
    {
        String(repeating: padChar, count: max(0, Int(desiredLength) - str.utf16.count)) + str
    }

    static func string_repeat(_ count: Double, _ segment: String) -> String {
        String(repeating: segment, count: Int(count))
    }

    static func string_replace(_ toReplace: String, _ replacement: String, _ str: String) -> String
    {
        str.replacing(toReplace, with: replacement)
    }

    static func string_toLower(_ str: String) -> String {
        str.lowercased()
    }

    static func string_toUpper(_ str: String) -> String {
        str.uppercased()
    }

    static func string_trimLeft(_ str: String) -> String {
        String(
            str.trimmingPrefix(while: { character in
                character.isWhitespace || character.isNewline
            })
        )
    }

    static func string_trimRight(_ str: String) -> String {
        let startToRestoreAfterTrimming =
            str.prefix(while: { character in
                character.isWhitespace || character.isNewline
            })
        return String(startToRestoreAfterTrimming)
            + String(str.trimmingCharacters(in: .whitespacesAndNewlines))
    }

    static func string_trim(_ str: String) -> String {
        str.trimmingCharacters(in: .whitespacesAndNewlines)
    }

    static func string_map(_ characterChange: (Character) -> Character, _ str: String) -> String {
        String(str.map(characterChange))
    }

    static func string_filter(_ keepCharacter: (Character) -> Bool, _ str: String) -> String {
        str.filter(keepCharacter)
    }

    static func string_lines(_ str: String) -> List_List<String> {
        arrayToList_List(str.components(separatedBy: .newlines))
    }

    static func string_split(_ separator: String, _ str: String) -> List_List<String> {
        arrayToList_List(str.split(separator: separator).map({ sub in String(sub) }))
    }

    static func string_all(_ isExpected: (Character) -> Bool, _ str: String) -> Bool {
        str.allSatisfy(isExpected)
    }

    static func string_any(_ isOdd: (Character) -> Bool, _ str: String) -> Bool {
        return str.contains(where: isOdd)
    }

    static func string_slice(_ start: Double, _ end: Double, _ str: String) -> String {
        if start >= 0 && start + 1 == end {
            return String(
                str.utf16[
                    str.utf16.index(
                        str.utf16.startIndex, offsetBy: Int(start))
                ])
        } else {
            // likely slow. Check, then find something faster
            let realStartIndexInclusive =
                if start >= 0 {
                    Int(start)
                } else {
                    str.count + Int(start)
                }
            let realEndIndexExclusive =
                if end >= 0 {
                    Int(end)
                } else {
                    str.count + Int(end)
                }
            return String(
                decoding: str.utf16[
                    str.utf16.index(
                        str.utf16.startIndex, offsetBy: realStartIndexInclusive
                    )..<str.utf16.index(
                        str.utf16.startIndex, offsetBy: realEndIndexExclusive
                    )
                ],
                as: UTF16.self
            )
        }
    }

    static func string_foldl<Folded>(
        _ reduce: (Character, Folded) -> Folded,
        _ initialFolded: Folded,
        _ str: String
    ) -> Folded {
        str.reduce(
            initialFolded,
            { (soFar, char) in
                reduce(char, soFar)
            }
        )
    }

    static func string_foldr<Folded>(
        _ reduce: (Character, Folded) -> Folded,
        _ initialFolded: Folded,
        _ str: String
    ) -> Folded {
        str.reversed().reduce(
            initialFolded,
            { (soFar, char) in
                reduce(char, soFar)
            }
        )
    }

    private static func arrayToList_List<A>(_ array: [A]) -> List_List<A> {
        var soFar: List_List<A> = .Empty
        for element in array.reversed() {
            soFar = .Cons(element, soFar)
        }
        return soFar
    }

    private static func list_ListToArray<A>(_ fullList: List_List<A>) -> [A] {
        var soFar: [A] = Array()
        var remainingList = fullList
        while true {
            switch remainingList {
            case .Empty:
                return soFar
            case .Cons(let remainingHead, let remainingTail):
                soFar.append(remainingHead)
                remainingList = remainingTail
            }
        }
    }

    static func list_singleton<A>(_ onlyElement: A) -> List_List<A> {
        .Cons(onlyElement, .Empty)
    }

    static func list_isEmpty<A>(_ list: List_List<A>) -> Bool {
        switch list {
        case .Empty: true
        case .Cons(_, _): false
        }
    }

    static func list_length<A>(_ list: List_List<A>) -> Double {
        list_foldl({ (_, soFar) in soFar + 1 }, 0, list)
    }

    static func list_foldl<A, Folded>(
        _ reduce: (A, Folded) -> Folded,
        _ initialFolded: Folded,
        _ list: List_List<A>
    ) -> Folded {
        var foldedSoFar = initialFolded
        var remainingList = list
        while true {
            switch remainingList {
            case .Empty:
                return foldedSoFar
            case .Cons(let head, let tail):
                foldedSoFar = reduce(head, initialFolded)
                remainingList = tail
            }
        }
    }

    static func list_foldr<A, Folded>(
        _ reduce: (A, Folded) -> Folded,
        _ initialFolded: Folded,
        _ list: List_List<A>
    ) -> Folded {
        list_foldl(reduce, initialFolded, list_reverse(list))
    }

    static func list_reverse<A>(_ list: List_List<A>) -> List_List<A> {
        list_foldl(List_List.Cons, .Empty, list)
    }

    static func list_all<A>(_ isExpected: (A) -> Bool, _ list: List_List<A>) -> Bool {
        var remainingList = list
        while true {
            switch remainingList {
            case .Empty:
                return true
            case .Cons(let head, let tail):
                if !isExpected(head) {
                    return false
                } else {
                    remainingList = tail
                }
            }
        }
    }

    static func list_any<A>(_ isOdd: (A) -> Bool, _ list: List_List<A>) -> Bool {
        var remainingList = list
        while true {
            switch remainingList {
            case .Empty:
                return false
            case .Cons(let head, let tail):
                if isOdd(head) {
                    return true
                } else {
                    remainingList = tail
                }
            }
        }
    }

    static func list_member<A>(_ needle: (A), _ list: List_List<A>) -> Bool {
        list_any({ element in basics_eq(element, needle) }, list)
    }

    static func list_drop<A>(_ countToSkip: Double, _ list: List_List<A>) -> List_List<A> {
        var remainingCountToSkip = countToSkip
        var remainingList = list
        while remainingCountToSkip >= 1 {
            switch remainingList {
            case .Empty:
                return remainingList
            case .Cons(_, let tail):
                remainingList = tail
                remainingCountToSkip -= 1
            }
        }
        return remainingList
    }

    static func list_take<A>(_ countToTake: Double, _ list: List_List<A>) -> List_List<A> {
        var remainingCountToTake = countToTake
        var remainingList = list
        var takenElementsArraySoFar: [A] = []
        while remainingCountToTake >= 1 {
            switch remainingList {
            case .Empty:
                return arrayToList_List(takenElementsArraySoFar)
            case .Cons(let head, let tail):
                takenElementsArraySoFar.append(head)
                remainingList = tail
                remainingCountToTake -= 1
            }
        }
        return arrayToList_List(takenElementsArraySoFar)
    }

    static func list_intersperse<A>(_ inBetween: A, _ list: List_List<A>) -> List_List<A> {
        switch list {
        case .Empty: .Empty
        case .Cons(let head, let tail):
            list_foldr(
                { (element, soFar) in
                    .Cons(element, .Cons(inBetween, soFar))
                },
                list_singleton(head),
                tail
            )
        }
    }

    static func list_map<A, B>(_ elementChange: (A) -> B, _ list: List_List<A>) -> List_List<B> {
        list_foldr(
            { (element, soFar) in
                .Cons(elementChange(element), soFar)
            },
            .Empty,
            list
        )
    }

    static func list_indexedMap<A, B>(
        _ indexedElementChange: (Double, A) -> B,
        _ list: List_List<A>
    )
        -> List_List<B>
    {
        list_foldr(
            { (element, soFar: (index: Double, list: List_List<B>)) in
                (
                    index: soFar.index + 1,
                    list: .Cons(indexedElementChange(soFar.index, element), soFar.list)
                )
            },
            (index: list_length(list), list: .Empty),
            list
        ).list
    }

    static func list_map2<A, B, C>(
        _ combineAb: (A, B) -> C,
        _ aList: List_List<A>,
        _ bList: List_List<B>
    ) -> List_List<C> {
        var remainingAList = aList
        var remainingBList = bList
        var combinedArraySoFar: [C] = []
        while true {
            switch (a: remainingAList, b: remainingBList) {
            case (a: .Empty, b: .Empty):
                return arrayToList_List(combinedArraySoFar)
            case (a: .Empty, b: .Cons(_, _)):
                return arrayToList_List(combinedArraySoFar)
            case (a: .Cons(_, _), b: .Empty):
                return arrayToList_List(combinedArraySoFar)
            case (a: .Cons(let aHead, let aTail), b: .Cons(let bHead, let bTail)):
                remainingAList = aTail
                remainingBList = bTail
                combinedArraySoFar.append(combineAb(aHead, bHead))
            }
        }
    }

    static func list_zip<A, B>(_ aList: List_List<A>, _ bList: List_List<B>)
        -> List_List<(first: A, second: B)>
    {
        list_map2({ (a, b) in (first: a, second: b) }, aList, bList)
    }

    static func list_unzip<A, B>(_ abList: List_List<(first: A, second: B)>)
        -> (first: List_List<A>, second: List_List<B>)
    {
        (
            first: list_map({ ab in ab.first }, abList),
            second: list_map({ ab in ab.second }, abList)
        )
    }

    static func list_filter<A>(_ keepElement: (A) -> Bool, _ list: List_List<A>) -> List_List<A> {
        list_foldr(
            { (element, soFar) in
                if keepElement(element) {
                    soFar
                } else {
                    .Cons(element, soFar)
                }
            },
            .Empty,
            list
        )
    }

    static func list_filterMap<A, B>(
        _ element_toMaybe_Maybe: (A) -> Maybe_Maybe<B>,
        _ list: List_List<A>
    ) -> List_List<B> {
        list_foldr(
            { (element, soFar) in
                switch element_toMaybe_Maybe(element) {
                case .Nothing:
                    soFar
                case .Just(let value):
                    .Cons(value, soFar)
                }
            },
            .Empty,
            list
        )
    }

    static func list_append<A>(_ earlier: List_List<A>, _ later: List_List<A>) -> List_List<A> {
        list_foldr(
            { (earlierElement, soFar) in
                .Cons(earlierElement, soFar)
            },
            later,
            earlier
        )
    }

    static func list_concatMap<A, B>(_ elementToList: (A) -> List_List<B>, _ list: List_List<A>)
        -> List_List<B>
    {
        return list_foldr(
            { (element, soFar) in
                list_append(elementToList(element), soFar)
            },
            .Empty,
            list
        )
    }

    static func list_concat<A>(_ list: List_List<List_List<A>>) -> List_List<A> {
        list_foldr(
            { (element, soFar) in
                list_append(element, soFar)
            },
            .Empty,
            list
        )
    }

    static func list_repeat<A>(_ count: Double, _ element: A) -> List_List<A> {
        if count <= 0 {
            return .Empty
        } else {
            var soFar = List_List<A>.Empty
            for _ in 1...Int(count) {
                soFar = .Cons(element, soFar)
            }
            return soFar
        }
    }

    static func list_range(_ start: Double, _ end: Double) -> List_List<Double> {
        if start > end {
            return .Empty
        } else {
            var soFar: List_List<Double> = .Empty
            for i in (Int(end)...Int(start)).reversed() {
                soFar = .Cons(Double(i), soFar)
            }
            return soFar
        }
    }
    static func list_sum(_ list: List_List<Double>) -> Double {
        list_foldl(basics_add, 0, list)
    }

    static func list_product(_ list: List_List<Double>) -> Double {
        list_foldl(basics_mul, 1, list)
    }

    static func list_maximum(_ list: List_List<Double>) -> Maybe_Maybe<Double> {
        return switch list {
        case .Empty:
            .Nothing
        case .Cons(let head, let tail):
            .Just(list_foldl(Double.maximum, head, tail))
        }
    }

    static func list_minimum(_ list: List_List<Double>) -> Maybe_Maybe<Double> {
        return switch list {
        case .Empty:
            .Nothing
        case .Cons(let head, let tail):
            .Just(list_foldl(Double.minimum, head, tail))
        }
    }

    static func list_sortWith<A>(_ elementCompare: (A, A) -> Basics_Order, _ list: List_List<A>)
        -> List_List<A>
    {
        var asArray = list_ListToArray(list)
        asArray.sort(by: { (a, b) in elementCompare(a, b) == .LT })  // mutate
        return arrayToList_List(asArray)
    }

    static func list_sortBy<A, Comp>(_ elementToComparable: (A) -> Comp, _ list: List_List<A>)
        -> List_List<A>
    where Comp: Comparable {
        var asArray = list_ListToArray(list)
        asArray.sort(by: { (a, b) in elementToComparable(a) < elementToComparable(b) })  // mutate
        return arrayToList_List(asArray)
    }

    static func list_sort<Comp>(_ list: List_List<Comp>)
        -> List_List<Comp>
    where Comp: Comparable {
        var asArray = list_ListToArray(list)
        asArray.sort(by: { (a, b) in a < b })  // mutate
        return arrayToList_List(asArray)
    }
"""


fastDictMapAndToList :
    (key -> value -> element)
    -> FastDict.Dict key value
    -> List element
fastDictMapAndToList keyValueToElement fastDict =
    fastDict
        |> FastDict.foldr
            (\key value soFar ->
                keyValueToElement key value
                    :: soFar
            )
            []


listMapAndCombineOk : (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOk elementToResult list =
    listMapAndCombineOkFrom [] elementToResult list


listMapAndCombineOkFrom : List ok -> (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOkFrom soFar elementToResult list =
    case list of
        [] ->
            Ok (soFar |> List.reverse)

        head :: tail ->
            case head |> elementToResult of
                Err headErr ->
                    Err headErr

                Ok headOk ->
                    listMapAndCombineOkFrom (headOk :: soFar)
                        elementToResult
                        tail
