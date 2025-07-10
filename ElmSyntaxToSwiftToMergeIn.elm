module ElmSyntaxToSwiftToMergeIn exposing
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
