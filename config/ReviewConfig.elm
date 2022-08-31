module ReviewConfig exposing (config)

{-| This file configures elm-review
Please do not change anything here
-}

import Import.NoCoreModule
import Import.NoUnqualified
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoForbiddenFeatures
import NoImportingEverything
import NoMinimalUnderscorePattern
import NoMissingTypeAnnotation
import NoNegationOfBooleanOperator
import NoPrimitiveTypeAlias
import NoRecursiveUpdate
import NoSimpleLetBody
import NoSinglePatternCase
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUselessSubscriptions
import Review.Rule exposing (Rule)
import Simplify
import UseCamelCase
import UseConstantsForStyle
import UseEtaReductions
import UseInvertedOperators
import UseLogicalOperators
import UseNamingConventions


config : List Rule
config =
    -- Rules provided by third party libraries
    [ NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoPrimitiveTypeAlias.rule
    , NoRecursiveUpdate.rule
    , NoSimpleLetBody.rule
    , NoSinglePatternCase.rule NoSinglePatternCase.fixInArgument
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoUselessSubscriptions.rule
    , Simplify.rule Simplify.defaults
    , UseCamelCase.rule UseCamelCase.default

    -- Custom PLTP rules
    , Import.NoCoreModule.rule
    , Import.NoUnqualified.rule
        [ "Html"
        , "Html.Attributes"
        , "Html.Events"
        , "Svg"
        , "Svg.Attributes"
        ]
    , NoForbiddenFeatures.rule
        { operators = []
        , functions =
            [ "Decode.andThen"
            ]
        , letIn = False
        , algebraicDataTypes = False
        , lambda = False
        }
    , NoMinimalUnderscorePattern.rule 3
    , NoNegationOfBooleanOperator.rule
    , UseConstantsForStyle.rule
    , UseInvertedOperators.rule
    , UseNamingConventions.rule
    , UseLogicalOperators.rule
    , UseEtaReductions.rule UseEtaReductions.LocatedError
    ]