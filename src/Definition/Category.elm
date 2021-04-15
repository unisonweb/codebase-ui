module Definition.Category exposing (..)

import Definition.Term exposing (TermCategory(..))
import Definition.Type exposing (TypeCategory(..))
import UI.Icon as Icon exposing (Icon)


type Category
    = Type TypeCategory
    | Term TermCategory


name : Category -> String
name category =
    case category of
        Type c ->
            case c of
                DataType ->
                    "type"

                AbilityType ->
                    "ability"

        Term c ->
            case c of
                PlainTerm ->
                    "term"

                TestTerm ->
                    "test"

                DocTerm ->
                    "doc"


icon : Category -> Icon
icon category =
    case category of
        Type c ->
            case c of
                DataType ->
                    Icon.Type

                AbilityType ->
                    Icon.Ability

        Term c ->
            case c of
                PlainTerm ->
                    Icon.Term

                TestTerm ->
                    Icon.Test

                DocTerm ->
                    Icon.Doc
