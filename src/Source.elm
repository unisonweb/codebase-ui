module Source exposing
    ( TermSource(..)
    , TypeSignature(..)
    , TypeSource(..)
    , ViewConfig(..)
    , numTermLines
    , numTypeLines
    , viewTermSignature
    , viewTermSource
    , viewTypeSource
    )

import Hash exposing (Hash)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Syntax exposing (Syntax)
import UI


type TypeSource
    = TypeSource Syntax
    | BuiltinType


type TypeSignature
    = TypeSignature Syntax


type TermSource
    = TermSource TypeSignature Syntax
    | BuiltinTerm TypeSignature


type ViewConfig msg
    = Rich (Hash -> msg)
    | Monochrome
    | Plain



-- HELPERS


numTypeLines : TypeSource -> Int
numTypeLines source =
    case source of
        TypeSource syntax ->
            Syntax.numLines syntax

        BuiltinType ->
            1


numTermLines : TermSource -> Int
numTermLines source =
    case source of
        TermSource _ syntax ->
            Syntax.numLines syntax

        BuiltinTerm (TypeSignature syntax) ->
            Syntax.numLines syntax



-- VIEW


viewTypeSource : ViewConfig msg -> TypeSource -> Html msg
viewTypeSource viewConfig source =
    let
        content =
            case source of
                TypeSource syntax ->
                    viewSyntax viewConfig syntax

                BuiltinType ->
                    span
                        []
                        [ span [] [ text "builtin " ]
                        , span [ class "data-type-keyword" ] [ text "type" ]
                        ]
    in
    viewCode viewConfig content


viewTermSignature : ViewConfig msg -> String -> TermSource -> Html msg
viewTermSignature viewConfig _ source =
    case source of
        TermSource (TypeSignature signature) _ ->
            viewCode viewConfig (viewSyntax viewConfig signature)

        BuiltinTerm (TypeSignature signature) ->
            viewCode viewConfig (viewSyntax viewConfig signature)


viewTermSource : ViewConfig msg -> String -> TermSource -> Html msg
viewTermSource viewConfig termName source =
    let
        content =
            case source of
                TermSource _ syntax ->
                    viewSyntax viewConfig syntax

                BuiltinTerm (TypeSignature syntax) ->
                    span
                        []
                        [ span [ class "hash-qualifier" ] [ text termName ]
                        , span [ class "type-ascription-colon" ] [ text " : " ]
                        , viewSyntax viewConfig syntax
                        ]
    in
    viewCode viewConfig content



-- VIEW HELPERS


viewCode : ViewConfig msg -> Html msg -> Html msg
viewCode viewConfig content =
    UI.codeBlock
        [ class (viewConfigToClassName viewConfig) ]
        content


viewConfigToClassName : ViewConfig msg -> String
viewConfigToClassName viewConfig =
    case viewConfig of
        Rich _ ->
            "rich"

        Monochrome ->
            "monochrome"

        Plain ->
            "plain"


viewSyntax : ViewConfig msg -> (Syntax.Syntax -> Html msg)
viewSyntax viewConfig =
    Syntax.view (viewConfigToSyntaxLinked viewConfig)


viewConfigToSyntaxLinked : ViewConfig msg -> Syntax.Linked msg
viewConfigToSyntaxLinked viewConfig =
    case viewConfig of
        Rich toReferenceClickMsg ->
            Syntax.Linked toReferenceClickMsg

        _ ->
            Syntax.NotLinked
