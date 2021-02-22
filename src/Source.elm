module Source exposing (TermSource(..), TypeSignature(..), TypeSource(..), viewTermSource, viewTypeSource)

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
    = TermSource Syntax
    | BuiltinTerm TypeSignature


viewTypeSource : (Hash -> msg) -> TypeSource -> Html msg
viewTypeSource toReferenceClickMsg source =
    let
        content =
            case source of
                TypeSource syntax ->
                    Syntax.view toReferenceClickMsg syntax

                BuiltinType ->
                    span
                        []
                        [ span [ class "builtin" ] [ text "builtin " ]
                        , span [ class "data-type-keyword" ] [ text "type" ]
                        ]
    in
    UI.codeBlock content


viewTermSource : (Hash -> msg) -> String -> TermSource -> Html msg
viewTermSource toReferenceClickMsg termName source =
    let
        content =
            case source of
                TermSource syntax ->
                    Syntax.view toReferenceClickMsg syntax

                BuiltinTerm (TypeSignature syntax) ->
                    span
                        []
                        [ span [ class "hash-qualifier" ] [ text termName ]
                        , span [ class "type-ascription-colon" ] [ text " : " ]
                        , Syntax.view toReferenceClickMsg syntax
                        , span [ class "blank" ] [ text "\n" ]
                        , span [ class "hash-qualifier" ] [ text termName ]
                        , span [ class "binding-equals" ] [ text " = " ]
                        , span [ class "builtin" ] [ text "builtin" ]
                        ]
    in
    UI.codeBlock content
