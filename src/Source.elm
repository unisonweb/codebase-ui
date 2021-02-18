module Source exposing (TermSource(..), TypeSignature(..), TypeSource(..), viewTermSource, viewTypeSource)

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


viewTypeSource : TypeSource -> Html msg
viewTypeSource source =
    let
        content =
            case source of
                TypeSource syntax ->
                    Syntax.view syntax

                BuiltinType ->
                    span
                        []
                        [ span [ class "builtin" ] [ text "builtin " ]
                        , span [ class "data-type-keyword" ] [ text "type" ]
                        ]
    in
    UI.codeBlock content


viewTermSource : String -> TermSource -> Html msg
viewTermSource termName source =
    let
        content =
            case source of
                TermSource syntax ->
                    Syntax.view syntax

                BuiltinTerm (TypeSignature syntax) ->
                    span
                        []
                        [ span [ class "hash-qualifier" ] [ text termName ]
                        , span [ class "type-ascription-colon" ] [ text " : " ]
                        , Syntax.view syntax
                        , span [ class "blank" ] [ text "\n" ]
                        , span [ class "hash-qualifier" ] [ text termName ]
                        , span [ class "binding-equals" ] [ text " = " ]
                        , span [ class "builtin" ] [ text "builtin" ]
                        ]
    in
    UI.codeBlock content
