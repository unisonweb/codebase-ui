module Definition.Source exposing
    ( ViewConfig(..)
    , numTermLines
    , numTypeLines
    , viewTermSignature
    , viewTermSource
    , viewTypeSource
    )

import Definition.Term as Term exposing (TermSignature(..), TermSource)
import Definition.Type as Type exposing (TypeSource)
import Hash exposing (Hash)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Syntax
import UI


type ViewConfig msg
    = Rich (Hash -> msg)
    | Monochrome
    | Plain



-- HELPERS


numTypeLines : TypeSource -> Int
numTypeLines source =
    case source of
        Type.Source syntax ->
            Syntax.numLines syntax

        Type.Builtin ->
            1


numTermLines : TermSource -> Int
numTermLines source =
    case source of
        Term.Source _ syntax ->
            Syntax.numLines syntax

        Term.Builtin (TermSignature syntax) ->
            Syntax.numLines syntax



-- VIEW


viewTypeSource : ViewConfig msg -> TypeSource -> Html msg
viewTypeSource viewConfig source =
    let
        content =
            case source of
                Type.Source syntax ->
                    viewSyntax viewConfig syntax

                Type.Builtin ->
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
        Term.Source (TermSignature signature) _ ->
            viewCode viewConfig (viewSyntax viewConfig signature)

        Term.Builtin (TermSignature signature) ->
            viewCode viewConfig (viewSyntax viewConfig signature)


viewTermSource : ViewConfig msg -> String -> TermSource -> Html msg
viewTermSource viewConfig termName source =
    let
        content =
            case source of
                Term.Source _ syntax ->
                    viewSyntax viewConfig syntax

                Term.Builtin (TermSignature syntax) ->
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
