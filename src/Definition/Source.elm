module Definition.Source exposing
    ( Source(..)
    , ViewConfig(..)
    , isBuiltin
    , numTermLines
    , numTermSignatureLines
    , numTypeLines
    , view
    , viewNamedTermSignature
    , viewTermSignature
    , viewTermSource
    , viewTypeSource
    )

import Definition.Reference exposing (Reference)
import Definition.Term as Term exposing (TermSignature(..), TermSource)
import Definition.Type as Type exposing (TypeSource)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Syntax
import UI


type ViewConfig msg
    = Rich (Reference -> msg)
    | Monochrome
    | Plain


type
    Source
    -- Term name source
    = Term String TermSource
    | Type TypeSource



-- HELPERS


isBuiltin : Source -> Bool
isBuiltin source =
    case source of
        Type Type.Builtin ->
            True

        Term _ (Term.Builtin _) ->
            True

        _ ->
            False


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

        Term.Builtin (TermSignature signature) ->
            Syntax.numLines signature


numTermSignatureLines : TermSource -> Int
numTermSignatureLines source =
    case source of
        Term.Source (TermSignature signature) _ ->
            Syntax.numLines signature

        Term.Builtin (TermSignature signature) ->
            Syntax.numLines signature



-- VIEW


view : ViewConfig msg -> Source -> Html msg
view viewConfig source =
    case source of
        Type typeSource ->
            viewTypeSource viewConfig typeSource

        Term termName termSource ->
            viewTermSource viewConfig termName termSource


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


viewTermSignature : ViewConfig msg -> TermSignature -> Html msg
viewTermSignature viewConfig (TermSignature syntax) =
    viewCode viewConfig (viewSyntax viewConfig syntax)


viewNamedTermSignature : ViewConfig msg -> String -> TermSignature -> Html msg
viewNamedTermSignature viewConfig termName signature =
    viewCode viewConfig (viewNamedTermSignature_ viewConfig termName signature)


viewNamedTermSignature_ : ViewConfig msg -> String -> TermSignature -> Html msg
viewNamedTermSignature_ viewConfig termName (TermSignature syntax) =
    span
        []
        [ span [ class "hash-qualifier" ] [ text termName ]
        , span [ class "type-ascription-colon" ] [ text " : " ]
        , viewSyntax viewConfig syntax
        ]


viewTermSource : ViewConfig msg -> String -> TermSource -> Html msg
viewTermSource viewConfig termName source =
    let
        content =
            case source of
                Term.Source _ syntax ->
                    viewSyntax viewConfig syntax

                Term.Builtin signature ->
                    viewNamedTermSignature viewConfig termName signature
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
