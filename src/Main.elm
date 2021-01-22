module Main exposing (..)

import Browser
import Html exposing (Html, a, article, aside, button, div, header, i, input, label, nav, section, span, text)
import Html.Attributes exposing (class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias DefinitionHash =
    String


type alias NamespacePath =
    List String


type alias TypeSignature =
    String


type alias FunctionBody =
    String


type alias Definition =
    { hash : DefinitionHash
    , path :
        NamespacePath
    , name : String
    , type_ : TypeSignature
    , body : FunctionBody
    }


type alias DefinitionView =
    { showCode : Bool
    , showDocs : Bool
    }


type alias OpenDefinition =
    ( Definition, DefinitionView )


type alias Model =
    { query : String
    , openDefinitions : List OpenDefinition
    }


init : Model
init =
    { query = ""
    , openDefinitions =
        [ ( { hash = "#123"
            , path = [ "base", "v1", "List" ]
            , name = "flatMap"
            , type_ = "(a ->{e} [b]) -> [a] ->{e} [b]"
            , body = "flatMap body"
            }
          , { showCode = False, showDocs = False }
          )
        , ( { hash = "#abc"
            , path = [ "base", "v1", "List" ]
            , name = "map"
            , type_ = "(a ->{𝕖} b) -> [a] ->{𝕖} [b]"
            , body = "map body"
            }
          , { showCode = False, showDocs = False }
          )
        ]
    }



-- UPDATE


type Msg
    = UpdateQuery String
    | CloseDefinition DefinitionHash
    | ToggleShowCode DefinitionHash


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateQuery query ->
            { model | query = query }

        CloseDefinition hash ->
            let
                nextOpenDefinitions =
                    List.filter (\( d, _ ) -> d.hash /= hash) model.openDefinitions
            in
            { model | openDefinitions = nextOpenDefinitions }

        ToggleShowCode hash ->
            let
                toggleShowCode ( def, viewSettings ) =
                    if def.hash == hash then
                        ( def, { viewSettings | showCode = not viewSettings.showCode } )

                    else
                        ( def, viewSettings )

                nextOpenDefinitions =
                    List.map toggleShowCode model.openDefinitions
            in
            { model | openDefinitions = nextOpenDefinitions }



-- VIEW


icon : String -> Html msg
icon name =
    i [ class ("fas fa-" ++ name) ] []


viewNamespaceTree : Model -> Html Msg
viewNamespaceTree model =
    div [] []


viewNamespacePath : NamespacePath -> Html msg
viewNamespacePath path =
    let
        namespaceLinks =
            List.map (\p -> a [ class "namespace" ] [ text p ]) path

        slash =
            span [ class "slash" ] [ text "/" ]
    in
    label
        [ class "definition-namespace-path" ]
        (List.intersperse slash namespaceLinks)


viewNothing : Html msg
viewNothing =
    text ""


viewDefinitionDetails : OpenDefinition -> Html msg
viewDefinitionDetails ( definition, viewSettings ) =
    if viewSettings.showCode then
        div [ class "definition-details" ] [ text definition.body ]

    else
        viewNothing


viewDefinition : OpenDefinition -> Html Msg
viewDefinition ( definition, viewSettings ) =
    let
        showCodeCaret =
            if viewSettings.showCode then
                "caret-down"

            else
                "caret-right"
    in
    div [ class "definition" ]
        [ div
            [ class "definition-summary" ]
            [ a [ class "action", onClick (ToggleShowCode definition.hash) ]
                [ icon showCodeCaret ]
            , div [ class "definition-info" ]
                [ viewNamespacePath definition.path
                , label [] [ text definition.name ]
                ]
            , a [ class "action", onClick (CloseDefinition definition.hash) ]
                [ icon "times" ]
            ]
        , viewDefinitionDetails ( definition, viewSettings )
        ]


view : Model -> Html Msg
view model =
    div []
        [ header [ id "main-header" ] [ text "Unison" ]
        , article [ id "panes" ]
            [ section [ id "main-nav-pane" ]
                [ header [ id "definition-search", class "pane-header" ]
                    [ icon "search"
                    , input
                        [ type_ "text"
                        , placeholder "Namespace, name, or signature"
                        , value model.query
                        , onInput UpdateQuery
                        ]
                        []
                    ]
                ]
            , viewNamespaceTree model
            , section [ id "main-pane" ]
                [ header [ class "pane-header" ] []
                , div [] (List.map viewDefinition model.openDefinitions)
                ]
            ]
        ]
