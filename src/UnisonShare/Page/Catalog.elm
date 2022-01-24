module UnisonShare.Page.Catalog exposing (..)

import Api
import Env exposing (Env)
import Html exposing (Html, div, h1, input, strong, table, tbody, td, text, tr)
import Html.Attributes exposing (autofocus, class, classList, placeholder)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Http
import KeyboardShortcut exposing (KeyboardShortcut(..))
import KeyboardShortcut.Key as Key exposing (Key(..))
import KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import Perspective
import Project exposing (ProjectListing)
import RemoteData exposing (RemoteData(..), WebData)
import SearchResults exposing (SearchResults(..))
import Task
import UI
import UI.Card as Card
import UI.Click as Click
import UI.Icon as Icon
import UI.PageLayout as PageLayout exposing (PageLayout)
import UnisonShare.Catalog as Catalog exposing (Catalog)
import UnisonShare.Route as Route



-- MODEL


type alias SearchResult =
    ( ProjectListing, String )


type alias CatalogSearchResults =
    SearchResults SearchResult


type alias CatalogSearch =
    { query : String, results : CatalogSearchResults }


type alias LoadedModel =
    { search : CatalogSearch
    , hasFocus : Bool
    , catalog : Catalog
    , keyboardShortcut : KeyboardShortcut.Model
    }


type alias Model =
    WebData LoadedModel


init : Env -> ( Model, Cmd Msg )
init env =
    ( Loading, fetchCatalog env )


{-| Fetch the Catalog in sequence by first fetching the doc, then the
projectListings and finally merging them into a Catalog
-}
fetchCatalog : Env -> Cmd Msg
fetchCatalog env =
    let
        perspective =
            Perspective.toCodebasePerspective env.perspective
    in
    Api.getDefinition perspective [ "_catalog" ]
        |> Api.toTask env.apiBasePath Catalog.decodeCatalogMask
        |> Task.andThen
            (\catalog ->
                Api.projects Nothing
                    |> Api.toTask env.apiBasePath Project.decodeListings
                    |> Task.map (\projects -> ( catalog, projects ))
            )
        |> Task.map (\( cm, ps ) -> Catalog.catalog cm ps)
        |> Task.attempt FetchCatalogFinished



-- UPDATE


type Msg
    = UpdateQuery String
    | UpdateFocus Bool
    | ClearQuery
    | SelectProject ProjectListing
    | FetchCatalogFinished (Result Http.Error Catalog)
    | Keydown KeyboardEvent
    | KeyboardShortcutMsg KeyboardShortcut.Msg


update : Env -> Msg -> Model -> ( Model, Cmd Msg )
update env msg model =
    case ( msg, model ) of
        ( FetchCatalogFinished catalogResult, _ ) ->
            case catalogResult of
                Err e ->
                    ( Failure e, Cmd.none )

                Ok catalog ->
                    let
                        initModel =
                            { search = { query = "", results = SearchResults.empty }
                            , hasFocus = True
                            , catalog = catalog
                            , keyboardShortcut = KeyboardShortcut.init env.operatingSystem
                            }
                    in
                    ( Success initModel, Cmd.none )

        ( UpdateFocus hasFocus, Success m ) ->
            ( Success { m | hasFocus = hasFocus }, Cmd.none )

        ( UpdateQuery query, Success m ) ->
            let
                searchResults =
                    if String.length query < 3 then
                        SearchResults.empty

                    else
                        query
                            |> Catalog.search m.catalog
                            |> SearchResults.fromList
            in
            ( Success { m | search = { query = query, results = searchResults } }, Cmd.none )

        ( ClearQuery, Success m ) ->
            ( Success { m | search = { query = "", results = SearchResults.empty } }, Cmd.none )

        ( SelectProject project, Success m ) ->
            ( Success m, Route.navigateToProject env.navKey project )

        ( Keydown event, Success m ) ->
            let
                ( keyboardShortcut, kCmd ) =
                    KeyboardShortcut.collect m.keyboardShortcut event.key

                cmd =
                    Cmd.map KeyboardShortcutMsg kCmd

                newModel =
                    { m | keyboardShortcut = keyboardShortcut }

                shortcut =
                    KeyboardShortcut.fromKeyboardEvent m.keyboardShortcut event
            in
            case shortcut of
                Sequence _ Escape ->
                    ( Success { newModel | search = { query = "", results = SearchResults.empty } }, cmd )

                Sequence _ ArrowUp ->
                    let
                        newSearch =
                            mapSearch SearchResults.prev m.search
                    in
                    ( Success { newModel | search = newSearch }, cmd )

                Sequence _ ArrowDown ->
                    let
                        newSearch =
                            mapSearch SearchResults.next m.search
                    in
                    ( Success { newModel | search = newSearch }, cmd )

                Sequence _ Enter ->
                    case m.search.results of
                        Empty ->
                            ( Success newModel, cmd )

                        SearchResults matches ->
                            let
                                navigate =
                                    matches
                                        |> SearchResults.focus
                                        |> Tuple.first
                                        |> Route.navigateToProject env.navKey
                            in
                            ( Success newModel, Cmd.batch [ cmd, navigate ] )

                Sequence (Just Semicolon) k ->
                    case Key.toNumber k of
                        Just n ->
                            let
                                navigate =
                                    SearchResults.getAt (n - 1) m.search.results
                                        |> Maybe.map Tuple.first
                                        |> Maybe.map (Route.navigateToProject env.navKey)
                                        |> Maybe.withDefault Cmd.none
                            in
                            ( Success newModel, Cmd.batch [ cmd, navigate ] )

                        Nothing ->
                            ( Success newModel, cmd )

                _ ->
                    ( Success newModel, cmd )

        ( KeyboardShortcutMsg kMsg, Success m ) ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg m.keyboardShortcut
            in
            ( Success { m | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd )

        _ ->
            ( model, Cmd.none )


mapSearch : (CatalogSearchResults -> CatalogSearchResults) -> CatalogSearch -> CatalogSearch
mapSearch f search =
    { search | results = f search.results }



-- VIEW


projectUrl : ProjectListing -> String
projectUrl =
    Route.forProject >> Route.toUrlString


viewProjectListing : ProjectListing -> Html msg
viewProjectListing project =
    Project.viewProjectListing (Click.Href (projectUrl project)) project


viewCategory : ( String, List ProjectListing ) -> Html msg
viewCategory ( category, projects ) =
    let
        projectLinks =
            projects
                |> List.map viewProjectListing
    in
    Card.titled category projectLinks
        |> Card.view


viewMatch : KeyboardShortcut.Model -> SearchResult -> Bool -> Maybe Key -> Html Msg
viewMatch keyboardShortcut ( project, category ) isFocused shortcut =
    let
        shortcutIndicator =
            if isFocused then
                KeyboardShortcut.view keyboardShortcut (Sequence Nothing Key.Enter)

            else
                case shortcut of
                    Nothing ->
                        UI.nothing

                    Just key ->
                        KeyboardShortcut.view keyboardShortcut (Sequence (Just Key.Semicolon) key)
    in
    tr
        [ classList [ ( "search-result", True ), ( "focused", isFocused ) ]
        , onClick (SelectProject project)
        ]
        [ td [ class "project-name" ] [ Project.viewProjectListing Click.Disabled project ]
        , td [ class "category" ] [ text category ]
        , td [] [ div [ class "shortcut" ] [ shortcutIndicator ] ]
        ]


indexToShortcut : Int -> Maybe Key
indexToShortcut index =
    let
        n =
            index + 1
    in
    if n > 9 then
        Nothing

    else
        n |> String.fromInt |> Key.fromString |> Just


viewMatches : KeyboardShortcut.Model -> SearchResults.Matches SearchResult -> Html Msg
viewMatches keyboardShortcut matches =
    let
        matchItems =
            matches
                |> SearchResults.mapMatchesToList (\d f -> ( d, f ))
                |> List.indexedMap (\i ( d, f ) -> ( d, f, indexToShortcut i ))
                |> List.map (\( d, f, s ) -> viewMatch keyboardShortcut d f s)
    in
    table [] [ tbody [] matchItems ]


viewSearchResults : KeyboardShortcut.Model -> CatalogSearch -> Html Msg
viewSearchResults keyboardShortcut { query, results } =
    if String.length query > 2 then
        let
            resultsPane =
                case results of
                    Empty ->
                        div [ class "empty-state" ] [ text ("No matching projects found for \"" ++ query ++ "\"") ]

                    SearchResults matches ->
                        viewMatches keyboardShortcut matches
        in
        div [ class "search-results" ] [ resultsPane ]

    else
        UI.nothing


viewLoaded : LoadedModel -> PageLayout Msg
viewLoaded model =
    let
        categories =
            model.catalog
                |> Catalog.toList
                |> List.map viewCategory

        searchResults =
            if model.hasFocus then
                viewSearchResults model.keyboardShortcut model.search

            else
                UI.nothing

        keyboardEvent =
            KeyboardEvent.on KeyboardEvent.Keydown Keydown
                |> KeyboardEvent.stopPropagation
                |> KeyboardEvent.preventDefaultWhen
                    (\evt -> List.member evt.key [ ArrowUp, ArrowDown, Semicolon ])
                |> KeyboardEvent.attach
    in
    PageLayout.HeroLayout
        { hero =
            PageLayout.PageHero
                (div [ class "catalog-hero" ]
                    [ h1 []
                        [ div []
                            [ strong [ class "explore" ] [ text "Explore" ]
                            , text ", "
                            , strong [ class "discover" ] [ text "Discover" ]
                            , text ", and "
                            , strong [ class "share" ] [ text "Share" ]
                            , text " Unison Code"
                            ]
                        , div [] [ text "Projects, libraries, documention, terms, and types" ]
                        ]
                    , div [ class "catalog-search", keyboardEvent ]
                        [ div [ class "search-field" ]
                            [ Icon.view Icon.search
                            , input
                                [ placeholder "Search for projects"
                                , onInput UpdateQuery
                                , autofocus True
                                , onBlur (UpdateFocus False)
                                , onFocus (UpdateFocus True)
                                ]
                                []
                            ]
                        , searchResults
                        ]
                    ]
                )
        , content = PageLayout.PageContent [ div [ class "categories" ] categories ]
        }


disabledPage : Html Msg -> PageLayout Msg
disabledPage content =
    PageLayout.HeroLayout
        { hero = PageLayout.PageHero UI.nothing
        , content = PageLayout.PageContent [ content ]
        }


view : Model -> Html Msg
view model =
    let
        page =
            case model of
                NotAsked ->
                    disabledPage (div [] [])

                Loading ->
                    disabledPage (div [] [])

                Failure _ ->
                    disabledPage (div [] [])

                Success m ->
                    viewLoaded m
    in
    PageLayout.view page
