module UnisonShare.Page.CatalogPage exposing (..)

import Api
import Code.Perspective as Perspective
import Code.Project as Project exposing (ProjectListing)
import Env exposing (Env)
import Html exposing (Html, div, h1, input, strong, table, tbody, td, text, tr)
import Html.Attributes exposing (autofocus, class, classList, placeholder)
import Html.Events exposing (onBlur, onFocus, onInput, onMouseDown)
import Http
import Lib.SearchResults as SearchResults exposing (SearchResults(..))
import List.Extra as ListE
import Maybe.Extra as MaybeE
import RemoteData exposing (RemoteData(..), WebData)
import Simple.Fuzzy as Fuzzy
import Task
import UI
import UI.Card as Card
import UI.Click as Click
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut(..))
import UI.KeyboardShortcut.Key as Key exposing (Key(..))
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import UI.PageLayout as PageLayout exposing (PageLayout)
import UnisonShare.Catalog as Catalog exposing (Catalog)
import UnisonShare.Catalog.CatalogMask exposing (CatalogMask)
import UnisonShare.Route as Route
import UnisonShare.User as User exposing (Username)



-- MODEL


type Match
    = UserMatch Username
    | ProjectMatch ProjectListing String


type alias CatalogSearchResults =
    SearchResults Match



-- TODO: Rename


type alias CatalogSearch =
    { query : String, results : CatalogSearchResults }


type alias LoadedModel =
    { search : CatalogSearch
    , hasFocus : Bool
    , catalog : Catalog
    , usernames : List Username
    , projectListings : List ProjectListing
    , keyboardShortcut : KeyboardShortcut.Model
    }


type alias Model =
    WebData LoadedModel


init : Env -> ( Model, Cmd Msg )
init env =
    ( Loading, fetch env )


{-| Fetch the Catalog in sequence by first fetching the doc, then the
projectListings and finally merging them into a Catalog
-}
fetch : Env -> Cmd Msg
fetch env =
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
        |> Task.attempt FetchFinished



-- UPDATE


type Msg
    = UpdateQuery String
    | UpdateFocus Bool
    | ClearQuery
    | SelectProject ProjectListing
    | SelectUser Username
    | FetchFinished (Result Http.Error ( CatalogMask, List ProjectListing ))
    | Keydown KeyboardEvent
    | KeyboardShortcutMsg KeyboardShortcut.Msg


update : Env -> Msg -> Model -> ( Model, Cmd Msg )
update env msg model =
    case ( msg, model ) of
        ( FetchFinished result, _ ) ->
            case result of
                Err e ->
                    ( Failure e, Cmd.none )

                Ok ( mask, listings ) ->
                    let
                        usernames =
                            listings
                                |> List.map (.owner >> Project.ownerToString)
                                |> ListE.unique
                                |> List.map User.usernameFromString
                                |> MaybeE.values

                        initModel =
                            { search = { query = "", results = SearchResults.empty }
                            , hasFocus = True
                            , catalog = Catalog.catalog mask listings
                            , usernames = usernames
                            , projectListings = listings
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
                            |> search m.catalog m.projectListings m.usernames
                            |> List.take 9
                            |> SearchResults.fromList
            in
            ( Success { m | search = { query = query, results = searchResults } }, Cmd.none )

        ( ClearQuery, Success m ) ->
            ( Success { m | search = { query = "", results = SearchResults.empty } }, Cmd.none )

        ( SelectProject project, Success m ) ->
            ( Success m, Route.navigateToProject env.navKey project )

        ( SelectUser username, Success m ) ->
            ( Success m, Route.navigateToUsername env.navKey username )

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
                                        |> matchToNavigate env
                            in
                            ( Success newModel, Cmd.batch [ cmd, navigate ] )

                Sequence (Just Semicolon) k ->
                    case Key.toNumber k of
                        Just n ->
                            let
                                navigate =
                                    SearchResults.getAt (n - 1) m.search.results
                                        |> Maybe.map (matchToNavigate env)
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
mapSearch f search_ =
    { search_ | results = f search_.results }


matchToNavigate : Env -> Match -> Cmd Msg
matchToNavigate env match =
    case match of
        UserMatch username ->
            Route.navigateToUsername env.navKey username

        ProjectMatch project _ ->
            Route.navigateToProject env.navKey project


toMatches : Catalog -> List ProjectListing -> List Username -> List Match
toMatches catalog projects users =
    let
        catalogProjects =
            catalog |> Catalog.toList |> List.foldl flat []

        flat ( category, projects_ ) acc =
            acc ++ List.map (\p -> ( p, category )) projects_

        categoryOf project =
            catalogProjects
                |> ListE.find (\( p, _ ) -> Project.equals project p)
                |> Maybe.map Tuple.second

        projectMatches =
            projects
                |> List.map
                    (\p ->
                        ProjectMatch p
                            (Maybe.withDefault "" (categoryOf p))
                    )

        userMatches =
            List.map UserMatch users
    in
    userMatches ++ projectMatches


search : Catalog -> List ProjectListing -> List Username -> String -> List Match
search catalog projects users query =
    let
        normalize m =
            case m of
                UserMatch u ->
                    User.usernameToString u

                ProjectMatch p _ ->
                    Project.slugString p
    in
    Fuzzy.filter normalize query (toMatches catalog projects users)



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


{-| View a match in the dropdown list. Use `onMouseDown` instead of `onClick`
to avoid competing with `onBlur` on the input
-}
viewMatch : KeyboardShortcut.Model -> Match -> Bool -> Maybe Key -> Html Msg
viewMatch keyboardShortcut match isFocused shortcut =
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
    case match of
        UserMatch username ->
            tr
                [ classList [ ( "search-result", True ), ( "focused", isFocused ) ]
                , onMouseDown (SelectUser username)
                ]
                [ td [ class "match-name" ]
                    [ div [ class "user-listing" ]
                        [ div [ class "avatar" ] [ Icon.view Icon.user ]
                        , text (User.usernameToString username)
                        ]
                    ]
                , td [ class "category" ] [ text "User" ]
                , td [] [ div [ class "shortcut" ] [ shortcutIndicator ] ]
                ]

        ProjectMatch project category ->
            tr
                [ classList [ ( "search-result", True ), ( "focused", isFocused ) ]
                , onMouseDown (SelectProject project)
                ]
                [ td [ class "match-name" ] [ Project.viewProjectListing Click.Disabled project ]
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


viewMatches : KeyboardShortcut.Model -> SearchResults.Matches Match -> Html Msg
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
                        div [ class "empty-state" ] [ text ("No matches found for \"" ++ query ++ "\"") ]

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
                                [ placeholder "Search for projects and users"
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
