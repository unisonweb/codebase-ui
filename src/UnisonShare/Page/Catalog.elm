module UnisonShare.Page.Catalog exposing (..)

import Api
import Env exposing (Env)
import Html exposing (Html, a, div, h1, input, span, strong, text)
import Html.Attributes exposing (autofocus, class, href, placeholder)
import Html.Events exposing (onBlur, onFocus, onInput)
import Http
import Perspective
import Project exposing (ProjectListing)
import RemoteData exposing (RemoteData(..), WebData)
import Task
import UI
import UI.Card as Card
import UI.Click as Click
import UI.Icon as Icon
import UI.PageLayout as PageLayout exposing (PageLayout)
import UnisonShare.Catalog as Catalog exposing (Catalog)
import UnisonShare.Route as Route



-- MODEL


type alias LoadedModel =
    { query : String
    , hasFocus : Bool
    , catalog : Catalog
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
                Api.projects
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
    | NoOp
    | FetchCatalogFinished (Result Http.Error Catalog)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( FetchCatalogFinished catalogResult, _ ) ->
            case catalogResult of
                Err e ->
                    ( Failure e, Cmd.none )

                Ok catalog ->
                    ( Success { query = "", hasFocus = True, catalog = catalog }, Cmd.none )

        ( UpdateFocus hasFocus, Success m ) ->
            ( Success { m | hasFocus = hasFocus }, Cmd.none )

        ( UpdateQuery query, Success m ) ->
            ( Success { m | query = query }, Cmd.none )

        ( ClearQuery, Success m ) ->
            ( Success { m | query = "" }, Cmd.none )

        _ ->
            ( model, Cmd.none )



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


viewSearchResult : ( ProjectListing, String ) -> Html msg
viewSearchResult ( project, category ) =
    a
        [ class "search-result", href (projectUrl project) ]
        [ Project.viewProjectListing Click.Disabled project
        , span [ class "category" ] [ text category ]
        ]


viewSearchResults : LoadedModel -> Html msg
viewSearchResults model =
    if String.length model.query > 3 then
        let
            results =
                model.query
                    |> Catalog.search model.catalog
                    |> List.map viewSearchResult

            resultsPane =
                if List.isEmpty results then
                    [ div [ class "empty-state" ] [ text ("No matching projects found for \"" ++ model.query ++ "\"") ] ]

                else
                    results
        in
        div [ class "search-results" ] resultsPane

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
                viewSearchResults model

            else
                UI.nothing
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
                    , div [ class "catalog-search" ]
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
