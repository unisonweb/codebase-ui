module UnisonShare.Page.Catalog exposing (..)

-- import Api

import Env exposing (Env)
import FullyQualifiedName as FQN
import Html exposing (Html, a, div, h1, input, strong, text)
import Html.Attributes exposing (class, href, placeholder)
import Http
import Project exposing (ProjectListing)
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import UI
import UI.AppHeader exposing (AppHeader)
import UI.Card as Card
import UI.Icon as Icon
import UI.PageLayout as PageLayout exposing (PageLayout)
import UnisonShare.Route as Route



-- MODEL


type Category
    = Category String (List ProjectListing)


type Catalog
    = Catalog (Set Category)


type alias LoadedModel =
    { query : String
    , catalog : Catalog
    }


type alias Model =
    WebData LoadedModel


init : Env -> ( Model, Cmd Msg )
init _ =
    {-
       let
           fetchCmd =
               Api.projects
                   |> Api.toRequest Project.decodeList FetchProjectsFinished
                   |> Api.perform env.apiBasePath
       in
       ( Loading, fetchCmd )
    -}
    let
        categories =
            Set.empty

        model =
            Success
                { catalog = Catalog categories
                , query = ""
                }
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = UpdateQuery String
    | ClearQuery
    | FetchProjectsFinished (Result Http.Error (List ProjectListing))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( FetchProjectsFinished projectsResult, _ ) ->
            case projectsResult of
                Err e ->
                    ( Failure e, Cmd.none )

                Ok projects ->
                    let
                        catalog =
                            projectsToCatalog projects
                    in
                    ( Success { query = "", catalog = catalog }, Cmd.none )

        ( UpdateQuery query, Success m ) ->
            ( Success { m | query = query }, Cmd.none )

        ( ClearQuery, Success m ) ->
            ( Success { m | query = "" }, Cmd.none )

        _ ->
            ( model, Cmd.none )


projectsToCatalog : List ProjectListing -> Catalog
projectsToCatalog _ =
    Catalog Set.empty



-- VIEW


viewProjectListing : ProjectListing -> Html msg
viewProjectListing project =
    let
        slug =
            FQN.cons (Project.ownerToString project.owner) project.name

        url =
            project
                |> Route.forProject
                |> Route.toUrlString
    in
    a [ class "project-listing", href url ] [ FQN.view slug ]


viewCategory : Category -> Html msg
viewCategory (Category category projects) =
    let
        projectLinks =
            projects
                |> List.map viewProjectListing
    in
    Card.titled category projectLinks
        |> Card.view


viewLoaded : AppHeader msg -> LoadedModel -> PageLayout msg
viewLoaded appHeader _ =
    let
        content =
            [ div [ class "categories" ]
                [ viewCategory
                    (Category "Featured"
                        [ { owner = Project.Owner "unison", name = FQN.fromString "base" }
                        , { owner = Project.Owner "unison", name = FQN.fromString "distributed" }
                        ]
                    )
                , viewCategory
                    (Category "Parsers & Text Manipulation"
                        [ { owner = Project.Owner "rlmark", name = FQN.fromString "parsing" }
                        , { owner = Project.Owner "stew", name = FQN.fromString "json" }
                        ]
                    )
                , viewCategory (Category "Databases" [])
                , viewCategory (Category "Datatypes" [])
                , viewCategory (Category "Math" [])
                , viewCategory (Category "Instrumentation" [])
                , viewCategory (Category "Multimedia" [])
                , viewCategory
                    (Category "Networking"
                        [ { owner = Project.Owner "unison", name = FQN.fromString "http" }
                        ]
                    )
                , viewCategory (Category "Utilities" [])
                ]
            ]
    in
    PageLayout.HeroLayout
        { header = appHeader
        , hero =
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
                    , div [ class "catalog-search" ] [ Icon.view Icon.search, input [ placeholder "Search for projects" ] [] ]
                    ]
                )
        , content = PageLayout.PageContent content
        }


disabledPage : AppHeader msg -> Html msg -> PageLayout msg
disabledPage appHeader content =
    PageLayout.HeroLayout
        { header = appHeader
        , hero = PageLayout.PageHero UI.nothing
        , content = PageLayout.PageContent [ content ]
        }


view : AppHeader msg -> Model -> Html msg
view appHeader model =
    let
        page =
            case model of
                NotAsked ->
                    disabledPage appHeader (div [] [])

                Loading ->
                    disabledPage appHeader (div [] [])

                Failure _ ->
                    disabledPage appHeader (div [] [])

                Success m ->
                    viewLoaded appHeader m
    in
    PageLayout.view page
