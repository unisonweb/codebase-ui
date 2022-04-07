module UnisonShare.Page.UserPage exposing (..)

import Code.Definition.Doc as Doc
import Code.Definition.Readme as Readme
import Code.Definition.Reference exposing (Reference)
import Code.FullyQualifiedName as FQN
import Code.Perspective as Perspective
import Code.Project as Project exposing (ProjectListing)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import Task
import UI
import UI.Card as Card
import UI.Click as Click
import UI.PageLayout as PageLayout exposing (PageLayout)
import UnisonShare.Api as ShareApi
import UnisonShare.Env exposing (Env)
import UnisonShare.Route as Route
import UnisonShare.User as User exposing (UserDetails, Username)



-- MODEL


type alias LoadedModel =
    { user : UserDetails
    , docFoldToggles : Doc.DocFoldToggles
    , projects : List ProjectListing
    }


type alias Model =
    WebData LoadedModel


init : Env -> Username -> ( Model, Cmd Msg )
init env username =
    ( Loading, fetchUser env username )


{-| Fetch the Catalog in sequence by first fetching the doc, then the
projectListings and finally merging them into a Catalog
-}
fetchUser : Env -> Username -> Cmd Msg
fetchUser env username =
    let
        perspective =
            Perspective.toRootPerspective env.perspective

        usernameFqn =
            username |> User.usernameToString |> FQN.fromString
    in
    ShareApi.namespace perspective usernameFqn
        |> HttpApi.toTask env.apiBasePath User.decodeDetails
        |> Task.andThen
            (\userDetails ->
                ShareApi.projects (username |> User.usernameToString |> Just)
                    |> HttpApi.toTask env.apiBasePath Project.decodeListings
                    |> Task.map (\projects -> ( userDetails, projects ))
            )
        |> Task.attempt FetchUserProfileFinished



-- UPDATE


type Msg
    = OpenReference Reference
    | ToggleDocFold Doc.FoldId
    | FetchUserProfileFinished (Result Http.Error ( UserDetails, List ProjectListing ))


update : Env -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case ( model, msg ) of
        ( _, FetchUserProfileFinished res ) ->
            case res of
                Err e ->
                    ( Failure e, Cmd.none )

                Ok ( userDetails, projects ) ->
                    let
                        m =
                            { docFoldToggles = Doc.emptyDocFoldToggles
                            , user = userDetails
                            , projects = projects
                            }
                    in
                    ( Success m, Cmd.none )

        ( Success _, OpenReference _ ) ->
            ( model, Cmd.none )

        ( Success m, ToggleDocFold fid ) ->
            ( Success { m | docFoldToggles = Doc.toggleFold m.docFoldToggles fid }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


projectUrl : ProjectListing -> String
projectUrl =
    Route.forProject >> Route.toUrlString


viewProjectListing : ProjectListing -> Html msg
viewProjectListing project =
    Project.viewProjectListing (Click.Href (projectUrl project)) project


viewLoadedModel : LoadedModel -> PageLayout Msg
viewLoadedModel { user, projects, docFoldToggles } =
    let
        readmeCard =
            user.readme
                |> Maybe.map (Readme.view OpenReference ToggleDocFold docFoldToggles)
                |> Maybe.map (\r -> Card.titled "readme" [ r ])
                |> Maybe.map Card.asContained
                |> Maybe.map Card.view
                |> Maybe.withDefault UI.nothing

        projectsCard =
            projects
                |> List.map viewProjectListing
                |> (\ps -> [ div [ class "projects" ] ps ])
                |> Card.titled "projects"
                |> Card.view
    in
    PageLayout.HeroLayout
        { hero =
            PageLayout.PageHero
                (div [ class "user-hero" ] [ h1 [] [ text (User.usernameToString user.username) ] ])
        , content = PageLayout.PageContent [ readmeCard, projectsCard ]
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
                    viewLoadedModel m
    in
    PageLayout.view page
