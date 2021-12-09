module UnisonShare.Page.Catalog exposing (..)

import Api
import Dict exposing (Dict)
import Env exposing (Env)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Project exposing (ProjectListing)
import RemoteData exposing (RemoteData(..), WebData)
import UI



-- MODEL


type Category
    = Category String


type Catalog
    = Catalog (Dict Category ProjectListing)


type alias LoadedModel =
    { query : String
    , catalog : Catalog
    }


type alias Model =
    WebData LoadedModel


init : Env -> ( Model, Cmd Msg )
init env =
    let
        fetchCmd =
            Api.projects
                |> Api.toRequest Project.decodeList FetchProjectsFinished
                |> Api.perform env.apiBasePath
    in
    ( Loading, fetchCmd )



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
    Catalog Dict.empty



-- VIEW


viewLoaded : LoadedModel -> Html Msg
viewLoaded _ =
    div [] [ text "Catalog" ]


view : Model -> Html Msg
view model =
    case model of
        NotAsked ->
            UI.nothing

        Loading ->
            UI.nothing

        Failure _ ->
            div [ class "" ] [ text "Error..." ]

        Success m ->
            viewLoaded m
