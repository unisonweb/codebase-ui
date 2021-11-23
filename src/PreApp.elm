module PreApp exposing (..)

import Api exposing (ApiBasePath(..), ApiRequest)
import App
import Browser
import Browser.Navigation as Nav
import Env exposing (Flags)
import Html
import Http
import Perspective exposing (Perspective, PerspectiveParams)
import Route exposing (Route)
import Url exposing (Url)


type Model
    = Initializing PreEnv
    | InitializationError PreEnv Http.Error
    | Initialized App.Model


type alias PreEnv =
    { flags : Flags
    , route : Route
    , navKey : Nav.Key
    , perspectiveParams : PerspectiveParams
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        route =
            Route.fromUrl flags.basePath url

        preEnv =
            { flags = flags
            , route = route
            , navKey = navKey
            , perspectiveParams = Route.perspectiveParams route
            }

        perspectiveToAppInit perspective =
            let
                env =
                    Env.init preEnv.flags perspective

                ( app, cmd ) =
                    App.init env preEnv.route preEnv.navKey
            in
            ( Initialized app, Cmd.map AppMsg cmd )

        fetchPerspective_ =
            ( Initializing preEnv, Api.perform (ApiBasePath flags.apiBasePath) (fetchPerspective preEnv) )
    in
    -- If we have a codebase hash we can construct a full perspective,
    -- otherwise we have to fetch the hash before being able to start up the
    -- app
    preEnv.perspectiveParams
        |> Perspective.fromParams
        |> Maybe.map perspectiveToAppInit
        |> Maybe.withDefault fetchPerspective_


fetchPerspective : PreEnv -> ApiRequest Perspective Msg
fetchPerspective preEnv =
    Api.codebaseHash |> Api.toRequest (Perspective.decode preEnv.perspectiveParams) (FetchPerspectiveFinished preEnv)


type Msg
    = FetchPerspectiveFinished PreEnv (Result Http.Error Perspective)
    | AppMsg App.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPerspectiveFinished preEnv result ->
            case result of
                Ok perspective ->
                    let
                        env =
                            Env.init preEnv.flags perspective

                        newRoute =
                            perspective
                                |> Perspective.toParams
                                |> Route.updatePerspectiveParams preEnv.route

                        ( app, cmd ) =
                            App.init env newRoute preEnv.navKey
                    in
                    ( Initialized app, Cmd.map AppMsg cmd )

                Err err ->
                    ( InitializationError preEnv err, Cmd.none )

        AppMsg appMsg ->
            case model of
                Initialized a ->
                    let
                        ( app, cmd ) =
                            App.update appMsg a
                    in
                    ( Initialized app, Cmd.map AppMsg cmd )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialized app ->
            Sub.map AppMsg (App.subscriptions app)

        _ ->
            Sub.none


view : Model -> Browser.Document Msg
view model =
    let
        appContext flags =
            Env.appContextFromString flags.appContext
    in
    case model of
        Initializing preEnv ->
            { title = "Loading.."
            , body = [ App.viewAppLoading (appContext preEnv.flags) ]
            }

        InitializationError preEnv error ->
            { title = "Application Error"
            , body = [ App.viewAppError (appContext preEnv.flags) error ]
            }

        Initialized appModel ->
            let
                app =
                    App.view appModel
            in
            { title = app.title
            , body = List.map (Html.map AppMsg) app.body
            }
