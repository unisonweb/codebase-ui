module Code.CodebaseTree exposing (Model, Msg, OutMsg(..), init, update, view)

import Api exposing (ApiRequest)
import Code.CodebaseTree.NamespaceListing as NamespaceListing
    exposing
        ( DefinitionListing(..)
        , NamespaceListing(..)
        , NamespaceListingChild(..)
        , NamespaceListingContent
        )
import Code.Definition.Category as Category
import Code.Definition.Reference exposing (Reference(..))
import Code.FullyQualifiedName as FQN exposing (FQN, unqualifiedName)
import Code.FullyQualifiedNameSet as FQNSet exposing (FQNSet)
import Code.HashQualified exposing (HashQualified(..))
import Code.Perspective as Perspective exposing (Perspective)
import Env exposing (Env)
import Html exposing (Html, a, div, label, span, text)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)
import Http
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.Icon as Icon exposing (Icon)
import UI.Tooltip as Tooltip



-- MODEL


type alias Model =
    { rootNamespaceListing : WebData NamespaceListing
    , expandedNamespaceListings : FQNSet
    }


init : Env -> ( Model, Cmd Msg )
init env =
    let
        model =
            { rootNamespaceListing = Loading, expandedNamespaceListings = FQNSet.empty }
    in
    ( model, Api.perform env.apiBasePath (fetchRootNamespaceListing env.perspective) )



-- UPDATE


type Msg
    = ToggleExpandedNamespaceListing FQN
    | FetchSubNamespaceListingFinished FQN (Result Http.Error NamespaceListing)
    | FetchRootNamespaceListingFinished (Result Http.Error NamespaceListing)
    | Out OutMsg


type OutMsg
    = None
    | OpenDefinition Reference
    | ChangePerspectiveToNamespace FQN


update : Env -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update env msg model =
    case msg of
        ToggleExpandedNamespaceListing fqn ->
            let
                shouldExpand =
                    not (FQNSet.member fqn model.expandedNamespaceListings)

                setLoading ((NamespaceListing h f _) as namespaceListing) =
                    if FQN.equals f fqn then
                        NamespaceListing h f Loading

                    else
                        namespaceListing

                nextNamespaceListing =
                    if shouldExpand && not namespaceContentFetched then
                        RemoteData.map (NamespaceListing.map setLoading) model.rootNamespaceListing

                    else
                        model.rootNamespaceListing

                namespaceContentFetched =
                    model.rootNamespaceListing
                        |> RemoteData.map (\nl -> NamespaceListing.contentFetched nl fqn)
                        |> RemoteData.withDefault False

                newModel =
                    { model
                        | expandedNamespaceListings = FQNSet.toggle fqn model.expandedNamespaceListings
                        , rootNamespaceListing = nextNamespaceListing
                    }

                cmd =
                    if shouldExpand && not namespaceContentFetched then
                        Api.perform env.apiBasePath (fetchSubNamespaceListing env.perspective fqn)

                    else
                        Cmd.none
            in
            ( newModel, cmd, None )

        FetchSubNamespaceListingFinished fetchedFqn result ->
            let
                replaceNamespaceListing ((NamespaceListing hash fqn _) as namespaceListing) =
                    if FQN.equals fetchedFqn fqn then
                        case result of
                            Ok (NamespaceListing _ _ content) ->
                                NamespaceListing hash fqn content

                            Err err ->
                                NamespaceListing hash fqn (Failure err)

                    else
                        namespaceListing

                nextNamespaceListing =
                    RemoteData.map (NamespaceListing.map replaceNamespaceListing) model.rootNamespaceListing
            in
            ( { model | rootNamespaceListing = nextNamespaceListing }, Cmd.none, None )

        FetchRootNamespaceListingFinished result ->
            case result of
                Ok (NamespaceListing hash fqn content) ->
                    ( { model | rootNamespaceListing = Success (NamespaceListing hash fqn content) }
                    , Cmd.none
                    , None
                    )

                Err err ->
                    ( { model | rootNamespaceListing = Failure err }, Cmd.none, None )

        Out outMsg ->
            case outMsg of
                {- CodebaseTree deals in names relative to the
                   Perspective and perspectives need to be relative to the
                   codebase, so when changing the perspective when a
                   namespace perspective already exists, merge the
                   namespace perspective fqn with the relative name of the
                   namespace being selected.
                -}
                ChangePerspectiveToNamespace name ->
                    let
                        newPerspectiveFqn =
                            case env.perspective of
                                Perspective.Namespace { fqn } ->
                                    FQN.append fqn name

                                Perspective.Codebase _ ->
                                    name
                    in
                    ( model, Cmd.none, ChangePerspectiveToNamespace newPerspectiveFqn )

                _ ->
                    ( model, Cmd.none, outMsg )



-- EFFECTS


fetchRootNamespaceListing : Perspective -> ApiRequest NamespaceListing Msg
fetchRootNamespaceListing perspective =
    fetchNamespaceListing perspective Nothing FetchRootNamespaceListingFinished


fetchSubNamespaceListing : Perspective -> FQN -> ApiRequest NamespaceListing Msg
fetchSubNamespaceListing perspective fqn =
    fetchNamespaceListing perspective (Just fqn) (FetchSubNamespaceListingFinished fqn)


fetchNamespaceListing : Perspective -> Maybe FQN -> (Result Http.Error NamespaceListing -> msg) -> ApiRequest NamespaceListing msg
fetchNamespaceListing perspective fqn toMsg =
    Api.list perspective (Maybe.map FQN.toString fqn)
        |> Api.toRequest (NamespaceListing.decode fqn) toMsg



-- VIEW


viewListingRow : Maybe msg -> String -> String -> Icon msg -> Html msg
viewListingRow clickMsg label_ category icon =
    let
        containerClass =
            class ("node " ++ category)

        container =
            clickMsg
                |> Maybe.map (\msg -> a [ containerClass, onClick msg ])
                |> Maybe.withDefault (span [ containerClass ])
    in
    -- TODO: Temporary work around to avoid the hidden catalog definition to
    -- show up on Share while the catalog page is being worked on
    if label_ == "_catalog" then
        UI.nothing

    else
        container
            [ Icon.view icon
            , viewListingLabel label_
            , span [ class "definition-category" ] [ text category ]
            ]


viewListingLabel : String -> Html msg
viewListingLabel label_ =
    label [ title label_ ] [ text label_ ]


viewDefinitionListing : DefinitionListing -> Html Msg
viewDefinitionListing listing =
    let
        viewDefRow ref fqn =
            viewListingRow (Just (Out (OpenDefinition ref))) (unqualifiedName fqn)
    in
    case listing of
        TypeListing _ fqn category ->
            viewDefRow (TypeReference (NameOnly fqn)) fqn (Category.name category) (Category.icon category)

        TermListing _ fqn category ->
            viewDefRow (TermReference (NameOnly fqn)) fqn (Category.name category) (Category.icon category)

        DataConstructorListing _ fqn ->
            viewDefRow (DataConstructorReference (NameOnly fqn)) fqn "constructor" Icon.dataConstructor

        AbilityConstructorListing _ fqn ->
            viewDefRow (AbilityConstructorReference (NameOnly fqn)) fqn "constructor" Icon.abilityConstructor

        PatchListing _ ->
            viewListingRow Nothing "Patch" "patch" Icon.patch


viewLoadedNamespaceListingContent : FQNSet -> NamespaceListingContent -> Html Msg
viewLoadedNamespaceListingContent expandedNamespaceListings content =
    let
        viewChild c =
            case c of
                SubNamespace nl ->
                    viewNamespaceListing expandedNamespaceListings nl

                SubDefinition dl ->
                    viewDefinitionListing dl
    in
    div [] (List.map viewChild content)


viewNamespaceListingContent : FQNSet -> WebData NamespaceListingContent -> Html Msg
viewNamespaceListingContent expandedNamespaceListings content =
    case content of
        Success loadedContent ->
            viewLoadedNamespaceListingContent expandedNamespaceListings loadedContent

        Failure err ->
            viewError err

        NotAsked ->
            UI.nothing

        Loading ->
            viewLoading


viewNamespaceListing : FQNSet -> NamespaceListing -> Html Msg
viewNamespaceListing expandedNamespaceListings (NamespaceListing _ name content) =
    let
        ( isExpanded, namespaceContent ) =
            if FQNSet.member name expandedNamespaceListings then
                ( True
                , div [ class "namespace-content" ]
                    [ viewNamespaceListingContent
                        expandedNamespaceListings
                        content
                    ]
                )

            else
                ( False, UI.nothing )

        changePerspectiveTo =
            Button.icon (Out (ChangePerspectiveToNamespace name)) Icon.intoFolder
                |> Button.stopPropagation
                |> Button.uncontained
                |> Button.small
                |> Button.view

        fullName =
            FQN.toString name
    in
    div [ class "subtree" ]
        [ a
            [ class "node namespace"
            , onClick (ToggleExpandedNamespaceListing name)
            ]
            [ Icon.caretRight |> Icon.withClassList [ ( "expanded", isExpanded ) ] |> Icon.view
            , viewListingLabel (unqualifiedName name)
            , Tooltip.tooltip changePerspectiveTo (Tooltip.Text ("Change perspective to " ++ fullName))
                |> Tooltip.withArrow Tooltip.End
                |> Tooltip.view
            ]
        , namespaceContent
        ]


viewError : Http.Error -> Html msg
viewError err =
    div [ class "error", title (Util.httpErrorToString err) ]
        [ Icon.view Icon.warn
        , text "Unable to load namespace"
        ]


viewLoading : Html msg
viewLoading =
    div [ class "loading" ]
        [ UI.loadingPlaceholderRow
        , UI.loadingPlaceholderRow
        , UI.loadingPlaceholderRow
        ]


view : Model -> Html Msg
view model =
    let
        listings =
            case model.rootNamespaceListing of
                Success (NamespaceListing _ _ content) ->
                    viewNamespaceListingContent
                        model.expandedNamespaceListings
                        content

                Failure err ->
                    viewError err

                NotAsked ->
                    viewLoading

                Loading ->
                    viewLoading
    in
    div [ class "codebase-tree" ]
        [ div [ class "namespace-tree" ] [ listings ]
        ]
