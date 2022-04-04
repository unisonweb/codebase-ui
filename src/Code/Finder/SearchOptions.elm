module Code.Finder.SearchOptions exposing (..)

import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Perspective as Perspective exposing (Perspective)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import UI


type WithinOption
    = AllNamespaces
      -- WithinNamespacePerspective has an FQN to make it more convenient
      -- (by avoiding having to pass the Perspective through the view layer)
      -- It is meant to be the same FQN that is in Perspective.Namespace.
    | WithinNamespacePerspective FQN
    | WithinNamespace FQN


type SearchOptions
    = SearchOptions WithinOption


init : Perspective -> Maybe FQN -> SearchOptions
init perspective mfqn =
    case ( perspective, mfqn ) of
        ( Perspective.Namespace { fqn }, Nothing ) ->
            SearchOptions (WithinNamespacePerspective fqn)

        ( _, Just fqn ) ->
            SearchOptions (WithinNamespace fqn)

        _ ->
            SearchOptions AllNamespaces


{-| Removing WithinNamespace when the Perspective is a Namespace perspective
pops back to the WithinNamespacePerspective option (which in turn can also be
removed)
-}
removeWithin : Perspective -> SearchOptions -> SearchOptions
removeWithin perspective (SearchOptions within) =
    let
        nextWithin =
            case ( within, perspective ) of
                ( WithinNamespace _, Perspective.Namespace { fqn } ) ->
                    WithinNamespacePerspective fqn

                _ ->
                    AllNamespaces
    in
    SearchOptions nextWithin


view : msg -> SearchOptions -> Html msg
view removeWithinMsg (SearchOptions within) =
    let
        viewWithin fqn =
            div [ class "finder-search-options" ]
                [ UI.optionBadge
                    removeWithinMsg
                    (span [] [ UI.subtle "within ", text (FQN.toString fqn) ])
                ]
    in
    case within of
        AllNamespaces ->
            UI.nothing

        WithinNamespacePerspective fqn ->
            viewWithin fqn

        WithinNamespace fqn ->
            viewWithin fqn
