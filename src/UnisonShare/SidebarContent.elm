module UnisonShare.SidebarContent exposing (..)

import FullyQualifiedName as FQN exposing (FQN)
import Html exposing (Html)
import UI.Sidebar as Sidebar


view : (FQN -> msg) -> Html msg
view changePerspectiveMsg =
    let
        base =
            FQN.fromString "unison.base"
    in
    Sidebar.section "Popular libraries"
        [ Sidebar.item (changePerspectiveMsg base) (FQN.toString base)
        ]
