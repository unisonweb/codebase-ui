module UI.PageLayout exposing (..)

import Html exposing (Html, div, header, section)
import Html.Attributes exposing (class, classList)
import UI.AppHeader as AppHeader exposing (AppHeader)
import UI.Sidebar as Sidebar


type PageHero msg
    = PageHero (Html msg)


type PageContent msg
    = PageContent (List (Html msg))


type PageLayout msg
    = HeroLayout
        { header : AppHeader msg
        , hero : PageHero msg
        , content :
            PageContent msg
        }
    | SidebarLayout
        { header : AppHeader msg
        , sidebar : List (Html msg)
        , sidebarToggled : Bool
        , content : PageContent msg
        }
    | FullLayout { header : AppHeader msg, content : PageContent msg }



-- VIEW


viewHero : PageHero msg -> Html msg
viewHero (PageHero content) =
    header [ class "page-hero" ] [ content ]


viewContent : PageContent msg -> Html msg
viewContent (PageContent content) =
    section [ class "page-content" ] content


view : PageLayout msg -> Html msg
view page =
    case page of
        HeroLayout { header, hero, content } ->
            div [ class "page hero-layout" ]
                [ AppHeader.view header
                , viewHero hero
                , viewContent content
                ]

        SidebarLayout { header, sidebar, sidebarToggled, content } ->
            div
                [ class "page sidebar-layout"
                , classList [ ( "sidebar-toggled", sidebarToggled ) ]
                ]
                [ AppHeader.view header
                , Sidebar.view sidebar
                , viewContent content
                ]

        FullLayout { header, content } ->
            div [ class "page full-layout" ]
                [ AppHeader.view header
                , viewContent content
                ]
