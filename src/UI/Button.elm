module UI.Button exposing
    ( Button
    , Size(..)
    , Type(..)
    , button
    , contained
    , danger
    , default
    , icon
    , iconThenLabel
    , large
    , link
    , linkIcon
    , linkIconThenLabel
    , medium
    , primary
    , primaryMono
    , share
    , small
    , uncontained
    , view
    , withSize
    , withType
    )

import Html exposing (Html, a, text)
import Html.Attributes exposing (class, href, rel, target)
import Html.Events exposing (onClick)
import UI.Icon as I


type Action clickMsg
    = OnClick clickMsg
    | Href String


type Content msg
    = Icon (I.Icon msg)
    | IconThenLabel (I.Icon msg) String
    | Label String


type alias Button msg =
    { action : Action msg
    , content : Content msg
    , type_ : Type
    , color : Color
    , size : Size
    }



-- BASE


button : clickMsg -> String -> Button clickMsg
button clickMsg label =
    { action = OnClick clickMsg
    , content = Label label
    , type_ = Contained
    , color = Default
    , size = Medium
    }


icon : msg -> I.Icon msg -> Button msg
icon msg icon_ =
    { action = OnClick msg
    , content = Icon icon_
    , type_ = Contained
    , color = Default
    , size = Medium
    }


iconThenLabel : msg -> I.Icon msg -> String -> Button msg
iconThenLabel msg icon_ label =
    { action = OnClick msg
    , content = IconThenLabel icon_ label
    , type_ = Contained
    , color = Default
    , size = Medium
    }


{-| Generally avoid using Button links as it is not semantically correct, but
useful in some cases for external links
-}
link : String -> String -> Button clickMsg
link url label =
    { action = Href url
    , content = Label label
    , type_ = Uncontained
    , color = Default
    , size = Medium
    }


linkIcon : String -> I.Icon msg -> Button msg
linkIcon url icon_ =
    { action = Href url
    , content = Icon icon_
    , type_ = Uncontained
    , color = Default
    , size = Medium
    }


linkIconThenLabel : String -> I.Icon msg -> String -> Button msg
linkIconThenLabel url icon_ label =
    { action = Href url
    , content = IconThenLabel icon_ label
    , type_ = Uncontained
    , color = Default
    , size = Medium
    }


view : Button clickMsg -> Html clickMsg
view { content, type_, color, action, size } =
    let
        ( contentType, content_ ) =
            case content of
                Icon i ->
                    ( "content-icon", [ I.view i ] )

                IconThenLabel i l ->
                    ( "content-icon-then-label", [ I.view i, text l ] )

                Label l ->
                    ( "content-label", [ text l ] )

        attrs =
            [ class "button"
            , class (typeToClassName type_)
            , class (colorToClassName color)
            , class (sizeToClassName size)
            , class contentType
            ]
    in
    case action of
        OnClick clickMsg ->
            Html.button (onClick clickMsg :: attrs) content_

        Href url ->
            a (attrs ++ [ href url, rel "noopener", target "_blank" ]) content_



-- VARIANTS


type Type
    = Contained
    | Uncontained


type Color
    = Default
    | PrimaryMono
    | Primary
    | Share
    | Danger


contained : Button clickMsg -> Button clickMsg
contained button_ =
    { button_ | type_ = Contained }


uncontained : Button clickMsg -> Button clickMsg
uncontained button_ =
    { button_ | type_ = Uncontained }


withType : Type -> Button clickMsg -> Button clickMsg
withType type_ button_ =
    { button_ | type_ = type_ }


default : Button clickMsg -> Button clickMsg
default =
    withColor Default


primaryMono : Button clickMsg -> Button clickMsg
primaryMono =
    withColor PrimaryMono


primary : Button clickMsg -> Button clickMsg
primary =
    withColor Primary


share : Button clickMsg -> Button clickMsg
share =
    withColor Share


danger : Button clickMsg -> Button clickMsg
danger =
    withColor Danger


withColor : Color -> Button clickMsg -> Button clickMsg
withColor color_ button_ =
    { button_ | color = color_ }



-- SIZES


type Size
    = Small
    | Medium
    | Large


small : Button clickMsg -> Button clickMsg
small =
    withSize Small


medium : Button clickMsg -> Button clickMsg
medium =
    withSize Medium


large : Button clickMsg -> Button clickMsg
large =
    withSize Large


withSize : Size -> Button clickMsg -> Button clickMsg
withSize size button_ =
    { button_ | size = size }



-- INTERNAL


sizeToClassName : Size -> String
sizeToClassName size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


typeToClassName : Type -> String
typeToClassName type_ =
    case type_ of
        Contained ->
            "contained"

        Uncontained ->
            "uncontained"


colorToClassName : Color -> String
colorToClassName color =
    case color of
        Default ->
            "default"

        PrimaryMono ->
            "primary-mono"

        Primary ->
            "primary"

        Share ->
            "share"

        Danger ->
            "danger"
