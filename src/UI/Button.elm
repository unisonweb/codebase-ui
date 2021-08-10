module UI.Button exposing
    ( Button
    , Size(..)
    , Type(..)
    , button
    , buttonIcon
    , buttonIconThenLabel
    , danger
    , default
    , large
    , link
    , linkIcon
    , linkIconThenLabel
    , medium
    , primary
    , primaryMono
    , share
    , small
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
    , size : Size
    }



-- BASE


button : clickMsg -> String -> Button clickMsg
button clickMsg label =
    { action = OnClick clickMsg
    , content = Label label
    , type_ = Default
    , size = Medium
    }


buttonIcon : msg -> I.Icon msg -> Button msg
buttonIcon msg icon_ =
    { action = OnClick msg
    , content = Icon icon_
    , type_ = Default
    , size = Medium
    }


buttonIconThenLabel : msg -> I.Icon msg -> String -> Button msg
buttonIconThenLabel msg icon_ label =
    { action = OnClick msg
    , content = IconThenLabel icon_ label
    , type_ = Default
    , size = Medium
    }


{-| Generally avoid using Button links as it is not semantically correct, but
useful in some cases for external links
-}
link : String -> String -> Button clickMsg
link url label =
    { action = Href url
    , content = Label label
    , type_ = Default
    , size = Medium
    }


linkIcon : String -> I.Icon msg -> Button msg
linkIcon url icon_ =
    { action = Href url
    , content = Icon icon_
    , type_ = Default
    , size = Medium
    }


linkIconThenLabel : String -> I.Icon msg -> String -> Button msg
linkIconThenLabel url icon_ label =
    { action = Href url
    , content = IconThenLabel icon_ label
    , type_ = Default
    , size = Medium
    }


view : Button clickMsg -> Html clickMsg
view { content, type_, action, size } =
    let
        ( contentType, content_ ) =
            case content of
                Icon i ->
                    ( "content-icon", [ I.view i ] )

                IconThenLabel i l ->
                    ( "content-icon-then-label", [ I.view i, text l ] )

                Label l ->
                    ( "content-label", [ text l ] )
    in
    case action of
        OnClick clickMsg ->
            Html.button
                [ class "button"
                , class (typeToClassName type_)
                , class (sizeToClassName size)
                , class contentType
                , onClick clickMsg
                ]
                content_

        Href url ->
            a
                [ class "button"
                , class (typeToClassName type_)
                , class (sizeToClassName size)
                , class contentType
                , href url
                , rel "noopener"
                , target "_blank"
                ]
                content_



-- VARIANTS


type Type
    = Default
    | PrimaryMono
    | Primary
    | Share
    | Danger


default : Button clickMsg -> Button clickMsg
default =
    withType Default


primaryMono : Button clickMsg -> Button clickMsg
primaryMono =
    withType PrimaryMono


primary : Button clickMsg -> Button clickMsg
primary =
    withType Primary


share : Button clickMsg -> Button clickMsg
share =
    withType Share


danger : Button clickMsg -> Button clickMsg
danger =
    withType Danger


withType : Type -> Button clickMsg -> Button clickMsg
withType type_ button_ =
    { button_ | type_ = type_ }



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
