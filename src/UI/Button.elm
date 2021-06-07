module UI.Button exposing
    ( Button
    , Size(..)
    , Type(..)
    , button
    , danger
    , default
    , large
    , medium
    , primary
    , primaryMono
    , share
    , small
    , view
    , withSize
    , withType
    )

import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Button clickMsg =
    { clickMsg : clickMsg
    , label : String
    , type_ : Type
    , size : Size
    }



-- BASE


button : clickMsg -> String -> Button clickMsg
button clickMsg label =
    { clickMsg = clickMsg
    , label = label
    , type_ = Default
    , size = Medium
    }


view : Button clickMsg -> Html clickMsg
view { label, type_, clickMsg, size } =
    Html.button
        [ class (typeToClassName type_)
        , class (sizeToClassName size)
        , onClick clickMsg
        ]
        [ text label ]



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
            "secondary"


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
