module Definition.Readme exposing (..)

import Definition.Doc as Doc exposing (Doc, DocFoldToggles, FoldId)
import Definition.Reference exposing (Reference)
import Html exposing (Html, div, header, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import UI.Icon as Icon


{-| Represent the Readme Doc definition of a namespace. This is typically
rendered slightly different than other docs when viewed from a Namespace
landing page point of view.
-}
type Readme
    = Readme Doc



-- VIEW


view :
    (Reference -> msg)
    -> (FoldId -> msg)
    -> DocFoldToggles
    -> Readme
    -> Html msg
view refToMsg toggleFoldMsg docFoldToggles (Readme doc) =
    div [ class "readme" ]
        [ header [] [ Icon.view Icon.doc, text "README" ]
        , Doc.view refToMsg toggleFoldMsg docFoldToggles doc
        ]



-- DECODE


decode : Decode.Decoder Readme
decode =
    Decode.map Readme Doc.decode
