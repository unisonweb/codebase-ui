module UIE.ModalStories exposing (..)

import Html
import UI.Modal as Modal
import UIExplorer exposing (UI, storiesOf)


stories : UI {} () {}
stories =
    storiesOf
        "Modal"
        [ ( "Default"
          , \_ ->
                Modal.modal "id" () (Modal.Content (Html.text "Modal content."))
                    |> Modal.view
          , {}
          )
        ]
