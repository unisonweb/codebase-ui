module UIE.TooltipStories exposing (..)

import Html
import Html.Attributes exposing (style)
import UI.Tooltip as Tooltip
import UIExplorer exposing (UI, storiesOf)


stories : UI {} () {}
stories =
    storiesOf
        "Tooltip"
        (List.map
            (\( name, pos ) ->
                ( name
                , \_ ->
                    Html.div [ style "width" "600px" ]
                        [ Html.span [] [ Html.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi gravida ipsum ac tellus gravida eleifend. (" ]
                        , Tooltip.tooltip
                            (Html.span [ style "color" "red" ] [ Html.text "hover here" ])
                            (Tooltip.Text "Tooltip")
                            |> Tooltip.withPosition pos
                            |> Tooltip.withArrow Tooltip.Start
                            |> Tooltip.view
                        , Html.span [] [ Html.text ")In hac habitasse platea dictumst. Nunc rutrum auctor nibh, sit amet ornare erat. " ]
                        ]
                , {}
                )
            )
            [ ( "Above", Tooltip.Above ), ( "Below", Tooltip.Below ), ( "LeftOf", Tooltip.LeftOf ), ( "RightOf", Tooltip.RightOf ) ]
        )
