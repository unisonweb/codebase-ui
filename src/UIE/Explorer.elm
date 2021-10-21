module UIE.Explorer exposing (main)

import UIE.ButtonStories
import UIE.ModalStories
import UIE.TooltipStories
import UIExplorer
    exposing
        ( UIExplorerProgram
        , category
        , createCategories
        , defaultConfig
        , exploreWithCategories
        )


main : UIExplorerProgram {} () {}
main =
    exploreWithCategories
        defaultConfig
        (createCategories
            |> category "UI"
                [ UIE.ButtonStories.stories
                , UIE.TooltipStories.stories
                , UIE.ModalStories.stories
                ]
        )
