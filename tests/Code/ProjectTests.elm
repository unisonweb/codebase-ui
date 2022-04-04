module Code.ProjectTests exposing (..)

import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Code.Project as Project
import Expect
import Test exposing (..)


slug : Test
slug =
    describe "Project.slug"
        [ test "Returns the slug of a project by owner and name" <|
            \_ ->
                Expect.equal
                    "unison.http"
                    (Project.slug project |> FQN.toString)
        ]



-- Helpers


project : Project.ProjectListing
project =
    { owner = Project.Owner "unison"
    , name = FQN.fromString "http"
    , hash = Hash.unsafeFromString "##unison.http"
    }
