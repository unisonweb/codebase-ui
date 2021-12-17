module ProjectTests exposing (..)

import Expect
import FullyQualifiedName as FQN
import Hash
import Project
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
