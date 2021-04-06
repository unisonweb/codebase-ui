module Definition.Info exposing (..)

import FullyQualifiedName exposing (FQN)
import Hash exposing (Hash)


type alias Info =
    { hash : Hash
    , namespace : Maybe String
    , name : String
    , otherNames : List FQN
    }
