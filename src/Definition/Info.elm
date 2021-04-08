module Definition.Info exposing (..)

import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import List.Extra as ListE
import List.Nonempty as NEL


type alias Info =
    { hash : Hash
    , name : String
    , namespace : Maybe String
    , otherNames : List FQN
    }


makeInfo :
    Hash
    -> String
    -> NEL.Nonempty FQN
    -> Info
makeInfo hash_ name_ allFqns =
    let
        ( namespace, otherNames ) =
            namespaceAndOtherNames name_ allFqns
    in
    Info hash_ name_ namespace otherNames



-- Helpers


namespaceAndOtherNames : String -> NEL.Nonempty FQN -> ( Maybe String, List FQN )
namespaceAndOtherNames suffixName fqns =
    let
        fqnWithin =
            fqns
                |> NEL.filter (FQN.isSuffixOf suffixName) (NEL.head fqns)
                |> NEL.head

        fqnsWithout =
            fqns
                |> NEL.toList
                |> ListE.filterNot (FQN.equals fqnWithin)
    in
    ( FQN.namespaceOf suffixName fqnWithin, fqnsWithout )
