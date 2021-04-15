module Definition.Info exposing (..)

import FullyQualifiedName as FQN exposing (FQN)
import List.Extra as ListE
import List.Nonempty as NEL



-- TODO: Without `otherNames` and if the `FQN` was added,
-- `Info` would be translatable to
-- `TermSummary`/`TypeSummary`.
-- Perhaps it should be `Naming` instead?
-- `otherNames` is a detail thing only.


type alias Info =
    { name : String
    , namespace : Maybe String
    , otherNames : List FQN
    }


makeInfo : String -> NEL.Nonempty FQN -> Info
makeInfo name_ allFqns =
    let
        ( namespace, otherNames ) =
            namespaceAndOtherNames name_ allFqns
    in
    Info name_ namespace otherNames



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
