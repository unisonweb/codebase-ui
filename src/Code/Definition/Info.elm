module Code.Definition.Info exposing (..)

import Code.Definition.Reference as Reference exposing (Reference)
import Code.FullyQualifiedName as FQN exposing (FQN)
import List.Extra as ListE
import List.Nonempty as NEL



-- TODO: Without `otherNames` and if the `FQN` was added,
-- `Info` would be translatable to
-- `TermSummary`/`TypeSummary`.
-- Perhaps it should be `Naming` instead?
-- `otherNames` is a detail thing only.


type alias Info =
    { name : FQN
    , namespace : Maybe FQN
    , otherNames : List FQN
    }


makeInfo : Reference -> FQN -> NEL.Nonempty FQN -> Info
makeInfo requestedRef suffixName allFqns =
    let
        ( namespace, otherNames ) =
            namespaceAndOtherNames requestedRef suffixName allFqns
    in
    Info suffixName namespace otherNames



-- Helpers


namespaceAndOtherNames : Reference -> FQN -> NEL.Nonempty FQN -> ( Maybe FQN, List FQN )
namespaceAndOtherNames requestedRef suffixName fqns =
    let
        shortest =
            NEL.sortBy FQN.numSegments >> NEL.head

        shortestSuffixMatching =
            let
                defaultFqn =
                    shortest fqns
            in
            fqns
                |> NEL.filter (FQN.isSuffixOf suffixName) defaultFqn
                |> shortest

        fqnWithin =
            case Reference.fqn requestedRef of
                Just requestedName ->
                    if FQN.isSuffixOf suffixName requestedName then
                        requestedName

                    else
                        shortestSuffixMatching

                Nothing ->
                    shortestSuffixMatching

        fqnsWithout =
            fqns
                |> NEL.toList
                |> ListE.filterNot (FQN.equals fqnWithin)
    in
    ( FQN.namespace fqnWithin, fqnsWithout )
