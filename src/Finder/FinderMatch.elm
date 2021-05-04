module Finder.FinderMatch exposing (..)

import Definition.AbilityConstructor as AbilityConstructor exposing (AbilityConstructor(..), AbilityConstructorSummary)
import Definition.DataConstructor as DataConstructor exposing (DataConstructor(..), DataConstructorSummary)
import Definition.Reference exposing (Reference(..))
import Definition.Term as Term exposing (Term(..), TermSummary)
import Definition.Type as Type exposing (Type(..), TypeSummary)
import FullyQualifiedName as FQN
import Hash
import HashQualified exposing (HashQualified(..))
import Json.Decode as Decode exposing (at, field, string)
import Json.Decode.Extra exposing (when)
import List.Nonempty as NEL
import Util exposing (decodeNonEmptyList, decodeTag)


type FinderItem
    = TermItem TermSummary
    | TypeItem TypeSummary
    | DataConstructorItem DataConstructorSummary
    | AbilityConstructorItem AbilityConstructorSummary


type MatchSegment
    = Gap String
    | Match String


type alias MatchSegments =
    NEL.Nonempty MatchSegment


type alias MatchPositions =
    NEL.Nonempty Int


type alias FinderMatch =
    { score : Int
    , matchSegments : MatchSegments
    , matchPositions : MatchPositions
    , item : FinderItem
    }



-- CREATE


finderMatch : Int -> MatchSegments -> FinderItem -> FinderMatch
finderMatch score matchSegments item =
    let
        matchPositions =
            matchSegmentsToMatchPositions matchSegments
    in
    FinderMatch score matchSegments matchPositions item



-- HELPERS


name : FinderMatch -> String
name fm =
    case fm.item of
        TypeItem (Type _ _ summary) ->
            summary.name

        TermItem (Term _ _ summary) ->
            summary.name

        DataConstructorItem (DataConstructor _ summary) ->
            summary.name

        AbilityConstructorItem (AbilityConstructor _ summary) ->
            summary.name


namespace : FinderMatch -> Maybe String
namespace fm =
    case fm.item of
        TypeItem (Type _ _ summary) ->
            summary.namespace

        TermItem (Term _ _ summary) ->
            summary.namespace

        DataConstructorItem (DataConstructor _ summary) ->
            summary.namespace

        AbilityConstructorItem (AbilityConstructor _ summary) ->
            summary.namespace


reference : FinderMatch -> Reference
reference fm =
    case fm.item of
        TypeItem (Type h _ _) ->
            TypeReference (HashOnly h)

        TermItem (Term h _ _) ->
            TermReference (HashOnly h)

        DataConstructorItem (DataConstructor h _) ->
            DataConstructorReference (HashOnly h)

        AbilityConstructorItem (AbilityConstructor h _) ->
            AbilityConstructorReference (HashOnly h)


matchSegmentsToMatchPositions : MatchSegments -> NEL.Nonempty Int
matchSegmentsToMatchPositions segments =
    let
        f s ( cur, is ) =
            case s of
                Gap str ->
                    ( cur + String.length str, is )

                Match str ->
                    let
                        matches =
                            str
                                |> String.toList
                                |> List.indexedMap (\i _ -> i + cur)
                    in
                    ( cur + String.length str, is ++ matches )

        ( _, positions ) =
            NEL.foldl f ( 0, [] ) segments
    in
    -- This `NEL.fromElement 0` is a random value to satisfy the Maybe. It
    -- is literally impossible (since there will always be a position for
    -- segments) and thats prolly a good indication that I'm not doing this
    -- right...
    Maybe.withDefault (NEL.fromElement 0) (NEL.fromList positions)



-- JSON DECODERS


decodeScore : Decode.Decoder Int
decodeScore =
    field "score" Decode.int


decodeTypeItem : Decode.Decoder FinderItem
decodeTypeItem =
    let
        makeSummary fqn name_ source =
            { fqn = fqn
            , name = name_
            , namespace = FQN.namespaceOf name_ fqn
            , source = source
            }
    in
    Decode.map TypeItem
        (Decode.map3 Type
            (field "namedType" Type.decodeHash)
            (field "namedType" (Type.decodeTypeCategory "typeTag"))
            (Decode.map3 makeSummary
                (field "namedType" Type.decodeFQN)
                (field "bestFoundTypeName" string)
                (field "typeDef" Type.decodeSource)
            )
        )


decodeTermItem : Decode.Decoder FinderItem
decodeTermItem =
    let
        makeSummary fqn name_ signature =
            { fqn = fqn
            , name = name_
            , namespace = FQN.namespaceOf name_ fqn
            , signature = signature
            }
    in
    Decode.map TermItem
        (Decode.map3 Term
            (field "namedTerm" Term.decodeHash)
            (field "namedTerm" (Term.decodeTermCategory "termTag"))
            (Decode.map3 makeSummary
                (field "namedTerm" Term.decodeFQN)
                (field "bestFoundTermName" string)
                (field "namedTerm" Term.decodeSignature)
            )
        )


decodeAbilityConstructorItem : Decode.Decoder FinderItem
decodeAbilityConstructorItem =
    let
        makeSummary fqn name_ signature =
            { fqn = fqn
            , name = name_
            , namespace = FQN.namespaceOf name_ fqn
            , signature = signature
            }
    in
    Decode.map AbilityConstructorItem
        (Decode.map2 AbilityConstructor
            (at [ "namedTerm", "termHash" ] Hash.decode)
            (Decode.map3 makeSummary
                (at [ "namedTerm", "termName" ] FQN.decode)
                (field "bestFoundTermName" string)
                (field "namedTerm" AbilityConstructor.decodeSignature)
            )
        )


decodeDataConstructorItem : Decode.Decoder FinderItem
decodeDataConstructorItem =
    let
        makeSummary fqn name_ signature =
            { fqn = fqn
            , name = name_
            , namespace = FQN.namespaceOf name_ fqn
            , signature = signature
            }
    in
    Decode.map DataConstructorItem
        (Decode.map2 DataConstructor
            (at [ "namedTerm", "termHash" ] Hash.decode)
            (Decode.map3 makeSummary
                (at [ "namedTerm", "termName" ] FQN.decode)
                (field "bestFoundTermName" string)
                (field "namedTerm" DataConstructor.decodeSignature)
            )
        )


decodeItem : Decode.Decoder FinderItem
decodeItem =
    let
        termTypeByHash hash =
            if AbilityConstructor.isAbilityConstructorHash hash then
                "AbilityConstructor"

            else if DataConstructor.isDataConstructorHash hash then
                "DataConstructor"

            else
                "Term"

        decodeConstructorSuffix =
            Decode.map termTypeByHash (at [ "contents", "namedTerm", "termHash" ] Hash.decode)
    in
    Decode.oneOf
        [ when decodeConstructorSuffix ((==) "AbilityConstructor") (field "contents" decodeAbilityConstructorItem)
        , when decodeConstructorSuffix ((==) "DataConstructor") (field "contents" decodeDataConstructorItem)
        , when decodeTag ((==) "FoundTermResult") (field "contents" decodeTermItem)
        , when decodeTag ((==) "FoundTypeResult") (field "contents" decodeTypeItem)
        ]


decodeMatchSegments : Decode.Decoder (NEL.Nonempty MatchSegment)
decodeMatchSegments =
    let
        decodeMatchSegment =
            Decode.oneOf
                [ when (field "tag" string) ((==) "Gap") (Decode.map Gap (field "contents" string))
                , when (field "tag" string) ((==) "Match") (Decode.map Match (field "contents" string))
                ]
    in
    at [ "result", "segments" ] (decodeNonEmptyList decodeMatchSegment)


decodeFinderMatch : Decode.Decoder FinderMatch
decodeFinderMatch =
    Decode.map3 finderMatch
        (Decode.index 0 decodeScore)
        (Decode.index 0 decodeMatchSegments)
        (Decode.index 1 decodeItem)


decodeMatches : Decode.Decoder (List FinderMatch)
decodeMatches =
    Decode.list decodeFinderMatch
