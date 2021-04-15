module Finder.FinderMatch exposing (..)

import Definition.Term as Term exposing (Term(..), TermSummary)
import Definition.Type as Type exposing (Type(..), TypeSummary)
import FullyQualifiedName as FQN
import HashQualified exposing (HashQualified(..))
import Json.Decode as Decode exposing (at, field, string)
import Json.Decode.Extra exposing (when)
import List.Nonempty as NEL
import Util exposing (decodeNonEmptyList, decodeTag)
import Workspace.Reference exposing (Reference(..))


type FinderItem
    = TermItem TermSummary
    | TypeItem TypeSummary


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


namespace : FinderMatch -> Maybe String
namespace fm =
    case fm.item of
        TypeItem (Type _ _ summary) ->
            summary.namespace

        TermItem (Term _ _ summary) ->
            summary.namespace


reference : FinderMatch -> Reference
reference fm =
    case fm.item of
        TypeItem (Type h _ _) ->
            TypeReference (HashOnly h)

        TermItem (Term h _ _) ->
            TermReference (HashOnly h)


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
            (field "namedType" Type.decodeTypeCategory)
            (Decode.map3 makeSummary
                (field "namedType" Type.decodeFQN)
                (field "bestFoundTypeName" string)
                (field "typeDef" Type.decodeTypeSource)
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
            (field "namedTerm" Term.decodeTermCategory)
            (Decode.map3 makeSummary
                (field "namedTerm" Term.decodeFQN)
                (field "bestFoundTermName" string)
                (field "namedTerm" Term.decodeTermSignature)
            )
        )


decodeItem : Decode.Decoder FinderItem
decodeItem =
    Decode.oneOf
        [ when decodeTag ((==) "FoundTermResult") (field "contents" decodeTermItem)
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
