module Finder.FinderMatch exposing (..)

import Definition.Term as Term exposing (Term(..), TermSummary)
import Definition.Type as Type exposing (Type(..), TypeSummary)
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


type alias FinderMatch =
    { score : Int
    , match : MatchSegments
    , item : FinderItem
    }


name : FinderMatch -> String
name fm =
    case fm.item of
        TypeItem (Type _ _ summary) ->
            summary.name

        TermItem (Term _ _ summary) ->
            summary.name


reference : FinderMatch -> Reference
reference fm =
    case fm.item of
        TypeItem (Type h _ _) ->
            TypeReference (HashOnly h)

        TermItem (Term h _ _) ->
            TermReference (HashOnly h)



-- JSON DECODERS


decodeScore : Decode.Decoder Int
decodeScore =
    field "score" Decode.int


decodeTypeItem : Decode.Decoder FinderItem
decodeTypeItem =
    let
        makeSummary fqn name_ source =
            { fqn = fqn, name = name_, source = source }
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
            { fqn = fqn, name = name_, signature = signature }
    in
    Decode.map TermItem
        (Decode.map3 Term
            (field "namedTerm" Term.decodeHash)
            (field "namedTerm" Term.decodeTermCategory)
            (Decode.map3 makeSummary
                (field "namedTerm" Term.decodeFQN)
                (field "bestTermName" string)
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
    Decode.map3 FinderMatch
        (Decode.index 0 decodeScore)
        (Decode.index 0 decodeMatchSegments)
        (Decode.index 1 decodeItem)


decodeMatches : Decode.Decoder (List FinderMatch)
decodeMatches =
    Decode.list decodeFinderMatch
