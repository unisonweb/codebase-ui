module UnisonShare.Catalog.CatalogMask exposing
    ( CatalogMask
    , categories
    , categoryOf
    , decode
    , empty
    , fromDoc
    , fromList
    , isEmpty
    , projectNames
    , toList
    )

import Definition.Doc as Doc exposing (Doc)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (field, index)
import List.Extra as ListE


{-| CatalogMask is used to create the Catalog and map ProjectListings to their
categories.

Indexed by project to category "unison.http" -> "Web & Networking"

-}
type CatalogMask
    = CatalogMask (Dict String String)



-- Create


empty : CatalogMask
empty =
    CatalogMask Dict.empty


fromList : List ( String, String ) -> CatalogMask
fromList categories_ =
    CatalogMask (Dict.fromList categories_)


{-| For right now, a CatalogMask is fetched as a Doc from the server and as
such we can parse that into a CatalogMask if it has the right shape (if not,
its an empty mask)
-}
fromDoc : Doc -> CatalogMask
fromDoc doc =
    let
        category_ d =
            case d of
                Doc.Section category content ->
                    case content of
                        [ Doc.BulletedList projects ] ->
                            List.map (\p -> ( Doc.toString "" p, Doc.toString " " category )) projects

                        _ ->
                            []

                _ ->
                    []

        categories_ =
            case doc of
                Doc.UntitledSection ds ->
                    List.concatMap category_ ds

                _ ->
                    []
    in
    fromList categories_



-- Helpers


isEmpty : CatalogMask -> Bool
isEmpty (CatalogMask mask) =
    Dict.isEmpty mask


categoryOf : String -> CatalogMask -> Maybe String
categoryOf projectName (CatalogMask mask) =
    Dict.get projectName mask


categories : CatalogMask -> List String
categories (CatalogMask mask) =
    Dict.values mask |> ListE.unique


projectNames : CatalogMask -> List String
projectNames (CatalogMask mask) =
    Dict.keys mask


toList : CatalogMask -> List ( String, String )
toList (CatalogMask mask) =
    Dict.toList mask



-- Decode


decode : Decode.Decoder CatalogMask
decode =
    Decode.map fromDoc decodeCatalogDoc


decodeCatalogDoc : Decode.Decoder Doc
decodeCatalogDoc =
    let
        decodeDoc : Decode.Decoder Doc
        decodeDoc =
            field "termDocs" (index 0 (index 2 Doc.decode))

        decodeTermDocs : Decode.Decoder (List Doc)
        decodeTermDocs =
            Decode.keyValuePairs decodeDoc |> Decode.map (List.map Tuple.second)

        decodeList : Decode.Decoder (List Doc)
        decodeList =
            field "termDefinitions" decodeTermDocs
    in
    Decode.map List.head decodeList
        |> Decode.andThen
            (Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Empty list")
            )
