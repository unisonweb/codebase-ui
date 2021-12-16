module UnisonShare.Catalog exposing (..)

import Dict exposing (Dict)
import FullyQualifiedName as FQN
import Json.Decode as Decode
import Project exposing (ProjectListing)
import UnisonShare.Catalog.CatalogMask as CatalogMask exposing (CatalogMask)


type Catalog
    = Catalog (Dict String (List ProjectListing))



-- CREATE


empty : Catalog
empty =
    Catalog Dict.empty


catalog : CatalogMask -> List ProjectListing -> Catalog
catalog mask projectListings_ =
    let
        catalog_ project ((Catalog dict) as acc) =
            let
                projectName =
                    project
                        |> Project.slug
                        |> FQN.toString

                categoryName =
                    CatalogMask.categoryOf projectName mask

                set old =
                    case old of
                        Just ps ->
                            Just (ps ++ [ project ])

                        Nothing ->
                            Just [ project ]
            in
            case categoryName of
                Just c ->
                    Catalog (Dict.update c set dict)

                Nothing ->
                    acc
    in
    List.foldl catalog_ empty projectListings_



-- HELPERS


isEmpty : Catalog -> Bool
isEmpty (Catalog dict) =
    Dict.isEmpty dict


categories : Catalog -> List String
categories (Catalog dict) =
    Dict.keys dict


projectListings : Catalog -> List ProjectListing
projectListings (Catalog dict) =
    List.concat (Dict.values dict)


toList : Catalog -> List ( String, List ProjectListing )
toList (Catalog dict) =
    Dict.toList dict



-- DECODE


decodeCatalogMask : Decode.Decoder CatalogMask
decodeCatalogMask =
    CatalogMask.decode
