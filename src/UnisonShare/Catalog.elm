module UnisonShare.Catalog exposing (..)

import Dict
import FullyQualifiedName as FQN
import Json.Decode as Decode
import OrderedDict exposing (OrderedDict)
import Project exposing (ProjectListing)
import UnisonShare.Catalog.CatalogMask as CatalogMask exposing (CatalogMask)


type Catalog
    = Catalog (OrderedDict String (List ProjectListing))



-- CREATE


{-| Create the empty Catalog
-}
empty : Catalog
empty =
    Catalog OrderedDict.empty


{-| Create a Catalog using a mask and listings. The mask is used to get the
categories and to ensure the category sort order is preserved
-}
catalog : CatalogMask -> List ProjectListing -> Catalog
catalog mask projectListings_ =
    let
        group project acc =
            let
                projectName =
                    project |> Project.slug |> FQN.toString

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
                    Dict.update c set acc

                Nothing ->
                    acc

        grouped =
            List.foldl group Dict.empty projectListings_

        sortedCategories category acc =
            case Dict.get category grouped of
                Just ps ->
                    insert category ps acc

                Nothing ->
                    acc
    in
    List.foldl sortedCategories empty (CatalogMask.categories mask)


{-| Insert a category and projects within into a Catalog
-}
insert : String -> List ProjectListing -> Catalog -> Catalog
insert categoryName projectListings_ (Catalog dict) =
    Catalog (OrderedDict.insert categoryName projectListings_ dict)


{-| Create a Catalog given a list of projects grouped by category
-}
fromList : List ( String, List ProjectListing ) -> Catalog
fromList items =
    items
        |> OrderedDict.fromList
        |> Catalog



-- HELPERS


isEmpty : Catalog -> Bool
isEmpty (Catalog dict) =
    OrderedDict.isEmpty dict


{-| Extract all categories from a Catalog
-}
categories : Catalog -> List String
categories (Catalog dict) =
    OrderedDict.keys dict


{-| Extract all project listings from a Catalog
-}
projectListings : Catalog -> List ProjectListing
projectListings (Catalog dict) =
    List.concat (OrderedDict.values dict)


{-| Convert a Catalog to a list of project listings grouped by category
-}
toList : Catalog -> List ( String, List ProjectListing )
toList (Catalog dict) =
    OrderedDict.toList dict



-- DECODE


decodeCatalogMask : Decode.Decoder CatalogMask
decodeCatalogMask =
    CatalogMask.decode
