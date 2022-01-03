module UnisonShare.CatalogTests exposing (..)

import Expect
import FullyQualifiedName as FQN
import Hash
import Project
import Test exposing (..)
import UnisonShare.Catalog as Catalog
import UnisonShare.Catalog.CatalogMask as CatalogMask


catalog : Test
catalog =
    describe "Catalog.catalog"
        [ describe "Create a Catalog from a CatalogMask and ProjectListings"
            [ test "It retains only the overlapping items between the mask and the listings" <|
                \_ ->
                    let
                        projectListings_ =
                            [ baseListing, distributedListing, textExtraListing, nanoidListing ]

                        catalog_ =
                            Catalog.catalog catalogMask projectListings_
                    in
                    Expect.equal
                        [ ( "Featured", [ baseListing, distributedListing ] )
                        , ( "Parsers & Text Manipulation", [ textExtraListing ] )
                        ]
                        (Catalog.toList catalog_)
            ]
        ]


categories : Test
categories =
    describe "Catalog.categories"
        [ test "Extracts the categories of a Catalog that exist in both the mask and the project listings" <|
            \_ ->
                let
                    projectListings_ =
                        [ baseListing, distributedListing, textExtraListing ]

                    catalog_ =
                        Catalog.catalog catalogMask projectListings_
                in
                Expect.equal [ "Featured", "Parsers & Text Manipulation" ] (Catalog.categories catalog_)
        ]


projectListings : Test
projectListings =
    describe "Catalog.projectListings"
        [ test "Extracts the projectListings of a Catalog that exist in both the mask and the project listings" <|
            \_ ->
                let
                    projectListings_ =
                        [ baseListing, textExtraListing, nanoidListing ]

                    catalog_ =
                        Catalog.catalog catalogMask projectListings_
                in
                Expect.equal [ baseListing, textExtraListing ] (Catalog.projectListings catalog_)
        ]


toList : Test
toList =
    describe "Catalog.toList"
        [ test "Returns the Catalog as a List" <|
            \_ ->
                let
                    projectListings_ =
                        [ baseListing, distributedListing, textExtraListing, nanoidListing ]

                    catalog_ =
                        Catalog.catalog catalogMask projectListings_
                in
                Expect.equal
                    [ ( "Featured", [ baseListing, distributedListing ] )
                    , ( "Parsers & Text Manipulation", [ textExtraListing ] )
                    ]
                    (Catalog.toList catalog_)
        ]


search : Test
search =
    describe "Catalog.search"
        [ test "Fuzzy finds projects in the catalog by project name " <|
            \_ ->
                let
                    projectListings_ =
                        [ baseListing, distributedListing, textExtraListing, nanoidListing ]

                    catalog_ =
                        Catalog.catalog catalogMask projectListings_
                in
                Expect.equal
                    [ ( baseListing, "Featured" )
                    , ( distributedListing, "Featured" )
                    ]
                    (Catalog.search catalog_ "unison")
        , test "Fuzzy finds projects in the catalog by category" <|
            \_ ->
                let
                    projectListings_ =
                        [ baseListing, distributedListing, textExtraListing, nanoidListing ]

                    catalog_ =
                        Catalog.catalog catalogMask projectListings_
                in
                Expect.equal
                    [ ( textExtraListing, "Parsers & Text Manipulation" )
                    ]
                    (Catalog.search catalog_ "parsers")
        ]



-- helpers


catalogMask : CatalogMask.CatalogMask
catalogMask =
    CatalogMask.fromList
        [ ( "unison.base", "Featured" )
        , ( "unison.distributed", "Featured" )
        , ( "unison.http", "Web & Networking" )
        , ( "hojberg.textExtra", "Parsers & Text Manipulation" )
        , ( "hojberg.money", "Datatypes" )
        ]


baseListing : Project.ProjectListing
baseListing =
    { owner = Project.Owner "unison"
    , name = FQN.fromString "base"
    , hash = Hash.unsafeFromString "##unison.base"
    }


distributedListing : Project.ProjectListing
distributedListing =
    { owner = Project.Owner "unison"
    , name = FQN.fromString "distributed"
    , hash = Hash.unsafeFromString "##unison.distributed"
    }


httpListing : Project.ProjectListing
httpListing =
    { owner = Project.Owner "unison"
    , name = FQN.fromString "http"
    , hash = Hash.unsafeFromString "##unison.http"
    }


textExtraListing : Project.ProjectListing
textExtraListing =
    { owner = Project.Owner "hojberg"
    , name = FQN.fromString "textExtra"
    , hash = Hash.unsafeFromString "##hojberg.textExtra"
    }


{-| Note: does purposely not exist in mask
-}
nanoidListing : Project.ProjectListing
nanoidListing =
    { owner = Project.Owner "hojberg"
    , name = FQN.fromString "nanoid"
    , hash = Hash.unsafeFromString "##hojberg.nanoid"
    }
