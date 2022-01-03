module UnisonShare.Catalog.CatalogMaskTests exposing (..)

import Definition.Doc as Doc
import Expect
import Test exposing (..)
import UnisonShare.Catalog.CatalogMask as CatalogMask


fromDoc : Test
fromDoc =
    describe "CatalogMask.fromDoc"
        [ test "Creates a CatalogMask from a Doc" <|
            \_ ->
                Expect.equal rawMask
                    (CatalogMask.fromDoc doc |> CatalogMask.toList)
        ]


fromList : Test
fromList =
    describe "CatalogMask.fromList"
        [ test "Creates a CatalogMask from a List" <|
            \_ ->
                let
                    catalogMask =
                        CatalogMask.fromList rawMask
                in
                Expect.equal rawMask
                    (CatalogMask.toList catalogMask)
        ]


categoryOf : Test
categoryOf =
    describe "CatalogMask.categoryOf"
        [ test "given a project, returns its category " <|
            \_ ->
                let
                    catalogMask =
                        CatalogMask.fromList rawMask
                in
                Expect.equal (Just "Featured") (CatalogMask.categoryOf "unison.base" catalogMask)
        ]


categories : Test
categories =
    describe "CatalogMask.categories"
        [ test "Returns all categories in a Mask" <|
            \_ ->
                let
                    catalogMask =
                        CatalogMask.fromList rawMask
                in
                Expect.equal
                    [ "Featured"
                    , "Web & Networking"
                    , "Parsers & Text Manipulation"
                    , "Datatypes"
                    ]
                    (CatalogMask.categories catalogMask)
        ]


projectNames : Test
projectNames =
    describe "CatalogMask.projectNames"
        [ test "Returns all project names in a Mask" <|
            \_ ->
                let
                    catalogMask =
                        CatalogMask.fromList rawMask
                in
                Expect.equal
                    [ "unison.base"
                    , "unison.distributed"
                    , "unison.http"
                    , "hojberg.textExtra"
                    , "hojberg.nanoid"
                    ]
                    (CatalogMask.projectNames catalogMask)
        ]



-- HELPERS


rawMask : List ( String, String )
rawMask =
    [ ( "unison.base", "Featured" )
    , ( "unison.distributed", "Featured" )
    , ( "unison.http", "Web & Networking" )
    , ( "hojberg.textExtra", "Parsers & Text Manipulation" )
    , ( "hojberg.nanoid", "Datatypes" )
    ]


doc : Doc.Doc
doc =
    Doc.UntitledSection
        [ Doc.Section (Doc.Word "Featured")
            [ Doc.BulletedList [ Doc.Word "unison.base", Doc.Word "unison.distributed" ] ]
        , Doc.Section (Doc.Word "Web & Networking")
            [ Doc.BulletedList [ Doc.Word "unison.http" ] ]
        , Doc.Section (Doc.Word "Parsers & Text Manipulation")
            [ Doc.BulletedList [ Doc.Word "hojberg.textExtra" ] ]
        , Doc.Section (Doc.Word "Datatypes")
            [ Doc.BulletedList [ Doc.Word "hojberg.nanoid" ] ]
        ]
