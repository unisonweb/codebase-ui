module SearchResultsTests exposing (..)

import Expect
import SearchResults
import Test exposing (..)


fromList : Test
fromList =
    describe "SearchResults.fromList"
        [ test "Returns Empty for empty list" <|
            \_ ->
                let
                    result =
                        SearchResults.fromList []
                in
                Expect.equal SearchResults.Empty result
        , test "Includes all matches" <|
            \_ ->
                let
                    result =
                        SearchResults.fromList [ "a", "b", "c" ]
                            |> SearchResults.toList
                in
                Expect.equal [ "a", "b", "c" ] result
        ]


next : Test
next =
    describe "SearchResults.next"
        [ test "moves focus to the next element" <|
            \_ ->
                let
                    result =
                        SearchResults.from [ "a" ] "b" [ "c" ]
                            |> SearchResults.next
                            |> SearchResults.toMaybe
                            |> Maybe.map SearchResults.focus
                in
                Expect.equal (Just "c") result
        , test "keeps focus if no elements after" <|
            \_ ->
                let
                    result =
                        SearchResults.from [ "a", "b" ] "c" []
                            |> SearchResults.next
                            |> SearchResults.toMaybe
                            |> Maybe.map SearchResults.focus
                in
                Expect.equal (Just "c") result
        ]


prev : Test
prev =
    describe "SearchResults.prev"
        [ test "moves focus to the prev element" <|
            \_ ->
                let
                    result =
                        SearchResults.from [ "a" ] "b" [ "c" ]
                            |> SearchResults.prev
                            |> SearchResults.toMaybe
                            |> Maybe.map SearchResults.focus
                in
                Expect.equal (Just "a") result
        , test "keeps focus if no elements before" <|
            \_ ->
                let
                    result =
                        SearchResults.from [] "a" [ "b", "c" ]
                            |> SearchResults.prev
                            |> SearchResults.toMaybe
                            |> Maybe.map SearchResults.focus
                in
                Expect.equal (Just "a") result
        ]



-- QUERY


getAt : Test
getAt =
    describe "SearchResults.getAt"
        [ test "When there are no results it returns Nothing" <|
            \_ ->
                let
                    result =
                        SearchResults.fromList [] |> SearchResults.getAt 3
                in
                Expect.equal Nothing result
        , test "When there the index is out of bounds it returns Nothing" <|
            \_ ->
                let
                    result =
                        SearchResults.fromList [ "foo", "bar", "baz" ] |> SearchResults.getAt 10
                in
                Expect.equal Nothing result
        , test "When there is an item at the index it returns Just of that item" <|
            \_ ->
                let
                    result =
                        SearchResults.fromList [ "foo", "bar", "baz" ] |> SearchResults.getAt 1
                in
                Expect.equal (Just "bar") result
        ]



-- MAP


mapToList : Test
mapToList =
    describe "SearchResults.mapToList"
        [ test "Maps definitions" <|
            \_ ->
                let
                    result =
                        SearchResults.from [ "a" ] "b" [ "c" ]
                            |> SearchResults.mapToList (\x isFocused -> ( x ++ "mapped", isFocused ))

                    expected =
                        [ ( "amapped", False ), ( "bmapped", True ), ( "cmapped", False ) ]
                in
                Expect.equal expected result
        ]
