module OpenDefinitionsTests exposing (..)

import Expect
import Hash
import OpenDefinitions exposing (..)
import RemoteData exposing (RemoteData(..))
import Test exposing (..)


equalKeys : OpenDefinitions -> OpenDefinitions -> Bool
equalKeys a b =
    let
        keys =
            OpenDefinitions.toList >> List.map (Tuple.first >> Hash.toString)
    in
    keys a == keys b


insertAfter : Test
insertAfter =
    let
        afterHash =
            Hash.fromString "#afterHash"

        insertHash =
            Hash.fromString "#insertHash"
    in
    describe "OpenDefinitions.insertAfter"
        [ test "Inserts a definition after a Hash" <|
            \_ ->
                let
                    original =
                        OpenDefinitions.fromList
                            [ ( Hash.fromString "#foo", NotAsked )
                            , ( afterHash, NotAsked )
                            , ( Hash.fromString "#bar", NotAsked )
                            ]

                    expected =
                        OpenDefinitions.fromList
                            [ ( Hash.fromString "#foo", NotAsked )
                            , ( afterHash, NotAsked )
                            , ( insertHash, NotAsked )
                            , ( Hash.fromString "#bar", NotAsked )
                            ]

                    result =
                        OpenDefinitions.insertAfter afterHash ( insertHash, NotAsked ) original
                in
                Expect.true "keys are equal" (equalKeys expected result)
        , test "When the Hash is missing from OpenDefinitions, insert at the end" <|
            \_ ->
                let
                    original =
                        OpenDefinitions.fromList
                            [ ( Hash.fromString "#foo", NotAsked )
                            , ( Hash.fromString "#bar", NotAsked )
                            ]

                    expected =
                        OpenDefinitions.fromList
                            [ ( Hash.fromString "#foo", NotAsked )
                            , ( Hash.fromString "#bar", NotAsked )
                            , ( insertHash, NotAsked )
                            ]

                    result =
                        OpenDefinitions.insertAfter afterHash ( insertHash, NotAsked ) original
                in
                Expect.true "keys are equals" (equalKeys expected result)
        , test "When the definition already exists in OpenDefinitions, replace after the Hash" <|
            \_ ->
                let
                    original =
                        OpenDefinitions.fromList
                            [ ( insertHash, NotAsked )
                            , ( Hash.fromString "#foo", NotAsked )
                            , ( afterHash, NotAsked )
                            , ( Hash.fromString "#bar", NotAsked )
                            ]

                    expected =
                        OpenDefinitions.fromList
                            [ ( Hash.fromString "#foo", NotAsked )
                            , ( afterHash, NotAsked )
                            , ( insertHash, NotAsked )
                            , ( Hash.fromString "#bar", NotAsked )
                            ]

                    result =
                        OpenDefinitions.insertAfter afterHash ( insertHash, NotAsked ) original
                in
                Expect.true "keys equals" (equalKeys expected result)
        , test "When the Hash is missing and the Definition already exists in OpenDefinitions, replace at the end" <|
            \_ ->
                let
                    original =
                        OpenDefinitions.fromList
                            [ ( Hash.fromString "#foo", NotAsked )
                            , ( insertHash, NotAsked )
                            , ( Hash.fromString "#bar", NotAsked )
                            ]

                    expected =
                        OpenDefinitions.fromList
                            [ ( Hash.fromString "#foo", NotAsked )
                            , ( Hash.fromString "#bar", NotAsked )
                            , ( insertHash, NotAsked )
                            ]

                    result =
                        OpenDefinitions.insertAfter afterHash ( insertHash, NotAsked ) original
                in
                Expect.true "keys equals" (equalKeys expected result)
        ]


replace : Test
replace =
    let
        replaceHash =
            Hash.fromString "#replaceHash"
    in
    describe "OpenDefinitions.replace"
        [ test "Replace an existing definition by Hash" <|
            \_ ->
                let
                    openDefs =
                        OpenDefinitions.fromList
                            [ ( Hash.fromString "#foo", NotAsked )
                            , ( Hash.fromString "#replaceHash", NotAsked )
                            , ( Hash.fromString "#bar", NotAsked )
                            ]

                    result =
                        OpenDefinitions.replace replaceHash Loading openDefs
                in
                Expect.equal (Just Loading) (OpenDefinitions.get replaceHash result)
        , test "Do nothing if the Hash is not found" <|
            \_ ->
                let
                    openDefs =
                        OpenDefinitions.fromList
                            [ ( Hash.fromString "#foo", NotAsked )
                            , ( Hash.fromString "#bar", NotAsked )
                            ]

                    result =
                        OpenDefinitions.replace replaceHash Loading openDefs
                in
                Expect.false
                    "The element is not part of OpenDefinitions"
                    (OpenDefinitions.member replaceHash openDefs)
        ]
