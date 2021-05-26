module CodebaseTree.NamespaceListingTests exposing (..)

import CodebaseTree.NamespaceListing as NamespaceListing exposing (NamespaceListing(..), NamespaceListingChild(..))
import Expect
import FullyQualifiedName as FQN
import Hash
import RemoteData exposing (RemoteData(..))
import Test exposing (..)


map : Test
map =
    describe "CodebaseTree.NamespaceListing.map"
        [ test "runs the function on deeply nestes namespaces" <|
            \_ ->
                let
                    hashA =
                        Hash.fromString "#a"

                    hashB =
                        Hash.fromString "#c"

                    hashC =
                        Hash.fromString "#b"

                    fqnA =
                        FQN.fromString "a"

                    fqnB =
                        FQN.fromString "a.b"

                    fqnC =
                        FQN.fromString "a.b.c"

                    original =
                        Maybe.map3
                            (\ha hb hc ->
                                NamespaceListing ha
                                    fqnA
                                    (Success
                                        [ SubNamespace
                                            (NamespaceListing
                                                hb
                                                fqnB
                                                (Success [ SubNamespace (NamespaceListing hc fqnC NotAsked) ])
                                            )
                                        ]
                                    )
                            )
                            hashA
                            hashB
                            hashC

                    expected =
                        Maybe.map3
                            (\ha hb hc ->
                                NamespaceListing ha
                                    fqnA
                                    (Success
                                        [ SubNamespace
                                            (NamespaceListing
                                                hb
                                                fqnB
                                                (Success [ SubNamespace (NamespaceListing hc fqnC Loading) ])
                                            )
                                        ]
                                    )
                            )
                            hashA
                            hashB
                            hashC

                    f ((NamespaceListing h fqn _) as nl) =
                        if FQN.equals fqn fqnC then
                            NamespaceListing h fqn Loading

                        else
                            nl

                    result =
                        Maybe.map (NamespaceListing.map f) original
                in
                Expect.equal expected result
        ]
