module Code.Namespace exposing (..)

import Code.Definition.Readme as Readme exposing (Readme)
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Json.Decode as Decode exposing (field, maybe)


type Namespace a
    = Namespace FQN Hash a


type alias NamespaceDetailsFields =
    { readme : Maybe Readme }


type alias NamespaceDetails =
    Namespace NamespaceDetailsFields



-- Helpers --------------------------------------------------------------------


fqn : Namespace a -> FQN
fqn (Namespace fqn_ _ _) =
    fqn_


hash : Namespace a -> Hash
hash (Namespace _ h _) =
    h


readme : NamespaceDetails -> Maybe Readme
readme (Namespace _ _ details) =
    details.readme



-- Decode ---------------------------------------------------------------------


decodeDetails : Decode.Decoder NamespaceDetails
decodeDetails =
    let
        makeDetails fqn_ hash_ readme_ =
            Namespace fqn_ hash_ { readme = readme_ }
    in
    Decode.map3 makeDetails
        (field "fqn" FQN.decode)
        (field "hash" Hash.decode)
        (maybe (field "readme" Readme.decode))
