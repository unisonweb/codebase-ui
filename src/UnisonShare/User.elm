module UnisonShare.User exposing
    ( User
    , UserDetails
    , Username
    , decodeDetails
    , usernameFromString
    , usernameToString
    )

import Definition.Readme as Readme exposing (Readme)
import Hash exposing (Hash)
import Json.Decode as Decode exposing (field, maybe, string)
import Project exposing (ProjectListing)


type Username
    = Username String


type alias User u =
    { u | hash : Hash, username : Username }


type alias UserDetails =
    User { readme : Maybe Readme, projects : List ProjectListing }



-- HELPERS


usernameFromString : String -> Maybe Username
usernameFromString raw =
    Just (Username raw)


usernameToString : Username -> String
usernameToString (Username raw) =
    raw



-- DECODE


decodeDetails : Decode.Decoder UserDetails
decodeDetails =
    let
        makeDetails hash username readme =
            { hash = hash, username = username, readme = readme, projects = [] }
    in
    Decode.map3 makeDetails
        (field "hash" Hash.decode)
        (field "fqn" (Decode.map Username string))
        (maybe (field "readme" Readme.decode))
