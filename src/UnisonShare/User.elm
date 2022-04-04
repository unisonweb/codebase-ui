module UnisonShare.User exposing
    ( User
    , UserDetails
    , Username
    , decodeDetails
    , usernameFromString
    , usernameToString
    )

import Definition.Readme as Readme exposing (Readme)
import Json.Decode as Decode exposing (field, maybe, string)
import Project exposing (ProjectListing)
import Url exposing (Url)


type Username
    = Username String


type alias User u =
    { u
        | username : Username
        , name : Maybe String
        , avatarUrl : Maybe Url
    }


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
        makeDetails username name avatarUrl readme =
            { username = username
            , name = name
            , avatarUrl = avatarUrl
            , readme = readme
            , projects = []
            }
    in
    Decode.map4 makeDetails
        (field "fqn" (Decode.map Username string))
        (Decode.succeed Nothing)
        (Decode.succeed Nothing)
        (maybe (field "readme" Readme.decode))
