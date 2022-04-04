module UnisonShare.Log exposing (..)


type LogEntryStatus
    = Success
    | Info
    | Error


type alias LogEntry l =
    { l
        | status : LogEntryStatus
        , id : String
        , title : String
        , description : Maybe String
        , loggedAt : String
    }


type alias Log a =
    List (LogEntry a)
