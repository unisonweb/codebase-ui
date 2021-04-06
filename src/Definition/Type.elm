module Definition.Type exposing (..)

import Definition.Info exposing (Info)
import Syntax exposing (Syntax)


type TypeSource
    = Source Syntax
    | Builtin


type Type a
    = Type Info a


{-| Currently same as TypeSummary, but will include Docs later -
-}
type alias TypeDetail =
    Type TypeSource


type alias TypeSummary =
    Type TypeSource


type alias TypeListing =
    Type ()
