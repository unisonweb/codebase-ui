module Definition.Term exposing (..)

import Definition.Info exposing (Info)
import Syntax exposing (Syntax)


type TermSource
    = Source TermSignature Syntax
    | Builtin TermSignature


type Term a
    = Term Info a


type TermSignature
    = TermSignature Syntax


type alias TermDetail =
    Term TermSource


type alias TermSummary =
    Term TermSignature


type alias TermListing =
    Term ()
