module Definition exposing (..)


type alias SignaturePart =
    String


type alias Signature =
    List SignaturePart


type Definition
    = Term String Signature
    | Type String Signature
    | Patch
    | Doc
