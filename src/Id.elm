module Id exposing (Id, equals, fromString, toString)

{--

`Id` is a simple type for getting more type safe ids and to avoid working with
strings directly.  It also carries a type (phantom type), such that we can
distinguish between `Id User` and `Id Doc` and not mix them by accident.

Phantom types: 
  https://wiki.haskell.org/Phantom_type
  https://thoughtbot.com/blog/modeling-currency-in-elm-using-phantom-types

--}


type Id a
    = Id String


fromString : String -> Id a
fromString rawId =
    Id rawId


toString : Id a -> String
toString (Id s) =
    s


equals : Id a -> Id a -> Bool
equals (Id s1) (Id s2) =
    s1 == s2
