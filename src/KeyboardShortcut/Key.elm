module KeyboardShortcut.Key exposing
    ( Key(..)
    , LetterCase(..)
    , decode
    , fromString
    , isModifier
    , toNumber
    , view
    )

import Json.Decode as Decode


type LetterCase
    = Upper
    | Lower


type Key
    = A LetterCase
    | B LetterCase
    | C LetterCase
    | D LetterCase
    | E LetterCase
    | F LetterCase
    | G LetterCase
    | H LetterCase
    | I LetterCase
    | J LetterCase
    | K LetterCase
    | L LetterCase
    | M LetterCase
    | N LetterCase
    | O LetterCase
    | P LetterCase
    | Q LetterCase
    | R LetterCase
    | S LetterCase
    | T LetterCase
    | U LetterCase
    | V LetterCase
    | W LetterCase
    | X LetterCase
    | Y LetterCase
    | Z LetterCase
    | Semicolon
    | Comma
    | Period
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Shift
    | Ctrl
    | Alt
    | Tab
      -- Windows & Command are covered by Meta
    | Meta
    | Space
    | Escape
    | Enter
    | Backspace
    | PageUp
    | PageDown
    | End
    | Home
    | Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Insert
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    | Multiply
    | Plus
    | Minus
    | ForwardSlash
    | Raw String



-- HELPERS


isModifier : Key -> Bool
isModifier k =
    case k of
        Ctrl ->
            True

        Alt ->
            True

        Meta ->
            True

        Shift ->
            True

        _ ->
            False


toNumber : Key -> Maybe Int
toNumber k =
    case k of
        Zero ->
            Just 0

        One ->
            Just 1

        Two ->
            Just 2

        Three ->
            Just 3

        Four ->
            Just 4

        Five ->
            Just 5

        Six ->
            Just 6

        Seven ->
            Just 7

        Eight ->
            Just 8

        Nine ->
            Just 9

        _ ->
            Nothing



-- DECODE


decode : Decode.Decoder Key
decode =
    Decode.map fromString Decode.string



-- CREATE


fromString : String -> Key
fromString str =
    case str of
        "a" ->
            A Lower

        "A" ->
            A Upper

        "b" ->
            B Lower

        "B" ->
            B Upper

        "c" ->
            C Lower

        "C" ->
            C Upper

        "d" ->
            D Lower

        "D" ->
            D Upper

        "e" ->
            E Lower

        "E" ->
            E Upper

        "f" ->
            F Lower

        "F" ->
            F Upper

        "g" ->
            G Lower

        "G" ->
            G Upper

        "h" ->
            H Lower

        "H" ->
            H Upper

        "i" ->
            I Lower

        "I" ->
            I Upper

        "j" ->
            J Lower

        "J" ->
            J Upper

        "k" ->
            K Lower

        "K" ->
            K Upper

        "l" ->
            L Lower

        "L" ->
            L Upper

        "m" ->
            M Lower

        "M" ->
            M Upper

        "n" ->
            N Lower

        "N" ->
            N Upper

        "o" ->
            O Lower

        "O" ->
            O Upper

        "p" ->
            P Lower

        "P" ->
            P Upper

        "q" ->
            Q Lower

        "Q" ->
            Q Upper

        "r" ->
            R Lower

        "R" ->
            R Upper

        "s" ->
            S Lower

        "S" ->
            S Upper

        "t" ->
            T Lower

        "T" ->
            T Upper

        "u" ->
            U Lower

        "U" ->
            U Upper

        "v" ->
            V Lower

        "V" ->
            V Upper

        "w" ->
            W Lower

        "W" ->
            W Upper

        "x" ->
            X Lower

        "X" ->
            X Upper

        "y" ->
            Y Lower

        "Y" ->
            Y Upper

        "z" ->
            Z Lower

        "Z" ->
            Z Upper

        ";" ->
            Semicolon

        "," ->
            Comma

        "." ->
            Period

        "ArrowLeft" ->
            ArrowLeft

        "ArrowRight" ->
            ArrowRight

        "ArrowUp" ->
            ArrowUp

        "ArrowDown" ->
            ArrowDown

        "Shift" ->
            Shift

        "Ctrl" ->
            Ctrl

        "Alt" ->
            Alt

        "Tab" ->
            Tab

        "Meta" ->
            Meta

        "Space" ->
            Space

        "Escape" ->
            Escape

        "Enter" ->
            Enter

        "Backspace" ->
            Backspace

        "PageUp" ->
            PageUp

        "PageDown" ->
            PageDown

        "End" ->
            End

        "Home" ->
            Home

        "0" ->
            Zero

        "1" ->
            One

        "2" ->
            Two

        "3" ->
            Three

        "4" ->
            Four

        "5" ->
            Five

        "6" ->
            Six

        "7" ->
            Seven

        "8" ->
            Eight

        "9" ->
            Nine

        "Insert" ->
            Insert

        "F1" ->
            F1

        "F2" ->
            F2

        "F3" ->
            F3

        "F4" ->
            F4

        "F5" ->
            F5

        "F6" ->
            F6

        "F7" ->
            F7

        "F8" ->
            F8

        "F9" ->
            F9

        "F10" ->
            F10

        "F11" ->
            F11

        "F12" ->
            F12

        "*" ->
            Multiply

        "+" ->
            Plus

        "-" ->
            Minus

        "/" ->
            ForwardSlash

        _ ->
            Raw str



-- VIEW


view : Key -> String
view key =
    case key of
        A Lower ->
            "a"

        A Upper ->
            "A"

        B Lower ->
            "b"

        B Upper ->
            "B"

        C Lower ->
            "c"

        C Upper ->
            "C"

        D Lower ->
            "d"

        D Upper ->
            "D"

        E Lower ->
            "e"

        E Upper ->
            "E"

        F Lower ->
            "f"

        F Upper ->
            "F"

        G Lower ->
            "g"

        G Upper ->
            "G"

        H Lower ->
            "h"

        H Upper ->
            "H"

        I Lower ->
            "i"

        I Upper ->
            "I"

        J Lower ->
            "j"

        J Upper ->
            "J"

        K Lower ->
            "k"

        K Upper ->
            "K"

        L Lower ->
            "l"

        L Upper ->
            "L"

        M Lower ->
            "m"

        M Upper ->
            "M"

        N Lower ->
            "n"

        N Upper ->
            "N"

        O Lower ->
            "o"

        O Upper ->
            "O"

        P Lower ->
            "p"

        P Upper ->
            "P"

        Q Lower ->
            "q"

        Q Upper ->
            "Q"

        R Lower ->
            "r"

        R Upper ->
            "R"

        S Lower ->
            "s"

        S Upper ->
            "S"

        T Lower ->
            "t"

        T Upper ->
            "T"

        U Lower ->
            "u"

        U Upper ->
            "U"

        V Lower ->
            "v"

        V Upper ->
            "V"

        W Lower ->
            "w"

        W Upper ->
            "W"

        X Lower ->
            "x"

        X Upper ->
            "X"

        Y Lower ->
            "y"

        Y Upper ->
            "Y"

        Z Lower ->
            "z"

        Z Upper ->
            "Z"

        Semicolon ->
            ";"

        Comma ->
            ","

        Period ->
            "."

        ArrowLeft ->
            "← "

        ArrowRight ->
            "→ "

        ArrowUp ->
            "↑"

        ArrowDown ->
            "↓"

        Shift ->
            "⇧"

        Ctrl ->
            "CTRL"

        Alt ->
            "ALT"

        Tab ->
            "⇥ "

        -- Windows -> "⊞"
        -- Command -> "⌘ "
        Meta ->
            "Meta"

        Space ->
            "Space"

        Escape ->
            "Esc"

        Enter ->
            "↵ "

        Backspace ->
            "⌫ "

        PageUp ->
            "PageUp"

        PageDown ->
            "PageDown"

        End ->
            "End"

        Home ->
            "Home"

        Zero ->
            "0"

        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Insert ->
            "Insert"

        F1 ->
            "F1"

        F2 ->
            "F2"

        F3 ->
            "F3"

        F4 ->
            "F4"

        F5 ->
            "F5"

        F6 ->
            "F6"

        F7 ->
            "F8"

        F8 ->
            "F8"

        F9 ->
            "F9"

        F10 ->
            "F10"

        F11 ->
            "F11"

        F12 ->
            "F12"

        Multiply ->
            "*"

        Plus ->
            "+"

        Minus ->
            "-"

        ForwardSlash ->
            "/"

        Raw str ->
            str
