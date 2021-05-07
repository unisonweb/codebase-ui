{--

  KeyboardShortcut.Key
  ====================

  Parsing from KeyboardEvent.key string
  -------------------------------------

  Not all `key` values are consistent across browsers:
  https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values

  This library does not attempt to support older inconsistent browser values,
  besides `Meta` and `OS` for Firefox.

--}


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
import System exposing (OperatingSystem(..))


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
    | QuestionMark
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

        -- Windows key is "OS" in Firefox, but will fixed to
        -- produce Meta in: https://bugzilla.mozilla.org/show_bug.cgi?id=1232918
        "OS" ->
            Meta

        "Meta" ->
            Meta

        " " ->
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

        "?" ->
            QuestionMark

        _ ->
            Raw str



-- VIEW


view : OperatingSystem -> Key -> String
view os key =
    let
        letter l casing =
            case casing of
                Lower ->
                    String.toLower l

                Upper ->
                    String.toUpper l
    in
    case key of
        A casing ->
            letter "a" casing

        B casing ->
            letter "b" casing

        C casing ->
            letter "c" casing

        D casing ->
            letter "d" casing

        E casing ->
            letter "e" casing

        F casing ->
            letter "f" casing

        G casing ->
            letter "g" casing

        H casing ->
            letter "h" casing

        I casing ->
            letter "i" casing

        J casing ->
            letter "j" casing

        K casing ->
            letter "k" casing

        L casing ->
            letter "l" casing

        M casing ->
            letter "m" casing

        N casing ->
            letter "n" casing

        O casing ->
            letter "o" casing

        P casing ->
            letter "p" casing

        Q casing ->
            letter "q" casing

        R casing ->
            letter "r" casing

        S casing ->
            letter "s" casing

        T casing ->
            letter "t" casing

        U casing ->
            letter "u" casing

        V casing ->
            letter "v" casing

        W casing ->
            letter "w" casing

        X casing ->
            letter "x" casing

        Y casing ->
            letter "y" casing

        Z casing ->
            letter "z" casing

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
            "Ctrl"

        Alt ->
            "Alt"

        Tab ->
            "⇥ "

        Meta ->
            case os of
                Windows ->
                    "⊞"

                MacOS ->
                    "⌘ "

                _ ->
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

        QuestionMark ->
            "?"

        Raw str ->
            str
