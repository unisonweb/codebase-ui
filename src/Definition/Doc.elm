module Definition.Doc exposing
    ( Doc(..)
    , DocFoldToggles
    , SpecialForm(..)
    , decode
    , emptyDocFoldToggles
    , isDocFoldToggled
    , toggleFold
    , view
    )

import Definition.Reference exposing (Reference(..))
import Definition.Source as Source exposing (Source(..))
import Definition.Term as Term exposing (TermSignature(..))
import Html
    exposing
        ( Html
        , a
        , article
        , aside
        , blockquote
        , br
        , div
        , hr
        , img
        , li
        , ol
        , p
        , section
        , span
        , strong
        , table
        , tbody
        , td
        , text
        , tr
        , ul
        )
import Html.Attributes exposing (alt, class, href, id, rel, src, start, target, title)
import Html.Events exposing (onClick)
import Id exposing (Id)
import Json.Decode as Decode exposing (bool, field, index, int, string)
import Json.Decode.Extra exposing (when)
import Set exposing (Set)
import Syntax exposing (Syntax)
import UI
import UI.Icon as Icon


type
    Doc
    -- Just raw text embedded in a doc. Will be unbroken.
    = Word String
      -- Inline monospace, as in ''some monospace code''.
    | Code Doc
      -- Block monospace with syntax highlighting.
      -- ''' blocks are parsed as ``` raw
    | CodeBlock String Doc
    | Bold Doc
    | Italic Doc
    | Strikethrough Doc
      -- Can be used to affect HTML rendering
    | Style String Doc
      -- Create a named fragment/target anchor point which can be used in
      -- links that will result in urls like
      -- https://unison-lang.org/#section1
    | Anchor String Doc
    | Blockquote Doc
    | Blankline
    | Linebreak
      -- For longer sections, this inserts a doodad or thingamajig
    | SectionBreak
      -- Tooltip inner tooltipContent
    | Tooltip Doc Doc
      -- Aside asideContent
    | Aside Doc
      -- Callout icon content
    | Callout (Maybe Doc) Doc
      -- Table rows
    | Table (List (List Doc))
      -- If folded, only summary is shown, otherwise
      -- summary is followed by details. Some renderers
      -- will present this as a toggle or clickable elipses
    | Folded { id : Id Doc, isFolded : Bool, summary : Doc, details : Doc }
      -- Documents separated by spaces and wrapped to available width
    | Paragraph (List Doc)
    | BulletedList (List Doc)
      -- NumberedList startingNumber listElements
    | NumberedList Int (List Doc)
      -- Section title subelements
    | Section Doc (List Doc)
      -- [our website](https://unisonweb.org) or [blah]({type MyType})
    | NamedLink Doc Doc
      -- image alt-text link caption
    | Image Doc Doc (Maybe Doc)
    | Special SpecialForm
      -- Concatenation of docs
    | Join (List Doc)
      -- A section with no title but otherwise laid out the same
    | UntitledSection (List Doc)
      -- A list of documents that should start on separate lines;
      -- this is used for nested lists, for instance
      -- * A
      --   * A.1
      --   * A.2
      -- * B
      --   * B.1
      --   * B.2
      -- Is modeled as:
      --   BulletedList [ Column [A, BulletedList [A.1, A.2]]
      --                , Column [B, BulletedList [B.1, B.2]]
    | Column (List Doc)
      -- Sometimes useful in paragraph text to avoid line breaks in
      -- awkward places
    | Group Doc


type
    SpecialForm
    -- @source{type Maybe, List.map @ note1 note2} or @foldedSource{type Maybe, List.map}
    -- The notes are ignored currently, but will later be used to produce
    -- rich annotated source code with tooltips, highlights and whatnot.
    --
    -- The backend has both Source and FoldedSource, but here on the Front-End,
    -- we merge the two with a Bool
    = Source (List { id : Id Doc, isFolded : Bool, summary : Syntax, details : Source })
      -- In `Example n expr`, `n` is the number of lambda parameters
      -- that should be elided during display.
      -- Ex: `Example 2 '(x y -> foo x y)` should render as `foo x y`.
      -- Ex: `Example 0 '(1 + 1)` should render as `42`.
      -- In the frontend we dont have
    | Example Syntax
      -- Same as `Example`, but as a block rather than inline element
    | ExampleBlock Syntax
      -- {type Maybe} or {List.map}
    | Link Syntax
      -- @signatures{List.map, List.filter, List.foldLeft}
    | Signature (List TermSignature)
      -- @signature{List.map}
    | SignatureInline TermSignature
      -- ```
      -- id x = x
      -- id 42 + 1
      -- ```
    | Eval Syntax Syntax
      -- @eval{1 + 1}
    | EvalInline Syntax Syntax
      -- For extensions like a `Diagram` or `Animation` type.
      -- Renderers will be best effort for these; not all
      -- renderers will support all extensions, currently wrapping Syntax
    | Embed Syntax
    | EmbedInline Syntax



-- FOLD STATE


{-| An Id present in DocFoldToggles means that the Bool on a Folded or
Source should be negated.

This type is meant to track state on an adjacent level to Doc, for instance on
the WorkspaceItem level.

-}
docId : String -> Id Doc
docId raw =
    Id.fromString raw


type DocFoldToggles
    = DocFoldToggles (Set String)


emptyDocFoldToggles : DocFoldToggles
emptyDocFoldToggles =
    DocFoldToggles Set.empty


toggleFold : DocFoldToggles -> Id Doc -> DocFoldToggles
toggleFold (DocFoldToggles toggles) id =
    let
        rawId =
            Id.toString id
    in
    if Set.member rawId toggles then
        DocFoldToggles (Set.remove rawId toggles)

    else
        DocFoldToggles (Set.insert rawId toggles)


isDocFoldToggled : DocFoldToggles -> Id Doc -> Bool
isDocFoldToggled (DocFoldToggles toggles) id =
    let
        rawId =
            Id.toString id
    in
    Set.member rawId toggles



-- VIEW


view : (Reference -> msg) -> (Id Doc -> msg) -> DocFoldToggles -> Doc -> Html msg
view refToMsg toggleFoldMsg docFoldToggles document =
    let
        viewConfig =
            Source.Rich refToMsg

        viewSource =
            Source.view viewConfig

        viewSignature =
            Source.viewTermSignature viewConfig

        linked =
            Syntax.Linked refToMsg

        viewSyntax =
            Syntax.view linked

        view_ sectionLevel doc =
            let
                -- Make it simple to retain the sectionLevel when recurring.
                -- the Section variant increments it locally
                viewAtCurrentSectionLevel =
                    view_ sectionLevel
            in
            case doc of
                Word word ->
                    span [ class "word" ] [ text word ]

                Code code ->
                    span [ class "rich source inline-code" ] [ UI.inlineCode [] (viewAtCurrentSectionLevel code) ]

                CodeBlock lang code ->
                    div [ class "rich source code", lang |> stringToClass |> class ] [ UI.codeBlock [] (viewAtCurrentSectionLevel code) ]

                Bold d ->
                    strong [] [ viewAtCurrentSectionLevel d ]

                Italic d ->
                    span [ class "italic" ] [ viewAtCurrentSectionLevel d ]

                Strikethrough d ->
                    span [ class "strikethrough" ] [ viewAtCurrentSectionLevel d ]

                -- TODO: Should this be a style string instead of a class?
                Style cssClass d ->
                    span [ cssClass |> stringToClass |> class ] [ viewAtCurrentSectionLevel d ]

                Anchor id_ d ->
                    a [ id id_, target id_ ] [ viewAtCurrentSectionLevel d ]

                Blockquote d ->
                    blockquote [] [ viewAtCurrentSectionLevel d ]

                Blankline ->
                    div [] [ br [] [], br [] [] ]

                Linebreak ->
                    br [] []

                SectionBreak ->
                    hr [] []

                Tooltip triggerContent tooltipContent ->
                    UI.withTooltip
                        (viewAtCurrentSectionLevel tooltipContent)
                        (viewAtCurrentSectionLevel triggerContent)

                Aside d ->
                    aside [] [ viewAtCurrentSectionLevel d ]

                Callout icon content ->
                    let
                        ( cls, ico ) =
                            case icon of
                                Just (Word emoji) ->
                                    ( class "callout callout-with-icon", div [ class "callout-icon" ] [ text emoji ] )

                                _ ->
                                    ( class "callout", UI.nothing )
                    in
                    div [ cls ]
                        [ ico
                        , div [ class "callout-content" ]
                            [ viewAtCurrentSectionLevel content ]
                        ]

                Table rows ->
                    let
                        viewCell d =
                            td [] [ viewAtCurrentSectionLevel d ]

                        viewRow cells =
                            tr [] (List.map viewCell cells)
                    in
                    table [] [ tbody [] (List.map viewRow rows) ]

                Folded { id, isFolded, summary, details } ->
                    let
                        isFolded_ =
                            if isDocFoldToggled docFoldToggles id then
                                not isFolded

                            else
                                isFolded
                    in
                    if isFolded_ then
                        div [ class "folded", class "is-folded" ]
                            -- Caret orientation for folded/unfolded is rotated
                            -- by CSS such that it can be animated
                            [ a [ onClick (toggleFoldMsg id) ] [ Icon.view Icon.caretDown ]
                            , viewAtCurrentSectionLevel summary
                            ]

                    else
                        div [ class "folded" ]
                            [ a [ onClick (toggleFoldMsg id) ] [ Icon.view Icon.caretDown ]
                            , div []
                                [ div [] [ viewAtCurrentSectionLevel summary ]
                                , viewAtCurrentSectionLevel details
                                ]
                            ]

                Paragraph content ->
                    p [] (List.map viewAtCurrentSectionLevel content)

                BulletedList items ->
                    let
                        viewItem d =
                            li [] [ viewAtCurrentSectionLevel d ]
                    in
                    ul [] (List.map viewItem items)

                NumberedList startNum items ->
                    let
                        viewItem d =
                            li [] [ viewAtCurrentSectionLevel d ]
                    in
                    ol [ start startNum ] (List.map viewItem items)

                Section title content ->
                    let
                        -- Unison Doc allows endlessly deep section nesting with
                        -- titles, but HTML only supports to h1-h6, so we clamp
                        -- the sectionLevel when converting
                        level =
                            min 6 sectionLevel

                        titleEl =
                            Html.node ("h" ++ String.fromInt level) [] [ viewAtCurrentSectionLevel title ]
                    in
                    section [] (titleEl :: List.map (view_ (sectionLevel + 1)) content)

                NamedLink label href_ ->
                    case href_ of
                        Word h ->
                            a [ href h, rel "noopener", target "_blank" ] [ viewAtCurrentSectionLevel label ]

                        _ ->
                            span [] [ viewAtCurrentSectionLevel label ]

                Image altText src_ caption ->
                    let
                        altAttr =
                            case altText of
                                Word t ->
                                    [ alt t ]

                                _ ->
                                    []

                        image =
                            case src_ of
                                Word s ->
                                    img (altAttr ++ [ src s ]) []

                                _ ->
                                    UI.nothing

                        imageWithCaption c =
                            div [ class "image-with-caption" ]
                                [ image
                                , div [ class "caption" ] [ viewAtCurrentSectionLevel c ]
                                ]
                    in
                    caption
                        |> Maybe.map imageWithCaption
                        |> Maybe.withDefault image

                Special specialForm ->
                    case specialForm of
                        Source sources ->
                            let
                                viewFoldedSource { id, isFolded, summary, details } =
                                    let
                                        isFolded_ =
                                            if isDocFoldToggled docFoldToggles id then
                                                not isFolded

                                            else
                                                isFolded
                                    in
                                    if isFolded_ then
                                        div [ class "folded", class "is-folded" ]
                                            -- Caret orientation for folded/unfolded is rotated
                                            -- by CSS such that it can be animated
                                            [ a [ onClick (toggleFoldMsg id) ] [ Icon.view Icon.caretRight ]
                                            , UI.inlineCode [] (viewSyntax summary)
                                            ]

                                    else
                                        div [ class "folded" ]
                                            [ a [ onClick (toggleFoldMsg id) ] [ Icon.view Icon.caretRight ]
                                            , viewSource details
                                            ]
                            in
                            div [ class "folded-sources" ]
                                (List.map viewFoldedSource sources)

                        Example syntax ->
                            span [ class "example-inline" ] [ UI.inlineCode [] (viewSyntax syntax) ]

                        ExampleBlock syntax ->
                            div [ class "example" ] [ UI.codeBlock [] (viewSyntax syntax) ]

                        Link syntax ->
                            UI.inlineCode [] (viewSyntax syntax)

                        Signature signatures ->
                            div [ class "signatures" ]
                                (List.map
                                    (\signature -> div [ class "signature" ] [ viewSignature signature ])
                                    signatures
                                )

                        SignatureInline signature ->
                            span [ class "signature-inline" ] [ viewSignature signature ]

                        Eval source result ->
                            div
                                [ class "eval" ]
                                [ UI.codeBlock [] (div [] [ viewSyntax source, div [ class "result" ] [ viewSyntax result ] ]) ]

                        EvalInline source result ->
                            span [ class "eval-inline" ]
                                [ UI.inlineCode [] (span [] [ viewSyntax source, span [ class "result" ] [ viewSyntax result ] ]) ]

                        Embed syntax ->
                            div [ class "embed" ] [ UI.codeBlock [] (viewSyntax syntax) ]

                        EmbedInline syntax ->
                            span [ class "embed-inline" ] [ UI.inlineCode [] (viewSyntax syntax) ]

                Join docs ->
                    div [] (List.map viewAtCurrentSectionLevel docs)

                UntitledSection content ->
                    section [] (List.map viewAtCurrentSectionLevel content)

                Column content ->
                    ul [ class "column" ]
                        (List.map
                            (\c -> li [] [ viewAtCurrentSectionLevel c ])
                            content
                        )

                Group content ->
                    span [ class "group" ] [ viewAtCurrentSectionLevel content ]
    in
    article [ class "doc" ] [ view_ 1 document ]



-- DECODE


decodeSpecialForm : Decode.Decoder SpecialForm
decodeSpecialForm =
    let
        tag =
            field "tag" string

        decodeSignature =
            Decode.map TermSignature Syntax.decode

        decodeSource isFolded =
            Decode.map2
                (\summary source ->
                    { id = docId "TODO"
                    , isFolded = isFolded
                    , summary = summary
                    , details = source
                    }
                )
                (field "summary" Syntax.decode)
                (Decode.map (Source.Term "TODO")
                    (Term.decodeTermSource
                        [ "termDefiniton", "tag" ]
                        [ "signature" ]
                        [ "termDefinition", "contents" ]
                    )
                )
    in
    Decode.oneOf
        [ when tag ((==) "Source") (Decode.map Source (field "contents" (Decode.list (decodeSource False))))
        , when tag ((==) "FoldedSource") (Decode.map Source (field "contents" (Decode.list (decodeSource True))))
        , when tag ((==) "Example") (Decode.map Example (field "contents" Syntax.decode))
        , when tag ((==) "ExampleBlock") (Decode.map ExampleBlock (field "contents" Syntax.decode))
        , when tag ((==) "Link") (Decode.map Link (field "contents" Syntax.decode))
        , when tag ((==) "Signature") (Decode.map Signature (field "contents" (Decode.list decodeSignature)))
        , when tag ((==) "SignatureInline") (Decode.map SignatureInline (field "contents" decodeSignature))
        , when tag
            ((==) "Eval")
            (Decode.map2 Eval
                (field "contents" (index 0 Syntax.decode))
                (field "contents" (index 1 Syntax.decode))
            )
        , when tag
            ((==) "EvalInline")
            (Decode.map2 EvalInline
                (field "contents" (index 0 Syntax.decode))
                (field "contents" (index 1 Syntax.decode))
            )
        , when tag ((==) "Embed") (Decode.map Embed (field "contents" Syntax.decode))
        , when tag ((==) "EmbedInline") (Decode.map EmbedInline (field "contents" Syntax.decode))
        ]


decode : Decode.Decoder Doc
decode =
    let
        tag =
            field "tag" string

        nested =
            Decode.lazy (\_ -> decode)
    in
    Decode.oneOf
        [ when tag ((==) "Word") (Decode.map Word (field "contents" string))
        , when tag ((==) "Code") (Decode.map Code (field "contents" nested))
        , when tag
            ((==) "CodeBlock")
            (Decode.map2 CodeBlock
                (field "contents" (index 0 string))
                (field "contents" (index 1 nested))
            )
        , when tag ((==) "Bold") (Decode.map Bold (field "contents" nested))
        , when tag ((==) "Italic") (Decode.map Italic (field "contents" nested))
        , when tag ((==) "Strikethrough") (Decode.map Strikethrough (field "contents" nested))
        , when tag
            ((==) "Style")
            (Decode.map2 Style
                (field "contents" (index 0 string))
                (field "contents" (index 1 nested))
            )
        , when tag
            ((==) "Anchor")
            (Decode.map2 Anchor
                (field "contents" (index 0 string))
                (field "contents" (index 1 nested))
            )
        , when tag ((==) "Blockquote") (Decode.map Blockquote (field "contents" nested))
        , when tag ((==) "Blankline") (Decode.succeed Blankline)
        , when tag ((==) "Linebreak") (Decode.succeed Linebreak)
        , when tag ((==) "SectionBreak") (Decode.succeed SectionBreak)
        , when tag
            ((==) "Tooltip")
            (Decode.map2 Tooltip
                (field "contents" (index 0 nested))
                (field "contents" (index 1 nested))
            )
        , when tag ((==) "Aside") (Decode.map Aside (field "contents" nested))
        , when tag
            ((==) "Callout")
            (Decode.map2 Callout
                (Decode.maybe (field "contents" (index 0 nested)))
                (field "contents" (index 1 nested))
            )
        , when tag ((==) "Table") (Decode.map Table (field "contents" (Decode.list (Decode.list nested))))
        , when tag
            ((==) "Folded")
            (Decode.map Folded
                (Decode.map3
                    (\isFolded summary details -> { id = docId "TODO", isFolded = isFolded, summary = summary, details = details })
                    (field "contents" (index 0 bool))
                    (field "contents" (index 1 nested))
                    (field "contents" (index 3 nested))
                )
            )
        , when tag ((==) "Paragraph") (Decode.map Paragraph (field "contents" (Decode.list nested)))
        , when tag ((==) "BulletedList") (Decode.map BulletedList (field "contents" (Decode.list nested)))
        , when tag
            ((==) "NumberedList")
            (Decode.map2 NumberedList
                (field "contents" (index 0 int))
                (field "contents" (index 1 (Decode.list nested)))
            )
        , when tag
            ((==) "Section")
            (Decode.map2 Section
                (field "contents" (index 0 nested))
                (field "contents" (index 1 (Decode.list nested)))
            )
        , when tag
            ((==) "NamedLink")
            (Decode.map2 NamedLink
                (field "contents" (index 0 nested))
                (field "contents" (index 1 nested))
            )
        , when tag
            ((==) "Image")
            (Decode.map3 Image
                (field "contents" (index 0 nested))
                (field "contents" (index 1 nested))
                (Decode.maybe (field "contents" (index 2 nested)))
            )
        , when tag ((==) "Special") (Decode.map Special (field "contents" decodeSpecialForm))
        , when tag ((==) "Join") (Decode.map Join (field "contents" (Decode.list nested)))
        , when tag ((==) "UntitledSection") (Decode.map UntitledSection (field "contents" (Decode.list nested)))
        , when tag ((==) "Column") (Decode.map Column (field "contents" (Decode.list nested)))
        , when tag ((==) "Group") (Decode.map Group (field "contents" nested))
        ]



-- INTERNAL HELPERS


stringToClass : String -> String
stringToClass s =
    String.replace " " "__" s
