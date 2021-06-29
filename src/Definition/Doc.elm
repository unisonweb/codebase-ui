module Definition.Doc exposing
    ( Doc(..)
    , DocFoldToggles
    , SpecialForm(..)
    , emptyDocFoldToggles
    , isDocFoldToggled
    , toggleFold
    , view
    )

import Definition.Reference as Reference exposing (Reference)
import Definition.Source as Source exposing (Source)
import Definition.Term exposing (TermSignature)
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
    -- @source{type Maybe, List.map @ note1 note2} OR
    -- The notes are ignored currently, but will later be used to produce
    -- rich annotated source code with tooltips, highlights and whatnot.
    = Source (List ( Reference, Source ))
      -- like Source, but the code starts out folded
    | FoldedSource (List { id : Id Doc, ref : Reference, isFolded : Bool, summary : Syntax, details : Source })
      -- In `Example n expr`, `n` is the number of lambda parameters
      -- that should be elided during display.
      -- Ex: `Example 2 '(x y -> foo x y)` should render as `foo x y`.
      -- Ex: `Example 0 '(1 + 1)` should render as `42`.
      -- In the frontend we dont have
    | Example Syntax
      -- Same as `Example`, but as a block rather than inline element
    | ExampleBlock Syntax
      -- {type Maybe} or {List.map}
    | Link Reference
      -- @signatures{List.map, List.filter, List.foldLeft}
    | Signature (List ( Reference, TermSignature )) --div
      -- @signature{List.map}
    | SignatureInline ( Reference, TermSignature ) -- span
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
FoldedSource
should be negated.

This type is meant to track state on an adjacent level to Doc, for instance on
the WorkspaceItem level.

-}
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

                Code d ->
                    span [ class "inline-code" ] [ viewAtCurrentSectionLevel d ]

                CodeBlock lang d ->
                    div [ class "code", lang |> stringToClass |> class ] [ viewAtCurrentSectionLevel d ]

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

                        Special s ->
                            case s of
                                Link ref ->
                                    a [ onClick (refToMsg ref) ] [ viewAtCurrentSectionLevel label ]

                                _ ->
                                    span [] [ viewAtCurrentSectionLevel label ]

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
                            div [ class "sources" ] (List.map (\( _, s ) -> viewSource s) sources)

                        FoldedSource sources ->
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

                        Link ref ->
                            a [ onClick (refToMsg ref) ] [ text (Reference.toHumanString ref) ]

                        Signature signatures ->
                            div [ class "signatures" ] (List.map (\( _, s ) -> div [ class "signature" ] [ viewSignature s ]) signatures)

                        SignatureInline ( _, signature ) ->
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



-- INTERNAL HELPERS


stringToClass : String -> String
stringToClass s =
    String.replace " " "__" s
