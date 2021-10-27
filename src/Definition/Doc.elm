module Definition.Doc exposing
    ( Doc(..)
    , DocFoldToggles
    , FoldId(..)
    , SpecialForm(..)
    , decode
    , emptyDocFoldToggles
    , isDocFoldToggled
    , mergeWords
    , toString
    , toggleFold
    , view
    )

import Definition.Reference exposing (Reference)
import Definition.Source as Source
import Definition.Term exposing (TermSignature(..))
import Html
    exposing
        ( Attribute
        , Html
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
import Html.Attributes exposing (alt, class, classList, href, id, rel, src, start, target, title)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (bool, field, index, int, string)
import Json.Decode.Extra as DecodeE exposing (when)
import Set exposing (Set)
import Syntax exposing (Syntax)
import TreePath exposing (TreePath)
import UI
import UI.FoldToggle as FoldToggle
import UI.Icon as Icon
import UI.Tooltip as Tooltip


type EmbeddedSource
    = EmbeddedSource Syntax Syntax
    | Builtin Syntax


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
    | Folded { foldId : FoldId, isFolded : Bool, summary : Doc, details : Doc }
      -- Documents separated by spaces and wrapped to available width
    | Span (List Doc)
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
      -- * docA
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
    = Source (List { foldId : FoldId, isFolded : Bool, source : EmbeddedSource })
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



-- FOLDING


type DocFoldToggles
    = DocFoldToggles (Set String)


type FoldId
    = FoldId TreePath


{-| A FoldId present in DocFoldToggles means that the Bool on a Folded or
Source should be negated.

This type is meant to track state on an adjacent level to Doc, for instance on
the WorkspaceItem level.

-}
emptyDocFoldToggles : DocFoldToggles
emptyDocFoldToggles =
    DocFoldToggles Set.empty


toggleFold : DocFoldToggles -> FoldId -> DocFoldToggles
toggleFold (DocFoldToggles toggles) (FoldId path) =
    let
        rawPath =
            TreePath.toString path
    in
    if Set.member rawPath toggles then
        DocFoldToggles (Set.remove rawPath toggles)

    else
        DocFoldToggles (Set.insert rawPath toggles)


isDocFoldToggled : DocFoldToggles -> FoldId -> Bool
isDocFoldToggled (DocFoldToggles toggles) (FoldId path) =
    let
        rawPath =
            TreePath.toString path
    in
    Set.member rawPath toggles


type IsFolded msg
    = IsFolded
        { toggleFoldMsg : FoldId -> msg
        , content : List (Html msg)
        , foldId : FoldId
        , isFolded : Bool
        }
    | Disabled (Html msg)


viewFolded : List (Attribute msg) -> IsFolded msg -> Html msg
viewFolded attrs isFolded_ =
    case isFolded_ of
        Disabled summary ->
            div (class "folded is-folded" :: attrs)
                [ FoldToggle.disabled |> FoldToggle.view
                , div [ class "folded-content" ] [ summary ]
                ]

        IsFolded { toggleFoldMsg, content, foldId, isFolded } ->
            div (classList [ ( "folded", True ), ( "is-folded", isFolded ) ] :: attrs)
                [ FoldToggle.foldToggle (toggleFoldMsg foldId) |> FoldToggle.isOpen (not isFolded) |> FoldToggle.view
                , div [ class "folded-content" ] content
                ]



-- VIEW


type NamedLinkHref
    = Href String
    | ReferenceHref Reference
    | InvalidHref


normalizeHref : NamedLinkHref -> Doc -> NamedLinkHref
normalizeHref href doc =
    case doc of
        Word w ->
            case href of
                InvalidHref ->
                    Href w

                Href h ->
                    Href (h ++ w)

                ReferenceHref _ ->
                    href

        Group d_ ->
            normalizeHref href d_

        Join ds ->
            List.foldl (\d acc -> normalizeHref acc d) href ds

        Special (Link syntax) ->
            let
                folder seg acc =
                    case acc of
                        Nothing ->
                            Syntax.reference seg

                        _ ->
                            acc
            in
            syntax
                |> Syntax.foldl folder Nothing
                |> Maybe.map ReferenceHref
                |> Maybe.withDefault InvalidHref

        _ ->
            href


{-| Merge adjacent Word elements in a list to 1 element with a string of words
separated by space— useful for rendering to the dom without creating dom
elements for each and every word in the doc, but instead rely on textNodes
-}
mergeWords : String -> List Doc -> List Doc
mergeWords sep docs =
    let
        merge_ : Doc -> List Doc -> List Doc
        merge_ d acc =
            case ( d, acc ) of
                ( Word w, (Word w_) :: rest ) ->
                    Word (w ++ sep ++ w_) :: rest

                _ ->
                    d :: acc
    in
    List.foldr merge_ [] docs


{-| Merge down Doc to String by merging Paragraphs and Words.
Used for things like extract an src of an image. I.e something that has to
be a Text and not a Doc
-}
toString : String -> Doc -> String
toString sep doc =
    let
        listToString =
            List.map (toString sep)
                >> List.filter (String.isEmpty >> not)
                >> String.join sep
    in
    case doc of
        Span ds ->
            listToString ds

        Group d ->
            toString sep d

        Join ds ->
            listToString ds

        Bold d ->
            toString sep d

        Italic d ->
            toString sep d

        Strikethrough d ->
            toString sep d

        Blockquote d ->
            toString sep d

        Section d ds ->
            toString sep d ++ sep ++ listToString ds

        UntitledSection ds ->
            listToString ds

        Column ds ->
            listToString ds

        Word w ->
            w

        _ ->
            ""


view : (Reference -> msg) -> (FoldId -> msg) -> DocFoldToggles -> Doc -> Html msg
view refToMsg toggleFoldMsg docFoldToggles document =
    let
        viewSignature =
            Source.viewTermSignature (Source.Rich refToMsg)

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

                viewSectionContent renderer doc_ =
                    case doc_ of
                        Span _ ->
                            p [] [ renderer doc_ ]

                        _ ->
                            renderer doc_
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
                    Tooltip.tooltip
                        (viewAtCurrentSectionLevel triggerContent)
                        (Tooltip.Rich (viewAtCurrentSectionLevel tooltipContent))
                        |> Tooltip.withArrow Tooltip.Start
                        |> Tooltip.view

                Aside d ->
                    span [ class "aside-anchor" ]
                        [ aside [] [ viewAtCurrentSectionLevel d ]
                        ]

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
                            tr [] (List.map viewCell (mergeWords " " cells))
                    in
                    table [] [ tbody [] (List.map viewRow rows) ]

                Folded { foldId, isFolded, summary, details } ->
                    let
                        isFolded_ =
                            if isDocFoldToggled docFoldToggles foldId then
                                not isFolded

                            else
                                isFolded

                        content =
                            if isFolded_ then
                                [ viewAtCurrentSectionLevel summary ]

                            else
                                [ viewAtCurrentSectionLevel summary
                                , viewAtCurrentSectionLevel details
                                ]
                    in
                    viewFolded
                        []
                        (IsFolded
                            { toggleFoldMsg = toggleFoldMsg
                            , content = content
                            , foldId = foldId
                            , isFolded = isFolded_
                            }
                        )

                Span docs ->
                    case docs of
                        [ d ] ->
                            viewAtCurrentSectionLevel d

                        ds ->
                            span [ class "span" ] (List.map viewAtCurrentSectionLevel (mergeWords " " ds))

                BulletedList items ->
                    let
                        viewItem d =
                            li [] [ viewAtCurrentSectionLevel d ]
                    in
                    ul [] (List.map viewItem (mergeWords " " items))

                NumberedList startNum items ->
                    let
                        viewItem d =
                            li [] [ viewAtCurrentSectionLevel d ]
                    in
                    ol [ start startNum ] (List.map viewItem (mergeWords " " items))

                Section title docs ->
                    let
                        -- Unison Doc allows endlessly deep section nesting with
                        -- titles, but HTML only supports to h1-h6, so we clamp
                        -- the sectionLevel when converting
                        level =
                            min 6 sectionLevel

                        titleEl =
                            Html.node ("h" ++ String.fromInt level) [] [ viewAtCurrentSectionLevel title ]
                    in
                    section [] (titleEl :: List.map (viewSectionContent (view_ (sectionLevel + 1))) docs)

                NamedLink label href_ ->
                    case normalizeHref InvalidHref href_ of
                        Href h ->
                            a [ class "named-link", href h, rel "noopener", target "_blank" ] [ viewAtCurrentSectionLevel label ]

                        ReferenceHref ref ->
                            a [ class "named-link", onClick (refToMsg ref) ] [ viewAtCurrentSectionLevel label ]

                        InvalidHref ->
                            span [ class "named-link invalid-href" ] [ viewAtCurrentSectionLevel label ]

                Image altText src_ caption ->
                    let
                        altAttr =
                            [ alt (toString " " altText) ]

                        image =
                            img (altAttr ++ [ src (toString "" src_) ]) []

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
                                viewFoldedSource { foldId, isFolded, source } =
                                    let
                                        isFolded_ =
                                            if isDocFoldToggled docFoldToggles foldId then
                                                not isFolded

                                            else
                                                isFolded

                                        content summary details =
                                            if isFolded_ then
                                                [ UI.codeBlock [] (viewSyntax summary) ]

                                            else
                                                [ UI.codeBlock [] (viewSyntax details) ]
                                    in
                                    case source of
                                        Builtin summary ->
                                            viewFolded
                                                [ class "rich source" ]
                                                (Disabled
                                                    (div
                                                        [ class "builtin-summary" ]
                                                        [ UI.codeBlock [] (viewSyntax summary)
                                                        , UI.badge (span [] [ strong [] [ text "Built-in " ], span [] [ text "provided by the Unison runtime" ] ])
                                                        ]
                                                    )
                                                )

                                        EmbeddedSource summary details ->
                                            viewFolded
                                                [ class "rich source" ]
                                                (IsFolded
                                                    { toggleFoldMsg = toggleFoldMsg
                                                    , content = content summary details
                                                    , foldId = foldId
                                                    , isFolded = isFolded_
                                                    }
                                                )
                            in
                            div [ class "folded-sources" ]
                                (List.map viewFoldedSource sources)

                        Example syntax ->
                            span [ class "source rich example-inline" ] [ UI.inlineCode [] (viewSyntax syntax) ]

                        ExampleBlock syntax ->
                            div [ class "source rich example" ] [ UI.codeBlock [] (viewSyntax syntax) ]

                        Link syntax ->
                            UI.inlineCode [ class "rich source" ] (viewSyntax syntax)

                        Signature signatures ->
                            div [ class "rich source signatures" ]
                                (List.map
                                    (\signature -> div [ class "signature" ] [ viewSignature signature ])
                                    signatures
                                )

                        SignatureInline signature ->
                            span [ class "rich source signature-inline" ] [ viewSignature signature ]

                        Eval source result ->
                            div
                                [ class "source rich eval" ]
                                [ UI.codeBlock [] (div [] [ viewSyntax source, div [ class "result" ] [ Icon.view Icon.arrowDown, div [] [ viewSyntax result ] ] ]) ]

                        EvalInline source result ->
                            span [ class "source rich eval-inline" ]
                                [ UI.inlineCode [] (span [] [ viewSyntax source, span [ class "result" ] [ Icon.view Icon.arrowRight, viewSyntax result ] ]) ]

                        Embed syntax ->
                            div [ class "source rich embed" ] [ UI.codeBlock [] (viewSyntax syntax) ]

                        EmbedInline syntax ->
                            span [ class "source rich embed-inline" ] [ UI.inlineCode [] (viewSyntax syntax) ]

                Join docs ->
                    span [ class "join" ] (List.map viewAtCurrentSectionLevel (mergeWords " " docs))

                UntitledSection docs ->
                    section [] (List.map (viewSectionContent viewAtCurrentSectionLevel) docs)

                Column docs ->
                    ul [ class "column" ]
                        (List.map
                            (\c -> li [] [ viewAtCurrentSectionLevel c ])
                            (mergeWords " " docs)
                        )

                Group content ->
                    span [ class "group" ] [ viewAtCurrentSectionLevel content ]
    in
    article [ class "definition-doc" ] [ view_ 1 document ]



-- DECODE


decodeSpecialForm : TreePath -> Decode.Decoder SpecialForm
decodeSpecialForm path =
    let
        tag =
            field "tag" string

        decodeSignature =
            Decode.map TermSignature Syntax.decode

        decodeUserObject =
            Decode.map2 EmbeddedSource
                (index 0 Syntax.decode)
                (index 1 Syntax.decode)

        decodeBuiltinObject =
            Decode.map Builtin Syntax.decode

        decodeSource_ isFolded =
            Decode.map
                (\source ->
                    { foldId = FoldId path
                    , isFolded = isFolded
                    , source = source
                    }
                )
                (Decode.oneOf
                    [ when tag ((==) "UserObject") (field "contents" decodeUserObject)
                    , when tag ((==) "BuiltinObject") (field "contents" decodeBuiltinObject)
                    ]
                )

        decodeSource isFolded =
            Decode.oneOf
                [ when tag ((==) "Term") (field "contents" (index 1 (decodeSource_ isFolded)))
                , when tag ((==) "Type") (field "contents" (index 1 (decodeSource_ isFolded)))
                ]
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


decode_ : TreePath -> Decode.Decoder Doc
decode_ path =
    let
        tag =
            field "tag" string

        nested_ next =
            Decode.lazy (\_ -> decode_ (path ++ next))

        nested0 =
            nested_ [ TreePath.VariantIndex 0 ]

        nested1 =
            nested_ [ TreePath.VariantIndex 1 ]

        nested2 =
            nested_ [ TreePath.VariantIndex 2 ]

        listWithPath_ next =
            DecodeE.indexedList (\i -> nested_ (next ++ [ TreePath.ListIndex i ]))

        listWithPath0 =
            listWithPath_ [ TreePath.VariantIndex 0 ]

        listWithPath1 =
            listWithPath_ [ TreePath.VariantIndex 1 ]
    in
    Decode.oneOf
        [ when tag ((==) "Word") (Decode.map Word (field "contents" string))
        , when tag ((==) "Code") (Decode.map Code (field "contents" nested0))
        , when tag
            ((==) "CodeBlock")
            (Decode.map2 CodeBlock
                (field "contents" (index 0 string))
                (field "contents" (index 1 nested1))
            )
        , when tag ((==) "Bold") (Decode.map Bold (field "contents" nested0))
        , when tag ((==) "Italic") (Decode.map Italic (field "contents" nested0))
        , when tag ((==) "Strikethrough") (Decode.map Strikethrough (field "contents" nested0))
        , when tag
            ((==) "Style")
            (Decode.map2 Style
                (field "contents" (index 0 string))
                (field "contents" (index 1 nested1))
            )
        , when tag
            ((==) "Anchor")
            (Decode.map2 Anchor
                (field "contents" (index 0 string))
                (field "contents" (index 1 nested1))
            )
        , when tag ((==) "Blockquote") (Decode.map Blockquote (field "contents" nested0))
        , when tag ((==) "Blankline") (Decode.succeed Blankline)
        , when tag ((==) "Linebreak") (Decode.succeed Linebreak)
        , when tag ((==) "SectionBreak") (Decode.succeed SectionBreak)
        , when tag
            ((==) "Tooltip")
            (Decode.map2 Tooltip
                (field "contents" (index 0 nested0))
                (field "contents" (index 1 nested1))
            )
        , when tag ((==) "Aside") (Decode.map Aside (field "contents" nested0))
        , when tag
            ((==) "Callout")
            (Decode.map2 Callout
                (Decode.maybe (field "contents" (index 0 nested0)))
                (field "contents" (index 1 nested1))
            )
        , when tag ((==) "Table") (Decode.map Table (field "contents" (Decode.list listWithPath0)))
        , when tag
            ((==) "Folded")
            (Decode.map Folded
                (Decode.map3
                    (\isFolded summary details -> { isFolded = isFolded, summary = summary, details = details, foldId = FoldId path })
                    (field "contents" (index 0 bool))
                    (field "contents" (index 1 nested1))
                    (field "contents" (index 2 nested2))
                )
            )

        -- Unison Doc Paragraph are not like a paragraph in the typesetting or html way; it's more like a bag of words
        , when tag ((==) "Paragraph") (Decode.map Span (field "contents" listWithPath0))
        , when tag ((==) "BulletedList") (Decode.map BulletedList (field "contents" listWithPath0))
        , when tag
            ((==) "NumberedList")
            (Decode.map2 NumberedList
                (field "contents" (index 0 int))
                (field "contents" (index 1 listWithPath1))
            )
        , when tag
            ((==) "Section")
            (Decode.map2 Section
                (field "contents" (index 0 nested0))
                (field "contents" (index 1 listWithPath1))
            )
        , when tag
            ((==) "NamedLink")
            (Decode.map2 NamedLink
                (field "contents" (index 0 nested0))
                (field "contents" (index 1 nested1))
            )
        , when tag
            ((==) "Image")
            (Decode.map3 Image
                (field "contents" (index 0 nested0))
                (field "contents" (index 1 nested1))
                (Decode.maybe (field "contents" (index 2 nested2)))
            )
        , when tag ((==) "Special") (Decode.map Special (field "contents" (decodeSpecialForm (path ++ [ TreePath.VariantIndex 0 ]))))
        , when tag ((==) "Join") (Decode.map Join (field "contents" listWithPath0))
        , when tag ((==) "UntitledSection") (Decode.map UntitledSection (field "contents" listWithPath0))
        , when tag ((==) "Column") (Decode.map Column (field "contents" listWithPath0))
        , when tag ((==) "Group") (Decode.map Group (field "contents" nested1))
        ]


decode : Decode.Decoder Doc
decode =
    decode_ []



-- INTERNAL HELPERS


stringToClass : String -> String
stringToClass s =
    String.replace " " "__" s
