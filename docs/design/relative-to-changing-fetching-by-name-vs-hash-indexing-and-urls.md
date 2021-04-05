RelativeTo changing, fetching by name vs hash, indexing, and URLs
=================================================================

Context
-------

The UI can be relative to a specific namespace, either the top level namespace—
this doesn't have a name, so we'll refer to it as the "codebase"— or any
namespace on any level in the codebase graph. 

This document aims to model this and it's impact on definition fetching,
tracking, and serialization to the URL.

These are the main questions and concerns that prompted this document:

* When changing the `RelativeTo` namespace, what should happen to
  `OpenDefinitions` (the data structure that holds all definitions open within
  the workspace)? All names inside rendered sources would need to be
  re-rendered and the named identifier (from URL) would no longer be accurate?
  Should we re-fetch and re-render, clear out the workspace, something else?

* When fetching something by name, we won't know until after its finished
  fetching that we might already have it open by hash?  We could always check
  open definitions by hash before fetching since we'll have the hash for
  everything, but the first request from the URL, perhaps that should be
  special cased?

* How do we best index such that we can allow checking if something is fetched
  already by hash and also allow fetching something for the first time just by
  a name? And at the same time support name collisions that require both `Hash`
  and `FQN`? There's a difference between an `HashQualified` used to fully identify
  a `Definition` and having both a hash and a name for a thing. Do we need to
  keep track of them separately?

* `HashQualified` normally operates on an `FQN` for the `NameOnly` variant, but
  the name of an open definition might not be a complete `FQN` as it is
  relative to a namespace, thus we have to use some very akin to
  `HashQualified`, but not it exactly. We need to keep track of what
  a definition is relative to.

* What about terms vs types? Right now they are variants of a `Definition`, but
  we want them to be distinct types. References should be indicative of which
  kind of definition they are referring to as it will be needed in the URL and
  we'll want to further separate rendering of terms and types (and potentially
  other things like patches, docs, tests etc.).

Decision
--------

We will add a new type: `RelativeTo`, that is used as a top level indicator of
what the UI is honed in on; either as the full `Codebase` or for a specific
`Namespace`:

```elm
type RelativeTo
    = CodeBase
    | Namespace Hash
```

When this `RelativeTo` changes, we'll open a new tab/window with the
`RelativeTo` as part of the URL. If there are no open definitions at the time
of changing `RelativeTo`, we change it in the same tab/window.

We explored re-fetching open definitions, adding `RelativeTo` annotations, and
clearing out the open definitions when changing `RelativeTo`, but opted for the
simpler flow of opening a new tab/window.

**Definitions: Terms and Types**

`Definition` will be retired in favor of `Term` and `Type` types. These types
will be configurable with an `Info` record and an extra field for varying
amounts of data:

```elm
type alias Info = { hash : Hash, name : String, otherNames : List FQN }

type Term a = Term Info a
type Type a = Type Info a

type alias TermDetail = Term TermSource
type alias TypeDetail = Type TypeSource
```

This will support better distinction of things that can be opened in the
workspace and how they are represented in the URL.

**Workspace**

A new `WorkplaceItems` type (replacing `OpenDefinitions`) through new
`Reference` and `ReferencedItem` types will support rendering definitions in
the workspace:

```elm
type HashQualified
    = NameOnly FQN
    | HashOnly Hash
    | HashQualified FQN Hash

type Reference 
  = TermReference HashQualified
  | TypeReference HashQualified

type Item 
  = TermItem Term 
  | TypeItem Type

type WorkspaceItem
  = Loading Reference
  | Failure Reference Error
  | Success Reference Item

type WorkspaceItems = 
  WorkspaceItems 
    { before : List WorkspaceItem
    , focus : WorkspaceItem
    , after : List WorkspaceItem
    }
```

When a term or type is opened, a `WorkspaceItem` will be added to
`WorkspaceItems` for the appropriate item type (`TermItem` or `TypeItem`) with
a `Reference` (typically `NameOnly`). When the definition has been fetched the
`WorkspaceItem` will be updated to include the data. 
Afterwards we'll de-duplicate items from `WorkspaceItems` (say we fetched by
`HashOnly` and had previously fetched by `NameOnly` and this didn't have enough
information to verify if a definition was previously fetched).

**Finder and Sidebar**

`Finder` and `NamespaceListings` (sidebar) will be updated to support these new
types in a configuration that makes sense for their level of data:

```elm
type alias TermSummary = Term TermSignature
type alias TypeSummary = Type TypeSource

type alias TermListing = Term ()
type alias TypeListing = Type ()
```

**Routing**

Routing will be updated to use the new `Reference` type to more strongly bind
references to their type:

```elm
type Route
    = Namespace RelativeTo
    | ByReference RelativeTo Reference
```
