{--

  TreePath
  ========
    
  Simple type for keeping tracking of a path down a tree of variants that
  could have lists as data. Definition.Doc is the main example of a tree
  structure that uses TreePath.

--}


module TreePath exposing (TreePath, TreePathItem(..), toString)


type TreePathItem
    = VariantIndex Int
    | ListIndex Int


type alias TreePath =
    List TreePathItem


toString : TreePath -> String
toString path =
    let
        pathItemToString item =
            case item of
                VariantIndex i ->
                    "VariantIndex#" ++ String.fromInt i

                ListIndex i ->
                    "ListIndex#" ++ String.fromInt i
    in
    path |> List.map pathItemToString |> String.join "."
