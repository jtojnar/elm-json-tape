module Codec.JsonTape.Encode exposing (dirTree)

{-| This module provides JSON encoder of a directory tree.

@docs dirTree
-}

import Codec.JsonTape exposing (..)
import Json.Encode exposing (..)


{-| JSON encoder to json-tape format.
-}
dirTree : DirTree -> Value
dirTree node =
    case node of
        File { name, file } ->
            object
                [ ( "kind", string "file" )
                , ( "name", string name )
                , ( "content", string file )
                ]

        Dir { name, contents } ->
            object
                [ ( "kind", string "directory" )
                , ( "name", string name )
                , ( "content", list (List.map dirTree contents) )
                ]
