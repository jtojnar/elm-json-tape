module Codec.JsonTape.Decode exposing (dirTree)

{-| This module provides JSON decoder of a directory tree.

@docs dirTree
-}

import Codec.JsonTape exposing (..)
import Json.Decode exposing (..)


{-| JSON decoder of json-tape format.
-}
dirTree : Decoder DirTree
dirTree =
    ("kind" := string)
        `andThen`
            \kind ->
                case kind of
                    "file" ->
                        object2 fillFile ("name" := string) ("content" := string)

                    "directory" ->
                        object2 fillDir ("name" := string) ("content" := list dirTree)

                    _ ->
                        fail ("Unknown kind: " ++ kind)


fillFile : String -> String -> DirTree
fillFile name content =
    File { name = name, file = content }


fillDir : String -> List DirTree -> DirTree
fillDir name content =
    Dir { name = name, contents = content }
