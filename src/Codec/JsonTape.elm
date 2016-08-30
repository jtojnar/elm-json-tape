module Codec.JsonTape exposing (DirTree(..))

{-| This module describes data structures representing files and directories.

@docs DirTree
-}


type alias FileName =
    String


{-| Data structure representing a directory tree.
-}
type DirTree
    = Dir
        { name : FileName
        , contents : List DirTree
        }
    | File
        { name : FileName
        , file : String
        }
