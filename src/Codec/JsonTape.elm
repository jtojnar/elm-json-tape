module Codec.JsonTape exposing (DirTree(..), getName)

{-| This module describes data structures representing files and directories.

# Data structure
@docs DirTree

# Helpers
@docs getName
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


{-| Auxilliary function for circumventing the type system.
-}
getName : DirTree -> FileName
getName node =
    case node of
        Dir { name } ->
            name

        File { name } ->
            name
