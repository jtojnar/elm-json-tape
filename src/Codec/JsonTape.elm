module Codec.JsonTape exposing (DirTree(..), getName, getObject, readFileRaw, readFile)

{-| This module describes data structures representing files and directories and functions for manipulating them.

# Data structure
@docs DirTree

# File manipulation
@docs getObject, readFileRaw, readFile

# Helpers
@docs getName
-}

import Base64
import String


type alias FileName =
    String


type alias Path =
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



-- FILE MANIPULATION


{-| Traverse the directory tree along path and return the seeked object.
-}
getObject : Path -> DirTree -> Result String DirTree
getObject path tree =
    get (String.split "/" path) [ tree ]


{-| Traverse the directory tree along path and return the content of the found file.
-}
readFileRaw : Path -> DirTree -> Result String String
readFileRaw path tree =
    getObject path tree
        `Result.andThen`
            (\obj ->
                case obj of
                    Dir _ ->
                        Err "Provided path is a directory."

                    File { file } ->
                        Ok file
            )


{-| Traverse the directory tree along path and return the content of the found file after base64 decoding it.
-}
readFile : Path -> DirTree -> Result String String
readFile path tree =
    readFileRaw path tree `Result.andThen` Base64.decode



-- HELPERS


{-| Auxilliary function that traverses the directory tree using path segments.
-}
get : List FileName -> List DirTree -> Result String DirTree
get nav level =
    case nav of
        cur :: rest ->
            case find cur level of
                Just node ->
                    if List.isEmpty rest then
                        Ok node
                    else
                        get rest (getChildren node)

                Nothing ->
                    Err "There is no such path in the tree."

        [] ->
            List.head level |> Result.fromMaybe "This should not happen"


{-| Auxilliary functions that searches for a file inside a list of files.
-}
find : FileName -> List DirTree -> Maybe DirTree
find searchedName contents =
    List.head
        (List.filter
            (\f -> getName f == searchedName)
            contents
        )


{-| Auxilliary function for circumventing the type system.
-}
getName : DirTree -> FileName
getName node =
    case node of
        Dir { name } ->
            name

        File { name } ->
            name


{-| Auxilliary function for getting children of the node; files have no children.
-}
getChildren : DirTree -> List DirTree
getChildren node =
    case node of
        Dir { contents } ->
            contents

        File _ ->
            []
