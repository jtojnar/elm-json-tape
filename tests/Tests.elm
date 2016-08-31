module Tests exposing (..)

import Test exposing (..)
import Expect
import Codec.JsonTape exposing (..)
import Codec.JsonTape.Decode as DecodeTape
import Codec.JsonTape.Encode as EncodeTape
import Json.Decode as Json
import Json.Encode exposing (..)


all : Test
all =
    concat
        [ describe "Serialisation and deserialisation"
            [ test "encoding of tree" <|
                \() ->
                    (EncodeTape.dirTree sampleTree) |> Expect.equal sampleJsonParsed
            , test "decoding of json-tape" <|
                \() ->
                    (Json.decodeString DecodeTape.dirTree sampleJson |> Result.map normaliseTree) |> Expect.equal (Ok sampleTree)
            , test "decoding of invalid json-tape" <|
                \() ->
                    (Json.decodeString DecodeTape.dirTree sampleJsonBadKind) |> Expect.equal (Err "I ran into a `fail` decoder: Unknown kind: badkind")
            ]
        , describe "Directory tree manipulation"
            [ describe "Getting a tree object"
                [ test "basic file object" <|
                    \() ->
                        (getObject "antigravity-0.1/PKG-INFO" sampleTree) |> Expect.equal (Ok sampleFileObject)
                , test "basic directory object" <|
                    \() ->
                        (getObject "antigravity-0.1/antigravity" sampleTree) |> Expect.equal (Ok sampleDirectoryObject)
                , test "reading non-existing path" <|
                    \() ->
                        (getObject "antigravity-0.1/foobar" sampleTree) |> Expect.equal (Err "There is no such path in the tree.")
                , test "attempting to descend through file" <|
                    \() ->
                        (getObject "antigravity-0.1/PKG-INFO/baz" sampleTree) |> Expect.equal (Err "There is no such path in the tree.")
                , test "traversing a deep tree" <|
                    \() ->
                        (getObject "a/b/c/d/e/f" deepTree) |> Expect.equal (Ok deepObject)
                ]
            , describe "Reading a file"
                [ test "basic reading" <|
                    \() ->
                        (readFileRaw "antigravity-0.1/PKG-INFO" sampleTree) |> Expect.equal (Ok sampleFileContent)
                , test "reading a directory" <|
                    \() ->
                        (readFileRaw "antigravity-0.1/antigravity" sampleTree) |> Expect.equal (Err "Provided path is a directory.")
                , test "reading non-existing path" <|
                    \() ->
                        (readFileRaw "antigravity-0.1/foobar" sampleTree) |> Expect.equal (Err "There is no such path in the tree.")
                , test "attempting to descend through file" <|
                    \() ->
                        (readFileRaw "antigravity-0.1/PKG-INFO/baz" sampleTree) |> Expect.equal (Err "There is no such path in the tree.")
                ]
            ]
        ]



-- HELPERS


{-| Sort contents of the directories by name.
-}
normaliseTree : DirTree -> DirTree
normaliseTree node =
    case node of
        File _ ->
            node

        Dir { name, contents } ->
            Dir { name = name, contents = List.sortBy getName contents |> List.map normaliseTree }



-- TEST DATA


sampleTree : DirTree
sampleTree =
    Dir
        { name = "antigravity-0.1"
        , contents =
            [ sampleFileObject
            , sampleDirectoryObject
            , File
                { name = "setup.py"
                , file =
                    "IyEvdXNyL2Jpbi9lbnYgcHl0aG9uDQoNCmZyb20gZGlzdHV0aWxzLmNvcmUgaW1wb3J0IHNldHVwDQoNCnNldHVwKA0KICAgIG5hbWU9J2FudGlncmF2aXR5JywNCiAgICB2ZXJzaW9uPScwLjEnLA0KICAgIGRlc2NyaXB0aW9uPSdBIHJlYWxseSBzaW1wbGUgbW9kdWxlIHRoYXQgYWxsb3cgZXZlcnlvbmUgdG8gZG8gImltcG9ydCBhbnRpZ3Jhdml0eSInLA0KICAgIGF1dGhvcj0nRmFiaWVuIFNjaHdvYicsDQogICAgYXV0aG9yX2VtYWlsPSdhbnRpZ3Jhdml0eUB4LXBodXR1cmUuY29tJywNCiAgICB1cmw9J2h0dHA6Ly9mYWJpZW4uc2Nod29iLm9yZy9hbnRpZ3Jhdml0eS8nLA0KICAgIHBhY2thZ2VzPVsnYW50aWdyYXZpdHknXSwNCik="
                }
            ]
        }


sampleFileContent : String
sampleFileContent =
    "TWV0YWRhdGEtVmVyc2lvbjogMS4wDQpOYW1lOiBhbnRpZ3Jhdml0eQ0KVmVyc2lvbjogMC4xDQpTdW1tYXJ5OiBBIHJlYWxseSBzaW1wbGUgbW9kdWxlIHRoYXQgYWxsb3cgZXZlcnlvbmUgdG8gZG8gImltcG9ydCBhbnRpZ3Jhdml0eSINCkhvbWUtcGFnZTogaHR0cDovL2ZhYmllbi5zY2h3b2Iub3JnL2FudGlncmF2aXR5Lw0KQXV0aG9yOiBGYWJpZW4gU2Nod29iDQpBdXRob3ItZW1haWw6IGFudGlncmF2aXR5QHgtcGh1dHVyZS5jb20NCkxpY2Vuc2U6IFVOS05PV04NCkRlc2NyaXB0aW9uOiBVTktOT1dODQpQbGF0Zm9ybTogVU5LTk9XTg0K"


sampleFileObject : DirTree
sampleFileObject =
    File
        { name = "PKG-INFO"
        , file = sampleFileContent
        }


sampleDirectoryObject : DirTree
sampleDirectoryObject =
    Dir
        { name = "antigravity"
        , contents =
            [ File
                { name = "__init__.py"
                , file =
                    "IyEvdXNyL2Jpbi9lbnYgcHl0aG9uDQoNClNUUklQX1VSTCA9ICJodHRwOi8veGtjZC5jb20vMzUzLyINCg0KZGVmIHN0YXJ0KCk6DQogICAgcmV0dXJuIFNUUklQX1VSTA=="
                }
            ]
        }


deepTree : DirTree
deepTree =
    Dir
        { name = "a"
        , contents =
            [ Dir
                { name = "b"
                , contents =
                    [ Dir
                        { name = "c"
                        , contents =
                            [ Dir
                                { name = "d"
                                , contents =
                                    [ Dir
                                        { name = "e"
                                        , contents =
                                            [ deepObject
                                            ]
                                        }
                                    ]
                                }
                            ]
                        }
                    ]
                }
            ]
        }


deepObject : DirTree
deepObject =
    File
        { name = "f"
        , file =
            "baz"
        }


sampleJson : String
sampleJson =
    "{\"kind\":\"directory\",\"content\":[{\"kind\":\"file\",\"content\":\"TWV0YWRhdGEtVmVyc2lvbjogMS4wDQpOYW1lOiBhbnRpZ3Jhdml0eQ0KVmVyc2lvbjogMC4xDQpTdW1tYXJ5OiBBIHJlYWxseSBzaW1wbGUgbW9kdWxlIHRoYXQgYWxsb3cgZXZlcnlvbmUgdG8gZG8gImltcG9ydCBhbnRpZ3Jhdml0eSINCkhvbWUtcGFnZTogaHR0cDovL2ZhYmllbi5zY2h3b2Iub3JnL2FudGlncmF2aXR5Lw0KQXV0aG9yOiBGYWJpZW4gU2Nod29iDQpBdXRob3ItZW1haWw6IGFudGlncmF2aXR5QHgtcGh1dHVyZS5jb20NCkxpY2Vuc2U6IFVOS05PV04NCkRlc2NyaXB0aW9uOiBVTktOT1dODQpQbGF0Zm9ybTogVU5LTk9XTg0K\",\"name\":\"PKG-INFO\"},{\"kind\":\"file\",\"content\":\"IyEvdXNyL2Jpbi9lbnYgcHl0aG9uDQoNCmZyb20gZGlzdHV0aWxzLmNvcmUgaW1wb3J0IHNldHVwDQoNCnNldHVwKA0KICAgIG5hbWU9J2FudGlncmF2aXR5JywNCiAgICB2ZXJzaW9uPScwLjEnLA0KICAgIGRlc2NyaXB0aW9uPSdBIHJlYWxseSBzaW1wbGUgbW9kdWxlIHRoYXQgYWxsb3cgZXZlcnlvbmUgdG8gZG8gImltcG9ydCBhbnRpZ3Jhdml0eSInLA0KICAgIGF1dGhvcj0nRmFiaWVuIFNjaHdvYicsDQogICAgYXV0aG9yX2VtYWlsPSdhbnRpZ3Jhdml0eUB4LXBodXR1cmUuY29tJywNCiAgICB1cmw9J2h0dHA6Ly9mYWJpZW4uc2Nod29iLm9yZy9hbnRpZ3Jhdml0eS8nLA0KICAgIHBhY2thZ2VzPVsnYW50aWdyYXZpdHknXSwNCik=\",\"name\":\"setup.py\"},{\"kind\":\"directory\",\"content\":[{\"kind\":\"file\",\"content\":\"IyEvdXNyL2Jpbi9lbnYgcHl0aG9uDQoNClNUUklQX1VSTCA9ICJodHRwOi8veGtjZC5jb20vMzUzLyINCg0KZGVmIHN0YXJ0KCk6DQogICAgcmV0dXJuIFNUUklQX1VSTA==\",\"name\":\"__init__.py\"}],\"name\":\"antigravity\"}],\"name\":\"antigravity-0.1\"}"


sampleJsonParsed : Value
sampleJsonParsed =
    object
        [ ( "kind", string "directory" )
        , ( "name", string "antigravity-0.1" )
        , ( "content"
          , list
                [ object
                    [ ( "kind", string "file" )
                    , ( "name", string "PKG-INFO" )
                    , ( "content", string "TWV0YWRhdGEtVmVyc2lvbjogMS4wDQpOYW1lOiBhbnRpZ3Jhdml0eQ0KVmVyc2lvbjogMC4xDQpTdW1tYXJ5OiBBIHJlYWxseSBzaW1wbGUgbW9kdWxlIHRoYXQgYWxsb3cgZXZlcnlvbmUgdG8gZG8gImltcG9ydCBhbnRpZ3Jhdml0eSINCkhvbWUtcGFnZTogaHR0cDovL2ZhYmllbi5zY2h3b2Iub3JnL2FudGlncmF2aXR5Lw0KQXV0aG9yOiBGYWJpZW4gU2Nod29iDQpBdXRob3ItZW1haWw6IGFudGlncmF2aXR5QHgtcGh1dHVyZS5jb20NCkxpY2Vuc2U6IFVOS05PV04NCkRlc2NyaXB0aW9uOiBVTktOT1dODQpQbGF0Zm9ybTogVU5LTk9XTg0K" )
                    ]
                , object
                    [ ( "kind", string "directory" )
                    , ( "name", string "antigravity" )
                    , ( "content"
                      , list
                            [ object
                                [ ( "kind", string "file" )
                                , ( "name", string "__init__.py" )
                                , ( "content", string "IyEvdXNyL2Jpbi9lbnYgcHl0aG9uDQoNClNUUklQX1VSTCA9ICJodHRwOi8veGtjZC5jb20vMzUzLyINCg0KZGVmIHN0YXJ0KCk6DQogICAgcmV0dXJuIFNUUklQX1VSTA==" )
                                ]
                            ]
                      )
                    ]
                , object
                    [ ( "kind", string "file" )
                    , ( "name", string "setup.py" )
                    , ( "content", string "IyEvdXNyL2Jpbi9lbnYgcHl0aG9uDQoNCmZyb20gZGlzdHV0aWxzLmNvcmUgaW1wb3J0IHNldHVwDQoNCnNldHVwKA0KICAgIG5hbWU9J2FudGlncmF2aXR5JywNCiAgICB2ZXJzaW9uPScwLjEnLA0KICAgIGRlc2NyaXB0aW9uPSdBIHJlYWxseSBzaW1wbGUgbW9kdWxlIHRoYXQgYWxsb3cgZXZlcnlvbmUgdG8gZG8gImltcG9ydCBhbnRpZ3Jhdml0eSInLA0KICAgIGF1dGhvcj0nRmFiaWVuIFNjaHdvYicsDQogICAgYXV0aG9yX2VtYWlsPSdhbnRpZ3Jhdml0eUB4LXBodXR1cmUuY29tJywNCiAgICB1cmw9J2h0dHA6Ly9mYWJpZW4uc2Nod29iLm9yZy9hbnRpZ3Jhdml0eS8nLA0KICAgIHBhY2thZ2VzPVsnYW50aWdyYXZpdHknXSwNCik=" )
                    ]
                ]
          )
        ]


sampleJsonBadKind : String
sampleJsonBadKind =
    "{\"kind\":\"badkind\",\"content\":\"bar\",\"name\":\"foo\"}"
