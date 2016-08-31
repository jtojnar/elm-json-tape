module Tests exposing (..)

import Test exposing (..)
import Expect
import String
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
            [ File
                { name = "PKG-INFO"
                , file =
                    "TWV0YWRhdGEtVmVyc2lvbjogMS4wDQpOYW1lOiBhbnRpZ3Jhdml0eQ0KVmVyc2lvbjogMC4xDQpTdW1tYXJ5OiBBIHJlYWxseSBzaW1wbGUgbW9kdWxlIHRoYXQgYWxsb3cgZXZlcnlvbmUgdG8gZG8gImltcG9ydCBhbnRpZ3Jhdml0eSINCkhvbWUtcGFnZTogaHR0cDovL2ZhYmllbi5zY2h3b2Iub3JnL2FudGlncmF2aXR5Lw0KQXV0aG9yOiBGYWJpZW4gU2Nod29iDQpBdXRob3ItZW1haWw6IGFudGlncmF2aXR5QHgtcGh1dHVyZS5jb20NCkxpY2Vuc2U6IFVOS05PV04NCkRlc2NyaXB0aW9uOiBVTktOT1dODQpQbGF0Zm9ybTogVU5LTk9XTg0K"
                }
            , Dir
                { name = "antigravity"
                , contents =
                    [ File
                        { name = "__init__.py"
                        , file =
                            "IyEvdXNyL2Jpbi9lbnYgcHl0aG9uDQoNClNUUklQX1VSTCA9ICJodHRwOi8veGtjZC5jb20vMzUzLyINCg0KZGVmIHN0YXJ0KCk6DQogICAgcmV0dXJuIFNUUklQX1VSTA=="
                        }
                    ]
                }
            , File
                { name = "setup.py"
                , file =
                    "IyEvdXNyL2Jpbi9lbnYgcHl0aG9uDQoNCmZyb20gZGlzdHV0aWxzLmNvcmUgaW1wb3J0IHNldHVwDQoNCnNldHVwKA0KICAgIG5hbWU9J2FudGlncmF2aXR5JywNCiAgICB2ZXJzaW9uPScwLjEnLA0KICAgIGRlc2NyaXB0aW9uPSdBIHJlYWxseSBzaW1wbGUgbW9kdWxlIHRoYXQgYWxsb3cgZXZlcnlvbmUgdG8gZG8gImltcG9ydCBhbnRpZ3Jhdml0eSInLA0KICAgIGF1dGhvcj0nRmFiaWVuIFNjaHdvYicsDQogICAgYXV0aG9yX2VtYWlsPSdhbnRpZ3Jhdml0eUB4LXBodXR1cmUuY29tJywNCiAgICB1cmw9J2h0dHA6Ly9mYWJpZW4uc2Nod29iLm9yZy9hbnRpZ3Jhdml0eS8nLA0KICAgIHBhY2thZ2VzPVsnYW50aWdyYXZpdHknXSwNCik="
                }
            ]
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
