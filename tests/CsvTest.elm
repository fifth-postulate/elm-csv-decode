module CsvTest exposing (suite)

import Csv exposing (parse)
import Csv.Decode exposing (Decoder, Error(..), bool, decode, float, int, map, string, succeed, fail)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Csv"
        [ describe "decode"
            [ decodeTest "decode a string" "\nhello" string (Ok [ "hello" ])
            , decodeTest "decode an int" "\n37" int (Ok [ 37 ])
            , decodeTest "decode a bool: True" "\nTrue" bool (Ok [ True ])
            , decodeTest "decode a bool: false" "\nfalse" bool (Ok [ False ])
            , decodeTest "decode a float" "\n0.5" float (Ok [ 0.5 ])
            , decodeTest "map a string" "\nhello" (map String.length string) (Ok [ 5 ])
            , decodeTest "succeed with value" "\nwhat,ever" (succeed True) (Ok [ True ])
            , decodeTest "fail with reason" "\nwhat,ever" (fail "Just a test") (Err <| MultipleErrors [(0, FailWithReason "Just a test")])
            ]
        ]


decodeTest : String -> String -> Decoder a -> Result Error (List a) -> Test
decodeTest description input decoder expected =
    test description <|
        \_ ->
            let
                actual =
                    input
                        |> parse
                        |> Result.mapError (\_ -> CsvParseError)
                        |> Result.andThen (decode decoder)
            in
            Expect.equal expected actual
