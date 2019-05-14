module CsvTest exposing (suite)

import Csv exposing (parse)
import Csv.Decode exposing (Decoder, Error(..), bool, decode, int, string, float)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Csv"
        [ describe "decode"
            [ decodeTest "decode a string" "\nhello" string (Ok [ "hello" ])
            , decodeTest "decode an int" "\n37" int (Ok [ 37 ])
            , decodeTest "decode an bool: True" "\nTrue" bool (Ok [ True ])
            , decodeTest "decode an bool: false" "\nfalse" bool (Ok [ False ])
            , decodeTest "decode an float" "\n0.5" float (Ok [ 0.5 ])
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
