module CsvTest exposing (suite)

import Csv exposing (parse)
import Csv.Decode exposing (Error(..), decode, string)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Csv"
        [ describe "decode"
            [ test "a string" <|
                \_ ->
                    let
                        actual =
                            "\nhello,world"
                                |> Csv.parse
                                |> Result.mapError (\_ -> CsvParseError)
                                |> Result.andThen (decode string)

                        expected =
                            Ok [ "hello" ]
                    in
                    Expect.equal expected actual
            ]
        ]
