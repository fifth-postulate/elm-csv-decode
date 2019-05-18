module CsvTest exposing (suite)

import Csv exposing (parse)
import Csv.Decode exposing (Decoder, Error(..), bool, decode, fail, float, int, map, map2, map3, maybe, oneOf, string, succeed)
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
            , decodeTest "fail with reason" "\nwhat,ever" (fail "Just a test") (Err <| MultipleErrors [ ( 0, FailWithReason "Just a test" ) ])
            , decodeTest "oneOf int" "\nnot a number" (oneOf [ int, succeed 51 ]) (Ok [ 51 ])
            , decodeTest "maybe int: Nothing" "\n,3435" (maybe int) (Ok [ Nothing ])
            , decodeTest "maybe int: Just" "\n28,3435" (maybe int) (Ok [ Just 28 ])
            , decodeTest "map2 a Point" "\n3,4" (map2 Point float float) (Ok [ Point 3 4 ])
            , decodeTest "map3 a Person" "\nTom,42,1.8" (map3 Person string int float) (Ok [ Person "Tom" 42 1.8 ])
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


type alias Point =
    { x : Float
    , y : Float
    }


type alias Person =
    { name : String, age : Int, height : Float }
