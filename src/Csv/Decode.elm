module Csv.Decode exposing
    ( Decoder, Csv, Error(..), Kind(..)
    , string, int, float, bool
    , decode
    , map
    , succeed, fail, oneOf
    )

{-| Turn CSV Values into Elm values. Inspired by [`elm/json`][elm-json], so make sure to check out this [intro to
JSON decoders][guide] to get a feel for how this library works!

[guide]: https://guide.elm-lang.org/effects/json.html
[elm-json]: https://package.elm-lang.org/packages/elm/json/latest/

Note this library does not include an underlying CSV parser. It assumes you are using something like [`periodic/elm-csv`][periodic] or [`lovasoa/elm-csv`][lovasoa] to get from `String` to `Csv`, where `Csv` is:

    type alias Csv =
        { headers : List String
        , records : List (List String)
        }

This library gets you the rest of the way, to a list of your own types.

[periodic]: https://package.elm-lang.org/packages/periodic/elm-csv/latest/
[lovasoa]: https://package.elm-lang.org/packages/lovasoa/elm-csv/latest/


# Types

@docs Decoder, Csv, Error, Kind


# Primitives

@docs string, int, float, bool


# Run Decoders

@docs decode


# Mapping

**Note:** If you run out of map functions, take a look at `Csv.Decode.Pipeline`
which makes it easier to handle large objects, but produces lower quality type
errors.

@docs map


# Fancy Decoding

@docs succeed, fail, maybe 

-}


{-| The raw CSV data structure.
-}
type alias Csv =
    { headers : List String
    , records : List (List String)
    }


{-| A value that knows how to decode CSV values.
-}
type Decoder a
    = Decoder (List String -> Result Error a)


{-| A structured error describing exactly how the decoder failed. You can use
this to create more elaborate visualizations of a decoder problem. For example,
you could show the entire CSV record and show the part causing the failure in
red.
-}
type Error
    = UnwrapErrorProblem
    | CsvParseError
    | Not Kind
    | FailWithReason String
    | InsufficientFields
    | NonApply
    | MultipleErrors (List ( Int, Error ))


{-| Kind determines what a decoder expects. Used in combination with the `Not` error.
-}
type Kind
    = AString
    | ABool
    | AInt
    | AFloat


{-| Decode the given `Csv` into a custom value by running `Decoder` on it.
This will fail if any of the records can not be decoded by the `Decoder` for some reason.
-}
decode : Decoder a -> Csv -> Result Error (List a)
decode (Decoder mapper) { records } =
    records
        |> List.map mapper
        |> gather


gather : List (Result Error a) -> Result Error (List a)
gather results =
    let
        split ( index, result ) ( errors, vs ) =
            case result of
                Ok v ->
                    ( errors, v :: vs )

                Err error ->
                    ( ( index, error ) :: errors, vs )

        ( indexedErrors, values ) =
            results
                |> List.indexedMap Tuple.pair
                |> List.foldr split ( [], [] )
    in
    if List.isEmpty indexedErrors then
        values
            |> List.reverse
            |> Ok

    else
        indexedErrors
            |> List.reverse
            |> MultipleErrors
            |> Err


isError : Result e t -> Bool
isError result =
    result
        |> Result.map (\_ -> True)
        |> Result.withDefault False


{-| Decode a CSV string into an Elm `String`.

    decodeString string "true"              == Err ...
    decodeString string "42"                == Err ...
    decodeString string "3.14"              == Err ...
    decodeString string "\"hello\""         == Ok "hello"
    decodeString string "{ \"hello\": 42 }" == Err ...

-}
string : Decoder String
string =
    decodeWith (Just << identity) (Not AString)


{-| Decode a CSV boolean into an Elm `Bool`.

    decodeString bool "true"              == Ok True
    decodeString bool "42"                == Err ...
    decodeString bool "3.14"              == Err ...
    decodeString bool "\"hello\""         == Err ...
    decodeString bool "{ \"hello\": 42 }" == Err ...

-}
bool : Decoder Bool
bool =
    decodeWith stringToBool (Not ABool)


stringToBool : String -> Maybe Bool
stringToBool input =
    case input of
        "True" ->
            Just True

        "true" ->
            Just True

        "False" ->
            Just False

        "false" ->
            Just False

        _ ->
            Nothing


{-| Decode a CSV number into an Elm `Int`.

    decodeString int "true"              == Err ...
    decodeString int "42"                == Ok 42
    decodeString int "3.14"              == Err ...
    decodeString int "\"hello\""         == Err ...
    decodeString int "{ \"hello\": 42 }" == Err ...

-}
int : Decoder Int
int =
    decodeWith String.toInt (Not AInt)


{-| Decode a CSV number into an Elm `Float`.

    decodeString float "true"              == Err ..
    decodeString float "42"                == Ok 42
    decodeString float "3.14"              == Ok 3.14
    decodeString float "\"hello\""         == Err ...
    decodeString float "{ \"hello\": 42 }" == Err ...

-}
float : Decoder Float
float =
    decodeWith String.toFloat (Not AFloat)


decodeWith : (String -> Maybe a) -> Error -> Decoder a
decodeWith fromString error =
    let
        take input =
            input
                |> List.head
                |> Maybe.andThen fromString
                |> Result.fromMaybe error
    in
    Decoder take


{-| Transform a decoder. Maybe you just want to know the length of a string:

    import String

    stringLength : Decoder Int
    stringLength =
        map String.length string

It is often helpful to use `map` with `oneOf`, like when defining `maybe`:

    maybe : Decoder a -> Decoder (Maybe a)
    maybe decoder =
        oneOf
            [ map Just decoder
            , succeed Nothing
            ]

-}
map : (a -> value) -> Decoder a -> Decoder value
map mapper (Decoder d) =
    Decoder (d >> Result.map mapper)

{-| Try a bunch of different decoders. This can be useful if the CSV may come
in a couple different formats. For example, say you want to read an array of
numbers, but some of them are `null`.

    import String

    badInt : Decoder Int
    badInt =
      oneOf [ int, null 0 ]

    -- decodeString (list badInt) "[1,2,null,4]" == Ok [1,2,0,4]

Why would someone generate CSV like this? Questions like this are not good
for your health. The point is that you can use `oneOf` to handle situations
like this!

You could also use `oneOf` to help version your data. Try the latest format,
then a few older ones that you still support. You could use `andThen` to be
even more particular if you wanted.
-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    let
       firstToSucceed ds input =
            case ds of
                [] -> Err NonApply

                (Decoder d) :: rs ->
                    case d input of
                        Ok _ as result -> result

                        Err _ ->
                            firstToSucceed rs input


    in
        Decoder <| firstToSucceed decoders


{-| Ignore the CSV and produce a certain Elm value.

    decodeString (succeed 42) "true"    == Ok 42
    decodeString (succeed 42) "[1,2,3]" == Ok 42
    decodeString (succeed 42) "hello"   == Err ... -- this is not a valid JSON string

This is handy when used with `oneOf` or `andThen`.

-}
succeed : a -> Decoder a
succeed value =
    Decoder (\_ -> Ok value)


{-| Ignore the CSV and make the decoder fail. This is handy when used with
`oneOf` or `andThen` where you want to give a custom error message in some
case.

See the [`andThen`](#andThen) docs for an example.

-}
fail : String -> Decoder a
fail reason =
    Decoder (\_ -> Err <| FailWithReason reason)

