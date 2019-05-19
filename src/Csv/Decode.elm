module Csv.Decode exposing
    ( Decoder, Csv, Error(..), Kind(..)
    , string, int, float, bool
    , decode
    , map, map2, map3
    , succeed, fail, maybe, oneOf
    )

{-| Turn CSV Values into Elm values. Inspired by [`elm/json`][elm-json], so make sure to check out this [intro to
JSON decoders][guide] to get a feel for how this library works!

[guide]: https://guide.elm-lang.org/effects/json.html
[elm-json]: https://package.elm-lang.org/packages/elm/json/latest/

Note this library does not include an underlying CSV parser. It assumes you are using something like [`periodic/elm-csv`][periodic] to get from `String` to `Csv`, where `Csv` is:

    type alias Csv =
        { headers : List String
        , records : List (List String)
        }

This library gets you the rest of the way, to a list of your own types.

[periodic]: https://package.elm-lang.org/packages/periodic/elm-csv/latest/

In the examples we make use of a `decodeString` function. Which is defined as

    decodeString : Decoder a -> String -> Result Error (List a)
    decodeString decoder input =
        input
            |> (++) "\n"
            |> Csv.parse
            |> Result.mapError (\_ -> CsvParseError)
            |> Result.andThen (decode decoder)


# Types

@docs Decoder, Csv, Error, Kind


# Primitives

@docs string, int, float, bool


# Run Decoders

@docs decode


# Mapping

@docs map, map2, map3


# Fancy Decoding

@docs succeed, fail, maybe, oneOf

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
    = Decoder (List String -> Result Error ( a, List String ))


{-| A structured error describing exactly how the decoder failed. You can use
this to create more elaborate visualizations of a decoder problem. For example,
you could show the entire CSV record and show the part causing the failure in
red.
-}
type Error
    = CsvParseError
    | Not Kind
    | FailWithReason String
    | NonApply
    | MultipleErrors (List ( Int, Error ))


{-| Kind determines what type a decoder expects. Used in combination with the `Not` error.
-}
type Kind
    = AString
    | ABool
    | AInt
    | AFloat


{-| Decode the given `Csv` into a list of custom value by running `Decoder` on it.
This will fail if any of the records can not be decoded by the `Decoder` for some reason.
-}
decode : Decoder a -> Csv -> Result Error (List a)
decode (Decoder mapper) { records } =
    records
        |> List.map mapper
        |> gather


gather : List (Result Error ( a, List String )) -> Result Error (List a)
gather results =
    let
        split ( index, result ) ( errors, vs ) =
            case result of
                Ok ( v, _ ) ->
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


{-| Decode a CSV string into an Elm `String`.

    decodeString string "true" == Ok [ "true" ]

    decodeString string "42" == Ok [ "42" ]

    decodeString string "3.14" == Ok [ "3.14" ]

    decodeString string "hello" == Ok [ "hello" ]

-}
string : Decoder String
string =
    decodeWith (Just << identity) (Not AString)


{-| Decode a CSV boolean into an Elm `Bool`.

    decodeString bool "true"  == Ok [ True ]
    decodeString bool "42"    == Err ...
    decodeString bool "3.14"  == Err ...
    decodeString bool "hello" == Err ...

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

    decodeString int "true"  == Err ...
    decodeString int "42"    == Ok [ 42 ]
    decodeString int "3.14"  == Err ...
    decodeString int "hello" == Err ...

-}
int : Decoder Int
int =
    decodeWith String.toInt (Not AInt)


{-| Decode a CSV number into an Elm `Float`.

    decodeString float "true"  == Err ..
    decodeString float "42"    == Ok [ 42 ]
    decodeString float "3.14"  == Ok [ 3.14 ]
    decodeString float "hello" == Err ...

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
                |> Maybe.map (\v -> ( v, List.tail input |> Maybe.withDefault [] ))
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
    let
        mapPair ( v, rest ) =
            ( mapper v, rest )
    in
    Decoder (d >> Result.map mapPair)


{-| Try two decoders and then combine the result. We can use this to decode
objects with many fields:


    type alias Point =
        { x : Float, y : Float }

    point : Decoder Point
    point =
        map2 Point
            float
            float

    -- decodeString point "3,4" == Ok { x = 3, y = 4 }

It tries each individual decoder and puts the result together with the `Point`
constructor.

-}
map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 mapper (Decoder a) (Decoder b) =
    let
        nextDecoder ( u, us ) =
            case b us of
                Ok ( v, vs ) ->
                    Ok ( mapper u v, vs )

                Err error ->
                    Err error
    in
    Decoder (a >> Result.andThen nextDecoder)


{-| Try three decoders and then combine the result. We can use this to decode
objects with many fields:


    type alias Person =
        { name : String, age : Int, height : Float }

    person : Decoder Person
    person =
        map3 Person
            string
            int
            float

    -- csv = "tom,42,1.8"
    -- decodeString person csv == Ok { name = "tom", age = 42, height = 1.8 }

Like `map2` it tries each decoder in order and then give the results to the
`Person` constructor. That can be any function though!

-}
map3 : (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
map3 mapper (Decoder a) (Decoder b) (Decoder c) =
    let
        next2Decoders ( u, us ) =
            case b us of
                Ok ( v, vs ) ->
                    case c vs of
                        Ok ( w, ws ) ->
                            Ok ( mapper u v w, ws )

                        Err error ->
                            Err error

                Err error ->
                    Err error
    in
    Decoder (a >> Result.andThen next2Decoders)


{-| Helpful for dealing with optional fields. Here are a few slightly different
examples:

    decodeString (maybe int) "42" == Ok [ Just 42 ]

    decodeString (maybe int) ",42" == Ok [ Nothing ]

-}
maybe : Decoder a -> Decoder (Maybe a)
maybe decoder =
    oneOf
        [ map Just decoder
        , succeed Nothing
        ]


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

-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    let
        firstToSucceed ds input =
            case ds of
                [] ->
                    Err NonApply

                (Decoder d) :: rs ->
                    case d input of
                        (Ok _) as result ->
                            result

                        Err _ ->
                            firstToSucceed rs input
    in
    Decoder <| firstToSucceed decoders


{-| Ignore the CSV and produce a certain Elm value.

    decodeString (succeed 42) "true"  == Ok [ 42 ]
    decodeString (succeed 42) "1,2,3" == Ok [ 42 ]
    decodeString (succeed 42) "hello" == Ok [ 42 ]

This is handy when used with `oneOf`.

-}
succeed : a -> Decoder a
succeed value =
    Decoder (\input -> Ok ( value, input ))


{-| Ignore the CSV and make the decoder fail. This is handy when used with
`oneOf` where you want to give a custom error message in some
case.
-}
fail : String -> Decoder a
fail reason =
    Decoder (\_ -> Err <| FailWithReason reason)
