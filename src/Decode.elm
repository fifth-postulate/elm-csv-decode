module Csv.Decode exposing
    ( Decoder, Csv, Error
    , decode
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

@docs Decoder, Csv, Error


# Primitives


# Run Decoders

@docs decode

-}


{-| The raw CSV data structure.
-}
type alias Csv =
    { header : List String
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
    | GeneralError
    | MultipleErrors (List ( Int, Error ))


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
