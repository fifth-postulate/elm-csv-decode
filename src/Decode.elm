module Csv.Decode exposing (Decoder)

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

# Primitives

@docs Decoder

-}


type Decoder a
    = Decoder
