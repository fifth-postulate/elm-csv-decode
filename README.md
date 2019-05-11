# elm-csv-decode-pipeline
Use pipelines to build CSV decoders.

## Motivation
The package for decoding CSV into data structures of your own is [`ericgj/elm-csv-decode`][elm-decode]. I had a hard time using the package because I was expecting it to be similar to [`elm/json`][elm-json], and it is subtly different.

This package is an exploration into if we could create a CSV decoder that is aligns more with my intuition, and works similar to [`NoRedInk/elm-json-decode-pipeline`][json-pipeline].

[elm-decode]: https://package.elm-lang.org/packages/ericgj/elm-csv-decode/latest/
[elm-json]: https://package.elm-lang.org/packages/elm/json/latest/
[json-pipeline]: https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/