# elm-zip

Read and write [ZIP archives](https://en.wikipedia.org/wiki/ZIP_file_format) using pure Elm.

See [the `Zip` module documentation](https://package.elm-lang.org/packages/agu-z/elm-zip/3.0.1/Zip) to learn how to use it.

You can also check out an [example app](https://github.com/agu-z/elm-zip/blob/main/examples/src/Read.elm) that can open an archive and extract files from it.

## Performance

These are the early days of this library and I'm mostly using it to work with fairly small archives.

If you find bottlenecks please [create an issue](https://github.com/agu-z/elm-zip/issues/new) that describes your use-case and
-whenever possible- provides test files.

## Acknowledgements

Thanks to [folkertdev](https://github.com/folkertdev) for writing [elm-flate](https://package.elm-lang.org/packages/folkertdev/elm-flate/latest/).

This would've taken considerably longer if I had to write the compression algorithm too ☺️

## --

[Agus Zubiaga](https://aguz.me) - 2021
