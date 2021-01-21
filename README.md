# elm-zip

Read and write [ZIP archives](https://en.wikipedia.org/wiki/ZIP_file_format) without leaving Elm.

See [the `Zip` module documentation](http://package.elm-lang.org/packages/agu-z/elm-zip/latest/Zip) to learn how to use it.

## Performance

These are the early days of this library and I'm mostly using it to work with fairly small archives. 

At this point, you might find performance bottlenecks. 
In that case, please [create an issue](https://github.com/agu-z/elm-zip/issues/new) that describes your use-case and
-whenever possible- provides test files.

We could deal with these limitations by [performing the intensive operations across multiple `update` cycles](https://discourse.elm-lang.org/t/long-running-computations-in-elm-that-wont-freeze-the-page/5836), 
or executing them on [WebWorkers](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers).

## Acknowledgements

Thanks to [folkertdev](https://github.com/folkertdev) for writing [elm-flate](https://package.elm-lang.org/packages/folkertdev/elm-flate/latest/).

This would've taken considerably longer if I had to write the compression algorithm too ☺️

## --

[Agus Zubiaga](https://aguz.me) - 2021
