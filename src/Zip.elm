module Zip exposing
    ( Zip
    , fromBytes
    , ls
    , byPath
    )

{-| Work with [Zip archives](https://en.wikipedia.org/wiki/ZIP_file_format).

@docs Zip


# Reading

@docs fromBytes


# Accessing Content

Once you have a `Zip`, you can use it to access its files and directories.

Use the [Zip.Entry module](./Zip-Entry#Entry) to do read their content and metadata.

@docs ls
@docs byPath

-}

import Bytes exposing (Bytes)
import Internal.Format exposing (CdRecord, Entry(..), readDirectory)


{-| Represents a Zip archive.

An archive is comprised of [entries](./Zip-Entry#Entry) which represent (optionally) compressed files and directories.

-}
type Zip
    = Zip Bytes (List CdRecord)


{-| Read a `Zip` from `Bytes`.

If you have [an uploaded File](https://package.elm-lang.org/packages/elm/file/latest/File) of an archive, you can use [`File.toBytes`](https://package.elm-lang.org/packages/elm/file/latest/File#toBytes) to read it:

    import File exposing (File)
    import Task exposing (Task)
    import Zip exposing (Zip)

    type Msg
        = GotZip (Maybe Zip)

    readArchive : File -> Cmd Msg
    readArchive file =
        file
            |> File.toBytes
            |> Task.map Zip.fromBytes
            |> Task.perform GotZip

You can also get `Bytes` from anywhere like [an HTTP request](https://package.elm-lang.org/packages/elm/http/latest/Http#expectBytes),
or even from [within another archive](./Zip-Entry#toBytes).

-}
fromBytes : Bytes -> Maybe Zip
fromBytes bytes =
    readDirectory bytes
        |> Maybe.map (\( data, records ) -> Zip data records)


{-| List all [entries](./Zip-Entry#Entry) in the archive.

    allEntries =
        Zip.ls zip

Files and directories get their own entries and they are not stored in a tree format.

If you only care about one of them, you can use the [`Zip.Entry.isDirectory`](./Zip-Entry#isDirectory) function to filter them:

    allFiles =
        List.filter (not << Entry.isDirectory) zip

-}
ls : Zip -> List Entry
ls (Zip allBytes records) =
    List.map (Entry allBytes) records


{-| Get an [entry](./Zip-Entry#Entry) by its absolute path.

    Zip.byPath "versions/v1.txt"

`Nothing` is returned if no entry matches the path exactly.

Directory entries are typically stored in the archive with a slash at the end:

    Zip.byPath "versions" == Nothing

    Zip.byPath "versions/" == Just Entry

-}
byPath : String -> Zip -> Maybe Entry
byPath name (Zip allBytes records) =
    records
        |> find (\record -> record.fileName == name)
        |> Maybe.map (Entry allBytes)


find : (a -> Bool) -> List a -> Maybe a
find check list =
    case list of
        [] ->
            Nothing

        item :: tail ->
            if check item then
                Just item

            else
                find check tail
