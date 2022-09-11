module Zip exposing
    ( Zip
    , fromBytes
    , entries
    , getEntry
    , count
    , isEmpty
    , empty
    , fromEntries
    , insert
    , filter
    , toBytes
    )

{-| Work with [Zip archives](https://en.wikipedia.org/wiki/ZIP_file_format).

@docs Zip


# Read an archive

@docs fromBytes


# Access the content

Once you have a `Zip`, you can use it to access its files and directories.

Use the [Zip.Entry module](./Zip-Entry#Entry) to do read their content and metadata.

@docs entries
@docs getEntry
@docs count
@docs isEmpty


# Build an archive

You can alter archives or create your own.

Checkout the [Build section](./Zip-Entry#build) of the `Zip.Entry` module to learn how to make your own entries.

@docs empty
@docs fromEntries
@docs insert
@docs filter


## ...and when it's ready

@docs toBytes

-}

import Bytes exposing (Bytes)
import Internal.Decode exposing (readDirectory)
import Internal.Encode exposing (writeArchive)
import Internal.Format exposing (Entry)
import Zip.Entry as Entry


{-| Represents a Zip archive.

An archive is comprised of [entries](./Zip-Entry#Entry) which represent files -that may be compressed- and directories.

-}
type Zip
    = Zip (List Entry)


{-| Read a `Zip` from `Bytes`.

If you have [an uploaded File](https://package.elm-lang.org/packages/elm/file/latest/File) of an archive,
you can use [`File.toBytes`](https://package.elm-lang.org/packages/elm/file/latest/File#toBytes) to read it:

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

You can also get `Bytes` from somewhere else, such as [an HTTP request](https://package.elm-lang.org/packages/elm/http/latest/Http#expectBytes),
or even from [within another archive](./Zip-Entry#toBytes).

-}
fromBytes : Bytes -> Maybe Zip
fromBytes bytes =
    readDirectory bytes |> Maybe.map Zip


{-| Write a `Zip` to `Bytes`.

From here, you can [download the archive](https://package.elm-lang.org/packages/elm/file/latest/File-Download#bytes),
[upload it to a server](https://package.elm-lang.org/packages/elm/http/latest/Http#bytesBody>), etc.

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            DownloadArchive ->
                ( model
                , model.zip
                    |> Zip.toBytes
                    |> File.Download.bytes "archive.zip" "application/zip"
                )

-}
toBytes : Zip -> Bytes
toBytes (Zip allEntries) =
    writeArchive allEntries


{-| Get all [entries](./Zip-Entry#Entry) in the archive.

    allEntries =
        Zip.entries zip

Files and directories get their own entries.

If you only care about one kind, you can use the [`Zip.Entry.isDirectory`](./Zip-Entry#isDirectory) function to filter them:

    allFiles =
        zip
            |> Zip.entries
            |> List.filter (not << Entry.isDirectory)

-}
entries : Zip -> List Entry
entries (Zip allEntries) =
    allEntries


{-| Get an [entry](./Zip-Entry#Entry) by its absolute path.

    zip |> Zip.getEntry "versions/v1.txt"

`Nothing` is returned if no entry matches the path exactly.

Directory entries are typically stored in the archive with a slash at the end:

    zip |> Zip.getEntry "versions" == Nothing

    zip |> Zip.getEntry "versions/" == Just (Entry(..))

-}
getEntry : String -> Zip -> Maybe Entry
getEntry path =
    entries >> find (Entry.path >> (==) path)


{-| Count the number of entries in an archive.
-}
count : Zip -> Int
count =
    entries >> List.length


{-| Determine if an archive is empty.
-}
isEmpty : Zip -> Bool
isEmpty =
    entries >> List.isEmpty


{-| An empty archive with no entries.

From here, you can use [`insert`](#insert) to add some entries.

-}
empty : Zip
empty =
    Zip []


{-| Create an archive from a list of entries.
-}
fromEntries : List Entry -> Zip
fromEntries =
    Zip


{-| Add a new entry to the archive.

This function replaces entries with the same path. You can conditionally add it by checking existence with the [`getEntry`](#getEntry) function:

    case zip |> Zip.getEntry path of
        Nothing ->
            -- Entry does not exist, create and add it
            zip |> Zip.insert (createEntry ())

        Just _ ->
            -- Entry already exists, leave archive as it is
            zip

-}
insert : Entry -> Zip -> Zip
insert entry (Zip currentEntries) =
    currentEntries
        |> List.filter (Entry.path >> (/=) (Entry.path entry))
        |> (::) entry
        |> Zip


{-| Only keep entries that pass a given test.


### Examples

Remove entries by path:

    filter (\entry -> Entry.path entry /= "sample/version.json") zip

Keep all files under 1MB:

    filter (\entry -> Entry.extractedSize entry < 1048576) zip

Keep only `.txt` files:

    filter (Entry.path >> String.endsWith ".txt") zip

-}
filter : (Entry -> Bool) -> Zip -> Zip
filter check (Zip currentEntries) =
    currentEntries
        |> List.filter check
        |> Zip


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
