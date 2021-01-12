module Zip.Entry exposing
    ( Entry
    , toString
    , toBytes
    , ExtractError(..)
    , path
    , extractedSize
    , compressedSize
    , lastModified
    , isDirectory
    , comment
    , checksum
    , extractWith
    )

{-| Work with files and directories in the archive.

@docs Entry


# Extract Content

@docs toString
@docs toBytes
@docs ExtractError


# Read Metadata

@docs path
@docs extractedSize
@docs compressedSize
@docs lastModified
@docs isDirectory
@docs comment
@docs checksum


# Other Compression Methods

This library uses the [`folkertdev/elm-flate`](https://package.elm-lang.org/packages/folkertdev/elm-flate/latest) package to support
[Deflate](https://en.wikipedia.org/wiki/Deflate) compression. Most archives you'll find in the wild will use this method.

If you're expecting to work with archives using other methods, you can use the following function to handle them.

@docs extractWith

-}

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Date
import Flate exposing (inflate)
import Internal.Format as Internal exposing (CdRecord, CompressionMethod(..), Entry(..), readFile)
import Time exposing (Posix, Zone)
import Time.Extra as Time


{-| Represents a file or a directory in a [`Zip`](./Zip) archive.

You can use this to [extract the content](#extract-content) and [read the metadata](#read-metadata).

Check out [Entry.path](#path) to learn more about the way these entries are stored.

-}
type alias Entry =
    Internal.Entry


{-| Extracting content from an entry might fail if:

1.  The data is compressed through an unsupported method. See [extractWith](#extractWith) for more information.
2.  The extracted data does not match the integrity checksum.
3.  The entry has no data of the expected type.
4.  The [DEFLATE](https://en.wikipedia.org/wiki/Deflate) data is corrupted.

-}
type ExtractError
    = UnsupportedCompression Int
    | IntegrityError
    | DecodeError
    | InflateError


{-| Extract the content of an `Entry` as a `String`.
-}
toString : Entry -> Result ExtractError String
toString =
    extractWith (always Nothing)
        >> Result.andThen (Result.fromMaybe DecodeError << asString)


asString : Bytes -> Maybe String
asString bytes =
    let
        decoder =
            Decode.string (Bytes.width bytes)
    in
    Decode.decode decoder bytes


{-| Extract the content of an `Entry` as `Bytes`.

Bytes can represent an image, a PDF, a ZIP within a ZIP, anything you can imagine.

Examples of what you can do with `Bytes`:

  - Use [`File.Download.bytes`](https://package.elm-lang.org/packages/elm/file/latest/File-Download#bytes) to download them as a file.
  - Use [`Http.bytesBody`](https://package.elm-lang.org/packages/elm/http/latest/Http#bytesBody) to send them to an HTTP server.
  - Use the [`elm/bytes`](https://package.elm-lang.org/packages/elm/bytes/latest) package to decode these bytes into any data structure.

-}
toBytes : Entry -> Result ExtractError Bytes
toBytes =
    extractWith (always Nothing)


{-| Extract archives with other compression methods.

    handleCompression { method, rawBytes } =
        case method of
            6 ->
                -- Use raw bytes to extract the data.
                decodeImplode rawBytes

            _ ->
                -- We don't know how to handle other methods.
                Nothing

    extracted =
        entry |> extractWith handleCompression

Note: This callback is only called if the method is not 0 (Stored) nor 8 (Deflated). You do not need to handle these yourself.

You can read more about compression methods and their corresponding numbers in section 4.4.5 of the [specification](https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT).

-}
extractWith :
    ({ method : Int, rawBytes : Bytes } -> Maybe Bytes)
    -> Entry
    -> Result ExtractError Bytes
extractWith fallback (Entry allBytes record) =
    case readFile allBytes record of
        Just rawBytes ->
            (case record.compressionMethod of
                Stored ->
                    Ok rawBytes

                Deflated ->
                    inflate rawBytes
                        |> Result.fromMaybe InflateError

                Unsupported method ->
                    case fallback { method = method, rawBytes = rawBytes } of
                        Just bytes ->
                            Ok bytes

                        Nothing ->
                            Err (UnsupportedCompression method)
            )
                |> Result.andThen (integrity record.crc32)

        Nothing ->
            Err DecodeError


integrity : Int -> Bytes -> Result ExtractError Bytes
integrity sum bytes =
    if sum == Flate.crc32 bytes then
        Ok bytes

    else
        Err IntegrityError


{-| Get the absolute path of an entry.

    path dir == "versions/"

    path v1 == "versions/v1"

    path v2 == "versions/v2"

Even though Zip archives are aware of directories, they do not store entries in a tree format.
Instead, each entry simply indicates its absolute path in the archive.

Different applications have different needs and they may or may not care about the tree structure.

Some applications might expect a certain structure and can simply use [`Zip.byPath`](./Zip#byName) to get the
relevant entries.

Other applications might want to explore the archive, and can use [`Zip.ls`](./Zip#ls) to get a list of the entries and go from there.

-}
path : Entry -> String
path (Entry _ record) =
    record.fileName


{-| Get the uncompressed size of an entry.

This is the number of bytes that you will get if you extract this entry.

-}
extractedSize : Entry -> Int
extractedSize (Entry _ record) =
    record.uncompressedSize


{-| Get the compressed size of an entry as stored in the archive.
-}
compressedSize : Entry -> Int
compressedSize (Entry _ record) =
    record.compressedSize


{-| Get the last time an entry was modified.

Zip time stamps are relative to the time zone they were created in.

Unfortunately, this information is not stored anywhere, making them only meaningful if you know it.

-}
lastModified : Zone -> Entry -> Posix
lastModified timezone (Entry _ record) =
    let
        time =
            record.lastModified
    in
    Time.partsToPosix timezone
        { year =
            time
                |> Bitwise.shiftRightBy 25
                |> (+) 1980
        , month =
            time
                |> Bitwise.shiftRightBy 21
                |> Bitwise.and 15
                |> Date.numberToMonth
        , day =
            time
                |> Bitwise.shiftRightBy 16
                |> Bitwise.and 31
        , hour =
            time
                |> Bitwise.shiftRightBy 11
                |> Bitwise.and 31
        , minute =
            time
                |> Bitwise.shiftRightBy 5
                |> Bitwise.and 63
        , second =
            time
                |> Bitwise.and 63
                |> (*) 2
        , millisecond = 0
        }


{-| Get the comment of an entry.

Did you know files can have comments? I didn't.

-}
comment : Entry -> String
comment (Entry _ record) =
    record.comment


{-| Determine whether an entry is a directory.
-}
isDirectory : Entry -> Bool
isDirectory (Entry _ record) =
    Bitwise.and record.externalAttributes 0x10 /= 0 || String.endsWith "/" record.fileName


{-| Get the [CRC32 checksum](https://en.wikipedia.org/wiki/Cyclic_redundancy_check) of an entry's uncompressed data.

You do not need to confirm the integrity of data manually. The extract content functions perform this check automatically for you.

However, you might still find this checksum useful for other purposes, like quickly determining whether two files are identical.

-}
checksum : Entry -> Int
checksum (Entry _ record) =
    record.crc32
