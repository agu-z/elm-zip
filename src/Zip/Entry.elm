module Zip.Entry exposing
    ( Entry
    , toString
    , toBytes
    , ExtractError(..)
    , path
    , basename
    , extractedSize
    , compressedSize
    , lastModified
    , isDirectory
    , comment
    , checksum
    , Meta
    , store
    , compress
    , createDirectory
    )

{-| Work with files and directories in the archive.

@docs Entry


# Extract Content

@docs toString
@docs toBytes
@docs ExtractError


# Read Metadata

@docs path
@docs basename
@docs extractedSize
@docs compressedSize
@docs lastModified
@docs isDirectory
@docs comment
@docs checksum


# Build

Create archive entries.

@docs Meta


## Files

When you create a file `Entry` you can choose to [store](#store) the data as-is or [compress](#compress) it.

Keep in mind that:

  - Compressing files is slower than storing them.
  - Compression is effective when the data contains repeated patterns. For example, XML files are good candidates.
  - Compressing very small files with few repeated patterns can actually result in bigger archives.
    This is because we need to store extra data in order to uncompress them.
  - The ZIP format stores files individually with their own compression. Unfortunately, patterns shared across files
    cannot be reused.

Hopefully that helps you decide whether you need compression or not.

@docs store
@docs compress


## Directories

@docs createDirectory


# Compression Methods

[Deflate](https://en.wikipedia.org/wiki/Deflate) compression is provided by
[`folkertdev/elm-flate`](https://package.elm-lang.org/packages/folkertdev/elm-flate/latest/).
Most archives you'll find in the wild will use this method.

If you're expecting to work with archives using other methods, you can handle them by using the method number
and raw bytes from the `UnsupportedCompression` case.

    case toBytes entry of
        Err (UnsupportedCompression 6 rawBytes) ->
            Ok <| decodeImplode rawBytes

        result ->
            result

You can read more about compression methods and their corresponding numbers in section 4.4.5 of
the [specification](https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT).

-}

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Flate exposing (inflate)
import Internal.Decode exposing (readFile)
import Internal.Encode exposing (noBytes)
import Internal.Format as Internal exposing (CompressionMethod(..), Entry(..), EntryBytes(..), EntryMeta)
import LZ77
import Time exposing (Month(..), Posix, Zone)
import Time.Extra as Time


{-| Represents a file or a directory in a [`Zip`](./Zip) archive.

You can use this to [extract the content](#extract-content) and [read the metadata](#read-metadata).

See [`Entry.path`](#path) to learn more about the way these entries are stored.

-}
type alias Entry =
    Internal.Entry


{-| Extracting content from an entry might fail if:

1.  The data is compressed through an unsupported method. See [Compression Methods](#compression-methods) for more information.
2.  The extracted data does not match the integrity checksum.
3.  The entry has no data of the expected type.
4.  The [DEFLATE](https://en.wikipedia.org/wiki/Deflate) data is corrupted.

-}
type ExtractError
    = UnsupportedCompression Int Bytes
    | IntegrityError
    | DecodeError
    | InflateError


{-| Extract the content of an `Entry` as a `String`.
-}
toString : Entry -> Result ExtractError String
toString =
    toBytes
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
toBytes ((Entry _ record) as entry) =
    case readFile entry of
        Just rawBytes ->
            (case record.compressionMethod of
                Stored ->
                    Ok rawBytes

                Deflated ->
                    inflate rawBytes
                        |> Result.fromMaybe InflateError

                Unsupported method ->
                    Err (UnsupportedCompression method rawBytes)
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

    path v1 == "versions/v1.txt"

    path v2 == "versions/v2.txt"

Even though Zip archives are aware of directories, they do not store entries in a tree format.
Instead, each entry simply indicates its absolute path in the archive.

Different applications have different needs and they may or may not care about the tree structure.

Some applications might expect a certain structure and can simply use [`Zip.getEntry`](./Zip#getEntry) to get the
relevant entries.

Other applications might want to explore the archive, and can use [`Zip.entries`](./Zip#entries) to get a list of the entries and go from there.

-}
path : Entry -> String
path (Entry _ record) =
    record.fileName


{-| Get the final component of an entry's path.

    basename v1 == "v1.txt"

    path v1 == "versions/v1.txt"

-}
basename : Entry -> String
basename =
    path
        >> String.split "/"
        >> List.filter ((/=) "")
        >> List.reverse
        >> List.head
        >> Maybe.withDefault ""


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

Zip time stamps are relative to the time zone they were created in. However, the time zone is not stored in the archive.
This means you need to know the zone to get a meaningful time stamp.

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
                |> numberToMonth
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


numberToMonth : Int -> Month
numberToMonth month =
    case max 1 month of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        _ ->
            Dec


{-| Get the comment of an entry.
-}
comment : Entry -> String
comment (Entry _ record) =
    record.comment


{-| Determine if an entry is a directory.
-}
isDirectory : Entry -> Bool
isDirectory (Entry _ record) =
    -- MS-DOS Directory Attribute
    (Bitwise.and record.externalAttributes 0x10 /= 0)
        -- Directory paths end with a slash
        || String.endsWith "/" record.fileName


{-| Get the [CRC32 checksum](https://en.wikipedia.org/wiki/Cyclic_redundancy_check) of an entry's uncompressed data.

You don't need to check the integrity of the data, the extract content functions do it for you.

However, you might still find this checksum useful for other purposes, like quickly determining whether two files are identical.

-}
checksum : Entry -> Int
checksum (Entry _ record) =
    record.crc32



-- Writing


posixToDos : ( Zone, Posix ) -> Int
posixToDos ( zone, time ) =
    let
        year =
            Time.toYear zone time
                - 1980
                |> Bitwise.shiftLeftBy 25

        month =
            Time.toMonth zone time
                |> monthToNumber
                |> Bitwise.shiftLeftBy 21

        day =
            Time.toDay zone time
                |> Bitwise.shiftLeftBy 16

        hour =
            Time.toHour zone time
                |> Bitwise.shiftLeftBy 11

        minute =
            Time.toMinute zone time
                |> Bitwise.shiftLeftBy 5

        second =
            Time.toSecond zone time // 2
    in
    year
        |> Bitwise.or month
        |> Bitwise.or day
        |> Bitwise.or hour
        |> Bitwise.or minute
        |> Bitwise.or second


monthToNumber : Month -> Int
monthToNumber month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


{-| Metadata needed to create an entry.

Note: `lastModified` requires a `Time.Zone` to be provided because ZIP time stamps are not stored in a universal zone (like UTC). Read more [above](#lastModified).

-}
type alias Meta =
    { path : String
    , lastModified : ( Zone, Posix )
    , comment : Maybe String
    }


entryMeta : Meta -> EntryMeta
entryMeta meta =
    { madeBy = 0x031E
    , extractMinVersion = 20
    , flag = 0
    , compressionMethod = Stored
    , lastModified = posixToDos meta.lastModified
    , crc32 = 0
    , compressedSize = 0
    , uncompressedSize = 0
    , fileName = meta.path
    , extraField = noBytes
    , comment = Maybe.withDefault "" meta.comment
    , internalAttributes = 0
    , externalAttributes = 0
    }


unixMode : Int -> Int
unixMode =
    Bitwise.shiftLeftBy 16


fileMode : Int
fileMode =
    unixMode 0x81B4


dirMode : Int
dirMode =
    unixMode 0x81B4


{-| Create an entry for a file without compressing it.

    import Bytes.Encode as Encode

    helloTxt =
        Encode.string "Hello, World!"
            |> Encode.encode
            |> store
                { path = "hello.txt"
                , lastModified = ( zone, now )
                , comment = Nothing
                }

Files inside directories are created by passing the absolute path:

    store
        { path = "versions/v1.txt"
        , lastModified = ( zone, now )
        , comment = Nothing
        }

-}
store : Meta -> Bytes -> Entry
store meta data =
    let
        base =
            entryMeta meta
    in
    Entry (Exactly data)
        { base
            | compressionMethod = Stored
            , lastModified = posixToDos meta.lastModified
            , crc32 = Flate.crc32 data
            , compressedSize = Bytes.width data
            , uncompressedSize = Bytes.width data
            , externalAttributes = fileMode
        }


{-| Compress a file with [Deflate](https://en.wikipedia.org/wiki/Deflate) and create an entry out of it.

Besides compression, it works just like [`store`](#store).

-}
compress : Meta -> Bytes -> Entry
compress meta uncompressed =
    let
        base =
            entryMeta meta

        compressed =
            Flate.deflateWithOptions (Flate.Dynamic (Flate.WithWindowSize LZ77.maxWindowSize)) uncompressed
    in
    Entry (Exactly compressed)
        { base
            | compressionMethod = Deflated
            , lastModified = posixToDos meta.lastModified
            , crc32 = Flate.crc32 uncompressed
            , compressedSize = Bytes.width compressed
            , uncompressedSize = Bytes.width uncompressed
            , externalAttributes = fileMode
        }


{-| Create a directory entry.

You do not need to explicitly create directories. Extracting programs automatically create directories in the path to a file.

Use this if you need to add directory metadata or if you want a directory to exist even if it doesn't contain any files.

-}
createDirectory : Meta -> Entry
createDirectory meta =
    let
        base =
            entryMeta meta
    in
    Entry (Exactly noBytes)
        { base
            | fileName =
                if String.endsWith "/" base.fileName then
                    base.fileName

                else
                    base.fileName ++ "/"
            , externalAttributes = dirMode
        }
