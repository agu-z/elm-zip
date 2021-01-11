module Zip.Entry exposing
    ( Entry
    , ExtractError(..)
    , checksum
    , comment
    , extract
    , extractWith
    , fileName
    , isDirectory
    , lastModified
    )

import Bitwise
import Bytes exposing (Bytes)
import Date
import Flate exposing (inflate)
import Internal.Format as Internal exposing (CdRecord, CompressionMethod(..), Entry(..), readFile)
import Time exposing (Posix, Zone)
import Time.Extra as Time


type alias Entry =
    Internal.Entry


fileName : Entry -> String
fileName (Entry _ record) =
    record.fileName


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


comment : Entry -> String
comment (Entry _ record) =
    record.comment


isDirectory : Entry -> Bool
isDirectory (Entry _ record) =
    Bitwise.and record.externalAttributes 0x10 /= 0 || String.endsWith "/" record.fileName


checksum : Entry -> Int
checksum (Entry _ record) =
    record.crc32


type ExtractError
    = UnsupportedCompression Int
    | InflateError
    | IntegrityError
    | NoData


extract : Entry -> Result ExtractError Bytes
extract =
    extractWith (always Nothing)


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
            Err NoData


integrity : Int -> Bytes -> Result ExtractError Bytes
integrity sum bytes =
    if sum == Flate.crc32 bytes then
        Ok bytes

    else
        Err IntegrityError
