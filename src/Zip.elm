module Zip exposing
    ( Entry
    , Zip
    , entries
    , fromBytes
    )

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Date
import Time exposing (Posix)
import Time.Extra as Time


type Zip
    = Zip Bytes (List CdRecord)


fromBytes : Bytes -> Maybe Zip
fromBytes bytes =
    findCdBounds bytes
        |> Maybe.andThen (\bounds -> Decode.decode (zipFrom bounds) bytes)


type Entry
    = Entry CdRecord


entries : Zip -> List Entry
entries (Zip _ records) =
    List.map Entry records



-- PRIVATE


zipFrom : CdBounds -> Decoder Zip
zipFrom bounds =
    Decode.map2 Zip
        (Decode.bytes bounds.start)
        (list bounds.recordCount cdRecord)


type alias CdBounds =
    { recordCount : Int
    , start : Int
    }


findCdBounds : Bytes -> Maybe CdBounds
findCdBounds bytes =
    let
        decoder signature =
            if signature == 0x06054B50 then
                Decode.map2 CdBounds
                    (Decode.unsignedInt16 LE |> after 6)
                    (Decode.unsignedInt16 LE |> after 4)

            else
                Decode.fail

        at offset =
            Decode.unsignedInt32 LE
                |> Decode.andThen decoder
                |> after offset

        attempt offset =
            case Decode.decode (at offset) bytes of
                Just bounds ->
                    Just bounds

                Nothing ->
                    if offset < 0 then
                        Nothing

                    else
                        attempt (offset - 1)
    in
    attempt (Bytes.width bytes - 22)


type alias CdRecord =
    { compressionMethod : CompressionMethod
    , lastModified : Posix
    , crc32 : Bytes
    , compressedSize : Int
    , uncompressedSize : Int
    , startOffset : Int
    , fileName : String
    , extraField : Bytes
    , comment : String
    , externalAttributes : Int
    }


type alias CdRecordBounds =
    { nameLength : Int
    , extraFieldLength : Int
    , commentLength : Int
    , externalAttributes : Int
    , startOffset : Int
    }


cdRecord : Decoder CdRecord
cdRecord =
    let
        start =
            Decode.map5 CdRecord
                (compressionMethod |> after 10)
                timestamp
                (Decode.bytes 4)
                (Decode.signedInt32 LE)
                (Decode.signedInt32 LE)

        recordBounds =
            Decode.map5 CdRecordBounds
                (Decode.unsignedInt16 LE)
                (Decode.unsignedInt16 LE)
                (Decode.unsignedInt16 LE)
                (Decode.unsignedInt32 LE |> after 4)
                (Decode.unsignedInt32 LE)

        finish ( beginning, bounds ) =
            Decode.map4 (beginning bounds.startOffset)
                (Decode.string bounds.nameLength)
                (Decode.bytes bounds.extraFieldLength)
                (Decode.string bounds.commentLength)
                (Decode.succeed bounds.externalAttributes)
    in
    Decode.map2 Tuple.pair start recordBounds
        |> Decode.andThen finish


type CompressionMethod
    = Stored
    | Deflated
    | Unsupported


compressionMethod : Decoder CompressionMethod
compressionMethod =
    let
        help m =
            case m of
                0 ->
                    Stored

                8 ->
                    Deflated

                _ ->
                    Unsupported
    in
    Decode.unsignedInt16 LE
        |> Decode.map help


timestamp : Decoder Posix
timestamp =
    Decode.map dosToPosix
        (Decode.unsignedInt32 LE)


dosToPosix : Int -> Posix
dosToPosix time =
    Time.partsToPosix Time.utc
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



-- Bytes Utils


after : Int -> Decoder b -> Decoder b
after offset =
    Decode.map2 (\_ a -> a) (Decode.bytes offset)


list : Int -> Decoder a -> Decoder (List a)
list length aDecoder =
    Decode.loop ( length, [] ) (listStep aDecoder)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep elementDecoder ( n, elements ) =
    if n <= 0 then
        Decode.succeed (Done (List.reverse elements))

    else
        Decode.map (\element -> Loop ( n - 1, element :: elements )) elementDecoder
