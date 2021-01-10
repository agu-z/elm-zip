module Internal.Format exposing
    ( CdBounds
    , CdRecord
    , CdRecordBounds
    , CompressionMethod(..)
    , Entry(..)
    , after
    , cdRecord
    , compressionMethod
    , findCdBounds
    , list
    , listStep
    , readDirectory
    , topDecoder
    )

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))


type Entry
    = Entry CdRecord


readDirectory : Bytes -> Maybe ( Bytes, List CdRecord )
readDirectory bytes =
    findCdBounds bytes
        |> Maybe.andThen (\bounds -> Decode.decode (topDecoder bounds) bytes)


topDecoder : CdBounds -> Decoder ( Bytes, List CdRecord )
topDecoder bounds =
    Decode.map2 Tuple.pair
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
    , lastModified : Int
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
                (Decode.unsignedInt32 LE)
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
