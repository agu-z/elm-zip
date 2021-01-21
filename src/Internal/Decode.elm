module Internal.Decode exposing (readDirectory, readFile)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Internal.Format exposing (CompressionMethod(..), Entry(..), EntryBytes(..), EntryMeta)


readDirectory : Bytes -> Maybe (List Entry)
readDirectory bytes =
    let
        topDecoder bounds =
            list bounds.recordCount (entryIn bytes)
                |> after bounds.start
    in
    findCdBounds bytes
        |> Maybe.andThen (\bounds -> Decode.decode (topDecoder bounds) bytes)


type alias CdBounds =
    { recordCount : Int
    , start : Int
    }


findCdBounds : Bytes -> Maybe CdBounds
findCdBounds bytes =
    let
        decoder =
            Decode.succeed CdBounds
                |> checkSignature 0x06054B50
                |> with (i16 |> after 6)
                |> with (i32 |> after 4)

        attempt offset =
            case Decode.decode (decoder |> after offset) bytes of
                Just bounds ->
                    Just bounds

                Nothing ->
                    if offset < 0 then
                        Nothing

                    else
                        attempt (offset - 1)
    in
    attempt (Bytes.width bytes - 22)


type alias CdRecordBounds =
    { nameLength : Int
    , extraFieldLength : Int
    , commentLength : Int
    , internalAttributes : Int
    , externalAttributes : Int
    , startOffset : Int
    }


entryIn : Bytes -> Decoder Entry
entryIn bytes =
    let
        start =
            Decode.succeed EntryMeta
                |> checkSignature 0x02014B50
                |> with i16
                |> with i16
                |> with i16
                |> with compressionMethod
                |> with i32
                |> with i32
                |> with i32
                |> with i32

        recordBounds =
            Decode.succeed CdRecordBounds
                |> with i16
                |> with i16
                |> with i16
                |> with (i16 |> after 2)
                |> with i32
                |> with i32

        finish ( makeMeta, bounds ) =
            Decode.map5 makeMeta
                (Decode.string bounds.nameLength)
                (Decode.bytes bounds.extraFieldLength)
                (Decode.string bounds.commentLength)
                (Decode.succeed bounds.internalAttributes)
                (Decode.succeed bounds.externalAttributes)
                |> Decode.map (Entry (Offset bytes bounds.startOffset))
    in
    Decode.map2 Tuple.pair start recordBounds
        |> Decode.andThen finish


compressionMethod : Decoder CompressionMethod
compressionMethod =
    let
        help m =
            case m of
                0 ->
                    Stored

                8 ->
                    Deflated

                method ->
                    Unsupported method
    in
    i16
        |> Decode.map help


readFile : Entry -> Maybe Bytes
readFile (Entry bytes meta) =
    case bytes of
        Exactly entryBytes ->
            Just entryBytes

        Offset allBytes startOffset ->
            let
                entryDataDecoder =
                    Decode.succeed (+)
                        |> checkSignature 0x04034B50
                        |> with (i16 |> after 22)
                        |> with i16
                        |> Decode.andThen
                            (\offset ->
                                Decode.bytes meta.compressedSize
                                    |> after offset
                            )
            in
            Decode.decode (entryDataDecoder |> after startOffset) allBytes



-- Bytes Helpers


checkSignature : Int -> Decoder a -> Decoder a
checkSignature expected =
    let
        check value =
            if value == expected then
                Decode.succeed ()

            else
                Decode.fail
    in
    Decode.map2 (\_ b -> b)
        (Decode.andThen check i32)


i16 : Decoder Int
i16 =
    Decode.unsignedInt16 LE


i32 : Decoder Int
i32 =
    Decode.unsignedInt32 LE


with : Decoder a -> Decoder (a -> b) -> Decoder b
with a fn =
    Decode.map2 (<|) fn a


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
