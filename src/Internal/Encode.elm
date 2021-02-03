module Internal.Encode exposing (noBytes, writeArchive)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Encode exposing (Encoder)
import Internal.Decode exposing (readFile)
import Internal.Format exposing (CompressionMethod(..), Entry(..))


writeArchive : List Entry -> Bytes
writeArchive entries =
    encodeEntries (List.length entries)
        entries
        { local = Encode.sequence []
        , directory = Encode.sequence []
        , position = 0
        , directorySize = 0
        }
        |> Encode.encode


type alias Step =
    { local : Encoder
    , directory : Encoder
    , position : Int
    , directorySize : Int
    }


encodeEntries : Int -> List Entry -> Step -> Encoder
encodeEntries recordCount entries step =
    case entries of
        [] ->
            Encode.sequence
                [ step.local
                , step.directory
                , endOfCentralDirectory
                    { recordCount = recordCount
                    , size = step.directorySize
                    , startOffset = step.position
                    }
                ]

        entry :: tail ->
            encodeEntries recordCount tail (advance entry step)


endOfCentralDirectory :
    { recordCount : Int
    , size : Int
    , startOffset : Int
    }
    -> Encoder
endOfCentralDirectory { recordCount, size, startOffset } =
    Encode.sequence
        [ i32 0x06054B50
        , i16 0
        , i16 0
        , i16 recordCount
        , i16 recordCount
        , i32 size
        , i32 startOffset
        , i16 0
        ]


advance : Entry -> Step -> Step
advance ((Entry _ meta) as entry) step =
    let
        fileNameWidth =
            Encode.getStringWidth meta.fileName

        extraFieldWidth =
            Bytes.width meta.extraField

        commonHeader =
            Encode.sequence
                [ i16 meta.extractMinVersion
                , i16 meta.flag
                , compressionMethod meta.compressionMethod
                , i32 meta.lastModified
                , i32 meta.crc32
                , i32 meta.compressedSize
                , i32 meta.uncompressedSize
                , i16 fileNameWidth
                , i16 extraFieldWidth
                ]

        commonWidth =
            26 + fileNameWidth + extraFieldWidth

        data =
            case readFile entry of
                Just bytes ->
                    [ Encode.bytes bytes ]

                Nothing ->
                    []

        localFile =
            Encode.sequence
                ([ i32 0x04034B50
                 , commonHeader
                 , Encode.string meta.fileName
                 , Encode.bytes meta.extraField
                 ]
                    ++ data
                )

        localFileWidth =
            4 + commonWidth + meta.compressedSize

        commentWidth =
            Encode.getStringWidth meta.comment

        record =
            Encode.sequence
                [ i32 0x02014B50
                , i16 meta.madeBy
                , commonHeader
                , i16 commentWidth
                , i16 0
                , i16 meta.internalAttributes
                , i32 meta.externalAttributes
                , i32 step.position
                , Encode.string meta.fileName
                , Encode.bytes meta.extraField
                , Encode.string meta.comment
                ]

        recordWidth =
            20 + commonWidth + commentWidth
    in
    { local = Encode.sequence [ step.local, localFile ]
    , directory = Encode.sequence [ step.directory, record ]
    , position = step.position + localFileWidth
    , directorySize = step.directorySize + recordWidth
    }


compressionMethod : CompressionMethod -> Encoder
compressionMethod method =
    i16 <|
        case method of
            Stored ->
                0

            Deflated ->
                8

            Unsupported x ->
                x



-- Bytes Helpers


i16 : Int -> Encoder
i16 =
    Encode.unsignedInt16 LE


i32 : Int -> Encoder
i32 =
    Encode.unsignedInt32 LE


noBytes : Bytes
noBytes =
    Encode.encode (Encode.sequence [])
