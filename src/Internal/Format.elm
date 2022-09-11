module Internal.Format exposing
    ( CompressionMethod(..)
    , Entry(..)
    , EntryBytes(..)
    , EntryMeta
    )

import Bytes exposing (Bytes)


type CompressionMethod
    = Stored
    | Deflated
    | Unsupported Int


type alias EntryMeta =
    { madeBy : Int
    , extractMinVersion : Int
    , flag : Int
    , compressionMethod : CompressionMethod
    , lastModified : Int
    , crc32 : Int
    , compressedSize : Int
    , uncompressedSize : Int
    , fileName : String
    , extraField : Bytes
    , comment : String
    , internalAttributes : Int
    , externalAttributes : Int
    }


type EntryBytes
    = Exactly Bytes
    | Offset Bytes Int


type Entry
    = Entry EntryBytes EntryMeta
