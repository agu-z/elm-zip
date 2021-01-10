module Zip exposing
    ( Zip
    , entries
    , fromBytes
    )

import Bytes exposing (Bytes)
import Internal.Format exposing (CdRecord, Entry(..), readDirectory)


type Zip
    = Zip Bytes (List CdRecord)


fromBytes : Bytes -> Maybe Zip
fromBytes bytes =
    readDirectory bytes
        |> Maybe.map (\( data, records ) -> Zip data records)


entries : Zip -> List Entry
entries (Zip _ records) =
    List.map Entry records
