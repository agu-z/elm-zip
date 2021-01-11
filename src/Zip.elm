module Zip exposing
    ( Zip
    , byName
    , fromBytes
    , ls
    )

import Bytes exposing (Bytes)
import Internal.Format exposing (CdRecord, Entry(..), readDirectory)


type Zip
    = Zip Bytes (List CdRecord)


fromBytes : Bytes -> Maybe Zip
fromBytes bytes =
    readDirectory bytes
        |> Maybe.map (\( data, records ) -> Zip data records)


ls : Zip -> List Entry
ls (Zip allBytes records) =
    List.map (Entry allBytes) records


byName : String -> Zip -> Maybe Entry
byName name (Zip allBytes records) =
    records
        |> find (\record -> record.fileName == name)
        |> Maybe.map (Entry allBytes)


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
