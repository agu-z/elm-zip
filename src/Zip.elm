module Zip exposing
    ( Zip
    , all
    , byName
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


all : Zip -> List Entry
all (Zip _ records) =
    List.map Entry records


byName : String -> Zip -> Maybe Entry
byName name (Zip _ records) =
    records
        |> find (\record -> record.fileName == name)
        |> Maybe.map Entry


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
