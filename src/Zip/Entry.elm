module Zip.Entry exposing
    ( Entry
    , comment
    , fileName
    , isDirectory
    , lastModified
    )

import Bitwise
import Date
import Internal.Format as Internal exposing (CdRecord, Entry(..))
import Time exposing (Posix, Zone)
import Time.Extra as Time


type alias Entry =
    Internal.Entry


fileName : Entry -> String
fileName (Entry record) =
    record.fileName


lastModified : Zone -> Entry -> Posix
lastModified timezone (Entry record) =
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
comment (Entry record) =
    record.comment


isDirectory : Entry -> Bool
isDirectory (Entry record) =
    Bitwise.and record.externalAttributes 0x10 /= 0 || String.endsWith "/" record.fileName
