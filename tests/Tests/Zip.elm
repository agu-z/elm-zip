module Tests.Zip exposing (sameDosTime, suite, withSample)

import Bytes.Encode as Encode
import Expect exposing (Expectation)
import Hex.Convert
import Test exposing (..)
import Time exposing (Posix)
import Zip exposing (Zip)
import Zip.Entry


maybeZip : Maybe Zip
maybeZip =
    "504B0304 14000000 0000E854 2B520000 00000000 00000000 00000700 20007361 6D706C65 2F55540D 00070455 FC5F03C6 FC5F0455 FC5F7578 0B000104 F5010000 04140000 00504B03 04140008 0008005A 552B5200 00000000 00000012 00000013 00200073 616D706C 652F7665 7273696F 6E2E6A73 6F6E5554 0D0007DD 55FC5F46 C6FC5FDD 55FC5F75 780B0001 04F50100 00041400 0000AB56 502A4A2D 2CCD2C4A 4D51B252 3052A8E5 0200504B 0708B4B1 EE2F1400 00001200 0000504B 03041400 00000000 6B552B52 00000000 00000000 00000000 10002000 73616D70 6C652F76 65727369 6F6E732F 55540D00 07FA55FC 5F03C6FC 5FFA55FC 5F75780B 000104F5 01000004 14000000 504B0304 14000800 08005955 2B520000 00000000 00001800 00001200 20007361 6D706C65 2F756E73 7570706F 72746564 55540D00 07DB55FC 5F9663FC 5FDB55FC 5F75780B 000104F5 01000004 14000000 0BCD2B2E 2D28C82F 2A494D51 48CECF2D 284A2D2E CECCCFE3 0200504B 07081DA7 03971A00 00001800 0000504B 03041400 08000800 4D552B52 00000000 00000000 0D000000 10002000 73616D70 6C652F63 6F727275 70746564 55540D00 07C255FC 5F9663FC 5FC255FC 5F75780B 000104F5 01000004 14000000 734A4C51 48CE484D CE2E2ECD E5020050 4B0708D7 A3D80A0F 0000000D 00000050 4B030414 00080008 0054552B 52000000 00000000 00130000 00180020 0073616D 706C652F 636F7272 75707465 645F6465 666C6174 6555540D 0007D155 FC5F9563 FC5FD155 FC5F7578 0B000104 F5010000 04140000 00084A4C 5148494D CB492C49 5548CB2F CA4D2CE1 0200504B 070808D5 FE871500 00001300 0000504B 03041400 00000000 79552B52 00000000 00000000 00000000 15002000 73616D70 6C652F76 65727369 6F6E732F 6D657461 2F55540D 00071656 FC5F1656 FC5F1656 FC5F7578 0B000104 F5010000 04140000 00504B03 04140008 000800DA 4A2B5200 00000000 0000000C 00000016 00200073 616D706C 652F7665 7273696F 6E732F76 312E7478 7455540D 00071D43 FC5F9863 FC5F1D43 FC5F7578 0B000104 F5010000 04140000 00CB48CD C9C95728 CF2FCA49 E1020050 4B07082D 3B08AF0E 0000000C 00000050 4B030414 00080008 0062552B 52000000 00000000 000E0000 00160020 0073616D 706C652F 76657273 696F6E73 2F76322E 74787455 540D0007 E855FC5F 9863FC5F E855FC5F 75780B00 0104F501 00000414 000000CB 48CDC9C9 D75128CF 2FCA4951 E4020050 4B0708C0 DF31B610 0000000E 00000050 4B030414 00080008 0075552B 52000000 00000000 00160000 00210020 0073616D 706C652F 76657273 696F6E73 2F6D6574 612F636F 6D6D656E 74732E74 78745554 0D00070F 56FC5F98 63FC5F16 56FC5F75 780B0001 04F50100 00041400 00000BCE C82FCD49 51284F55 282D4E55 284ECD2D 4B2DB2E7 0200504B 07085E76 68DF1800 00001600 0000504B 01021403 14000000 0000E854 2B520000 00000000 00000000 00000700 20000000 00000000 0000ED41 00000000 73616D70 6C652F55 540D0007 0455FC5F 03C6FC5F 0455FC5F 75780B00 0104F501 00000414 00000050 4B010214 03140008 0008005A 552B52B4 B1EE2F14 00000012 00000013 00200000 00000000 000000A4 81450000 0073616D 706C652F 76657273 696F6E2E 6A736F6E 55540D00 07DD55FC 5F46C6FC 5FDD55FC 5F75780B 000104F5 01000004 14000000 504B0102 14031400 00000000 6B552B52 00000000 00000000 00000000 10002000 00000000 00000000 ED41BA00 00007361 6D706C65 2F766572 73696F6E 732F5554 0D0007FA 55FC5F03 C6FC5FFA 55FC5F75 780B0001 04F50100 00041400 0000504B 01021403 14000800 0A005955 2B521DA7 03971A00 00001800 00001200 20000000 00000000 0000A481 08010000 73616D70 6C652F75 6E737570 706F7274 65645554 0D0007DB 55FC5F96 63FC5FDB 55FC5F75 780B0001 04F50100 00041400 0000504B 01021403 14000800 08004D55 2A52D7A3 D70A0F00 00000D00 00001000 20000000 00000000 0000A481 82010000 73616D70 6C652F63 6F727275 70746564 55540D00 07C255FC 5F9663FC 5FC255FC 5F75780B 000104F5 01000004 14000000 504B0102 14031400 08000800 54552B52 08D5FE87 15000000 13000000 18002000 00000000 00000000 A481EF01 00007361 6D706C65 2F636F72 72757074 65645F64 65666C61 74655554 0D0007D1 55FC5F95 63FC5FD1 55FC5F75 780B0001 04F50100 00041400 0000504B 01021403 14000000 00007955 2B520000 00000000 00000000 00001500 20000000 00000000 0000ED41 6A020000 73616D70 6C652F76 65727369 6F6E732F 6D657461 2F55540D 00071656 FC5F1656 FC5F1656 FC5F7578 0B000104 F5010000 04140000 00504B01 02140314 00080008 00DA4A2B 522D3B08 AF0E0000 000C0000 00160020 00000000 00000000 00A481BD 02000073 616D706C 652F7665 7273696F 6E732F76 312E7478 7455540D 00071D43 FC5F9863 FC5F1D43 FC5F7578 0B000104 F5010000 04140000 00504B01 02140314 00080008 0062552B 52C0DF31 B6100000 000E0000 00160020 00000000 00000000 00A4812F 03000073 616D706C 652F7665 7273696F 6E732F76 322E7478 7455540D 0007E855 FC5F9863 FC5FE855 FC5F7578 0B000104 F5010000 04140000 00504B01 02140314 00080008 0075552B 525E7668 DF180000 00160000 00210020 00000000 00000000 00A481A3 03000073 616D706C 652F7665 7273696F 6E732F6D 6574612F 636F6D6D 656E7473 2E747874 55540D00 070F56FC 5F9863FC 5F1656FC 5F75780B 000104F5 01000004 14000000 504B0506 00000000 0A000A00 D2030000 2A040000 0000"
        |> String.replace " " ""
        |> Hex.Convert.toBytes
        |> Maybe.andThen Zip.fromBytes


withSample : (Zip -> Expectation) -> () -> Expectation
withSample fn () =
    case maybeZip of
        Just zip ->
            fn zip

        Nothing ->
            Expect.fail "failed to decode"


sameDosTime : Posix -> Posix -> Expectation
sameDosTime a b =
    Time.posixToMillis a
        |> toFloat
        |> Expect.within (Expect.Absolute 2000) (Time.posixToMillis b |> toFloat)


suite : Test
suite =
    describe "Zip"
        [ describe "fromBytes"
            [ test "creates a Zip when valid" (withSample <| \_ -> Expect.pass)
            ]
        , describe "ls"
            [ test "returns all entries in the file" <|
                withSample (Zip.entries >> List.length >> Expect.equal 10)
            ]
        , describe "byName"
            [ test "returns just the entry if it exists" <|
                withSample
                    (Zip.getEntry "sample/version.json"
                        >> Maybe.map Zip.Entry.path
                        >> Expect.equal (Just "sample/version.json")
                    )
            , test "returns nothing if it does not exist" <|
                withSample
                    (Zip.getEntry "sample/nonexistent"
                        >> Maybe.map Zip.Entry.path
                        >> Expect.equal Nothing
                    )
            ]
        , describe "count"
            [ test "returns the number of entries in the archive" <|
                withSample (Zip.count >> Expect.equal 10)
            ]
        , describe "isEmpty"
            [ test "returns True if empty" <|
                \_ ->
                    Zip.empty
                        |> Zip.isEmpty
                        |> Expect.equal True
            , test "returns False if not empty" <|
                withSample (Zip.isEmpty >> Expect.equal False)
            ]
        , describe "empty"
            [ test "has no entries" <|
                \_ ->
                    Zip.empty
                        |> Zip.entries
                        |> List.length
                        |> Expect.equal 0
            ]
        , describe "fromEntries"
            [ test "creates an archive with the provided entries" <|
                withSample
                    (\zip ->
                        let
                            entries =
                                Zip.entries zip
                        in
                        Zip.fromEntries entries
                            |> Zip.entries
                            |> Expect.equal entries
                    )
            ]
        , describe "insert"
            [ test "adds an entry to the archive" <|
                withSample
                    (\zip ->
                        case Zip.getEntry "sample/version.json" zip of
                            Just entry ->
                                Zip.empty
                                    |> Zip.insert entry
                                    |> Expect.all
                                        [ Zip.count >> Expect.equal 1
                                        , Zip.getEntry "sample/version.json" >> Expect.equal (Just entry)
                                        ]

                            Nothing ->
                                Expect.fail "Couldn't find entry"
                    )
            , test "replaces entries with the same path" <|
                withSample
                    (\zip ->
                        let
                            newEntry =
                                Zip.Entry.store
                                    { path = "sample/version.json"
                                    , lastModified = ( Time.utc, Time.millisToPosix 0 )
                                    , comment = Nothing
                                    }
                                    (Encode.encode (Encode.string "{ \"current\":  30 }"))
                        in
                        zip
                            |> Zip.insert newEntry
                            |> Expect.all
                                [ Zip.count >> Expect.equal 10
                                , Zip.getEntry "sample/version.json" >> Expect.equal (Just newEntry)
                                ]
                    )
            ]
        , describe "filter" <|
            let
                filtered =
                    Zip.filter (Zip.Entry.path >> String.endsWith ".txt")
            in
            [ test "removes entries for which the predicate returns False" <|
                withSample
                    (filtered
                        >> Expect.all
                            [ Zip.getEntry "sample/version.json" >> Expect.equal Nothing
                            , Zip.getEntry "sample/" >> Expect.equal Nothing
                            , Zip.getEntry "sample/corrupted" >> Expect.equal Nothing
                            ]
                    )
            , test "keeps entries for which the predicate returns True" <|
                withSample
                    (filtered
                        >> Expect.all
                            [ Zip.getEntry "sample/versions/v1.txt" >> Expect.notEqual Nothing
                            , Zip.getEntry "sample/versions/v2.txt" >> Expect.notEqual Nothing
                            , Zip.getEntry "sample/versions/meta/comments.txt" >> Expect.notEqual Nothing
                            ]
                    )
            ]
        , describe "toBytes" <|
            let
                posix =
                    Time.millisToPosix 1611189269538

                hiTxt =
                    Zip.Entry.store
                        { path = "hi.txt"
                        , lastModified = ( Time.utc, posix )
                        , comment = Just "some comment"
                        }
                        (Encode.encode (Encode.string "hello world"))

                nestedHiTxt =
                    Zip.Entry.compress
                        { path = "data/hi.txt"
                        , lastModified = ( Time.utc, Time.millisToPosix 0 )
                        , comment = Nothing
                        }
                        (Encode.encode (Encode.string "hello world"))

                dataDir =
                    Zip.Entry.createDirectory
                        { path = "data/"
                        , lastModified = ( Time.utc, Time.millisToPosix 0 )
                        , comment = Nothing
                        }
            in
            [ test "empty archives" <|
                \_ ->
                    Zip.empty
                        |> Zip.toBytes
                        |> Zip.fromBytes
                        |> Maybe.map Zip.isEmpty
                        |> Expect.equal (Just True)
            , test "stored files" <|
                \_ ->
                    let
                        maybeEntry =
                            Zip.empty
                                |> Zip.insert hiTxt
                                |> Zip.toBytes
                                |> Zip.fromBytes
                                |> Maybe.andThen (Zip.getEntry "hi.txt")
                    in
                    case maybeEntry of
                        Nothing ->
                            Expect.fail "Entry not found in encoded archive"

                        Just entry ->
                            Expect.all
                                [ Zip.Entry.toString >> Expect.equal (Ok "hello world")
                                , Zip.Entry.lastModified Time.utc >> sameDosTime posix
                                , Zip.Entry.comment >> Expect.equal "some comment"
                                ]
                                entry
            , test "stored under directories" <|
                \_ ->
                    Zip.empty
                        |> Zip.insert nestedHiTxt
                        |> Zip.toBytes
                        |> Zip.fromBytes
                        |> Maybe.andThen (Zip.getEntry "data/hi.txt")
                        |> Maybe.map Zip.Entry.toString
                        |> Expect.equal (Just <| Ok "hello world")
            , test "directory entries" <|
                \_ ->
                    Zip.empty
                        |> Zip.insert dataDir
                        |> Zip.toBytes
                        |> Zip.fromBytes
                        |> Maybe.andThen (Zip.getEntry "data/")
                        |> Maybe.map Zip.Entry.isDirectory
                        |> Expect.equal (Just True)
            , test "multiple entries" <|
                \_ ->
                    Zip.fromEntries [ hiTxt, nestedHiTxt, dataDir ]
                        |> Zip.toBytes
                        |> Zip.fromBytes
                        |> Maybe.map Zip.count
                        |> Expect.equal (Just 3)
            ]
        ]
