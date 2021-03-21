module Tests.Entry exposing (suite)

import Bytes.Encode as Encode
import Expect exposing (Expectation)
import Hex.Convert
import Test exposing (..)
import Tests.Zip
import Time exposing (Posix, Zone)
import Zip
import Zip.Entry exposing (..)


withSample : String -> (Entry -> Expectation) -> () -> Expectation
withSample name expect =
    Tests.Zip.withSample
        (\zip ->
            case zip |> Zip.getEntry name of
                Just entry ->
                    expect entry

                Nothing ->
                    Expect.fail ("Failed to load entry: " ++ name)
        )


versionsDir : (Entry -> Expectation) -> () -> Expectation
versionsDir =
    withSample "sample/versions/"


versionJson : (Entry -> Expectation) -> () -> Expectation
versionJson =
    withSample "sample/version.json"


unsupported : (Entry -> Expectation) -> () -> Expectation
unsupported =
    withSample "sample/unsupported"


corrupted : (Entry -> Expectation) -> () -> Expectation
corrupted =
    withSample "sample/corrupted"


corruptedDeflate : (Entry -> Expectation) -> () -> Expectation
corruptedDeflate =
    withSample "sample/corrupted_deflate"


v1 : (Entry -> Expectation) -> () -> Expectation
v1 =
    withSample "sample/versions/v1.txt"


testEntryMeta : Meta -> Entry -> List Test
testEntryMeta meta entry =
    [ test "keeps the right path" <|
        \_ ->
            entry
                |> path
                |> Expect.equal meta.path
    , test "keeps the right comment" <|
        \_ ->
            entry
                |> comment
                |> Just
                |> Expect.equal meta.comment
    , test "keeps the right timestamp" <|
        \_ ->
            entry
                |> lastModified (Tuple.first meta.lastModified)
                |> Tests.Zip.sameDosTime (Tuple.second meta.lastModified)
    ]


timestamp : ( Zone, Posix )
timestamp =
    ( Time.utc, Time.millisToPosix 1611189269538 )


suite : Test
suite =
    describe "Zip.Entry"
        [ describe "path"
            [ test "returns the correct value" <|
                versionJson (path >> Expect.equal "sample/version.json")
            ]
        , describe "basename"
            [ test "works with nested files" <|
                v1 (basename >> Expect.equal "v1.txt")
            , test "works with nested directories" <|
                withSample "sample/versions/" (basename >> Expect.equal "versions")
            , test "works with root entries" <|
                withSample "sample/" (basename >> Expect.equal "sample")
            ]
        , describe "extractedSize"
            [ test "returns the correct value" <|
                v1 (extractedSize >> Expect.equal 12)
            ]
        , describe "compressedSize"
            [ test "returns the correct value" <|
                v1 (compressedSize >> Expect.equal 14)
            ]
        , describe "lastModified"
            [ test "returns the correct value" <|
                versionJson (lastModified Time.utc >> Expect.equal (Time.millisToPosix 1610361772000))
            ]
        , describe "comment"
            [ test "returns the correct value" <|
                versionJson (comment >> Expect.equal "")
            ]
        , describe "isDirectory"
            [ test "returns False if file" <|
                versionJson (isDirectory >> Expect.equal False)
            , test "returns True if directory" <|
                versionsDir (isDirectory >> Expect.equal True)
            ]
        , describe "checksum"
            [ test "returns the correct value" <|
                versionJson (checksum >> Expect.equal 804172212)
            ]
        , describe "extracting"
            [ test "returns uncompressed text" <|
                versionJson
                    (toString
                        >> Result.toMaybe
                        >> Expect.equal (Just "{ \"required\": 2 }\n")
                    )
            , test "returns uncompressed bytes" <|
                v1
                    (toBytes
                        >> Result.toMaybe
                        >> Expect.equal (Hex.Convert.toBytes "68656C6C6F2C20776F726C64210A")
                    )
            , test "checks integrity" <|
                corrupted (toBytes >> Expect.equal (Err IntegrityError))
            , test "fails on unsupported compression method" <|
                unsupported
                    (\entry ->
                        case toBytes entry of
                            Err (UnsupportedCompression 0x0A _) ->
                                Expect.pass

                            _ ->
                                Expect.fail "Did not fail with Unsupported Compression"
                    )
            , test "fails on corrupted flate data" <|
                corruptedDeflate (toBytes >> Expect.equal (Err InflateError))
            ]
        , describe "store" <|
            let
                meta =
                    { path = "data/hi.txt"
                    , lastModified = timestamp
                    , comment = Just "hello world comment"
                    }

                entry =
                    store meta (Encode.encode <| Encode.string "hello world")
            in
            [ test "keeps the right data" <|
                \_ ->
                    entry
                        |> toString
                        |> Expect.equal (Ok "hello world")
            , test "does not compress" <|
                \_ ->
                    compressedSize entry
                        |> Expect.equal (extractedSize entry)
            , test "does not mark as directory" <|
                \_ ->
                    entry
                        |> isDirectory
                        |> Expect.equal False
            ]
                ++ testEntryMeta meta entry
        , describe "compress" <|
            let
                meta =
                    { path = "data/hi.txt"
                    , lastModified = timestamp
                    , comment = Just "nested file"
                    }

                entry =
                    compress meta (Encode.encode <| Encode.string "hello world")
            in
            [ test "keeps the right data" <|
                \_ ->
                    entry
                        |> toString
                        |> Expect.equal (Ok "hello world")
            , test "does compress" <|
                \_ ->
                    compressedSize entry
                        |> Expect.notEqual (extractedSize entry)
            , test "does not mark as directory" <|
                \_ ->
                    entry
                        |> isDirectory
                        |> Expect.equal False
            ]
                ++ testEntryMeta meta entry
        , describe "createDirectory" <|
            let
                meta =
                    { path = "data/"
                    , lastModified = timestamp
                    , comment = Just "directory"
                    }

                entry =
                    createDirectory meta
            in
            [ test "marks as directory" <|
                \_ ->
                    entry
                        |> isDirectory
                        |> Expect.equal True
            , test "appends slash if missing" <|
                \_ ->
                    createDirectory
                        { path = "data"
                        , lastModified = timestamp
                        , comment = Nothing
                        }
                        |> path
                        |> Expect.equal "data/"
            ]
                ++ testEntryMeta meta entry
        ]
