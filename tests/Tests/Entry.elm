module Tests.Entry exposing (suite)

import Expect exposing (Expectation)
import Hex.Convert
import Test exposing (..)
import Tests.Zip
import Time
import Zip
import Zip.Entry exposing (..)


withSample : String -> (Entry -> Expectation) -> () -> Expectation
withSample name expect =
    Tests.Zip.withSample
        (\zip ->
            case zip |> Zip.byPath name of
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


suite : Test
suite =
    describe "Zip.Entry"
        [ describe "path"
            [ test "returns the correct value" <|
                versionJson (path >> Expect.equal "sample/version.json")
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
                unsupported (toBytes >> Expect.equal (Err (UnsupportedCompression 0x0A)))
            , test "fails on corrupted flate data" <|
                corruptedDeflate (toBytes >> Expect.equal (Err InflateError))
            ]
        , describe "extractWith"
            [ test "allows falling back on unsupported compression" <|
                unsupported
                    (extractWith
                        (\{ method, rawBytes } ->
                            if method == 0x0A then
                                Just rawBytes

                            else
                                Nothing
                        )
                        >> Expect.equal (Err IntegrityError)
                    )
            ]
        ]
