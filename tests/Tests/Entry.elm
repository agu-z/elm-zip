module Tests.Entry exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Expect exposing (Expectation)
import Test exposing (..)
import Tests.Zip
import Time
import Zip
import Zip.Entry exposing (..)


withSample : String -> (Entry -> Expectation) -> () -> Expectation
withSample name expect =
    Tests.Zip.withSample
        (\zip ->
            case zip |> Zip.byName name of
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


asString : Bytes -> Maybe String
asString bytes =
    let
        decoder =
            Decode.string (Bytes.width bytes)
    in
    Decode.decode decoder bytes


suite : Test
suite =
    describe "Zip.Entry"
        [ describe "fileName"
            [ test "returns the correct value" <|
                versionJson (fileName >> Expect.equal "sample/version.json")
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
        , describe "extract"
            [ test "returns uncompressed data" <|
                versionJson
                    (extract
                        >> Result.toMaybe
                        >> Maybe.andThen asString
                        >> Expect.equal (Just "{ \"required\": 2 }\n")
                    )
            , test "checks integrity" <|
                corrupted (extract >> Expect.equal (Err IntegrityError))
            , test "fails on unsupported compression method" <|
                unsupported (extract >> Expect.equal (Err (UnsupportedCompression 0x0A)))
            , test "fails on corrupted flate data" <|
                corruptedDeflate (extract >> Expect.equal (Err InflateError))
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
