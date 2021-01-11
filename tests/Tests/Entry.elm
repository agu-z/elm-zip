module Tests.Entry exposing (suite)

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


root : (Entry -> Expectation) -> () -> Expectation
root =
    withSample "elm-zip-main/"


gitignore : (Entry -> Expectation) -> () -> Expectation
gitignore =
    withSample "elm-zip-main/.gitignore"


suite : Test
suite =
    describe "Zip.Entry"
        [ describe "fileName"
            [ test "returns the correct value" <|
                gitignore (fileName >> Expect.equal "elm-zip-main/.gitignore")
            ]
        , describe "lastModified"
            [ test "returns the correct value" <|
                gitignore (lastModified Time.utc >> Expect.equal (Time.millisToPosix 1610177800000))
            ]
        , describe "comment"
            [ test "returns the correct value" <|
                gitignore (comment >> Expect.equal "")
            ]
        , describe "isDirectory"
            [ test "returns False if file" <|
                gitignore (isDirectory >> Expect.equal False)
            , test "returns True if directory" <|
                root (isDirectory >> Expect.equal True)
            ]
        ]
