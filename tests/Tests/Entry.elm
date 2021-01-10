module Tests.Entry exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import Tests.Zip
import Time
import Zip
import Zip.Entry exposing (..)


withSample : (Entry -> Expectation) -> () -> Expectation
withSample fn =
    Tests.Zip.withSample
        (\zip ->
            case Zip.entries zip of
                _ :: gitignore :: _ ->
                    fn gitignore

                _ ->
                    Expect.fail "failed to load entry"
        )


testGetter : String -> (Entry -> a) -> a -> Test
testGetter name fn expected =
    describe name
        [ test "returns the correct value" <|
            withSample (fn >> Expect.equal expected)
        ]


suite : Test
suite =
    describe "Zip.Entry"
        [ testGetter "fileName" fileName "elm-zip-main/.gitignore"
        , testGetter "lastModified" (lastModified Time.utc) (Time.millisToPosix 1610177800000)
        ]
