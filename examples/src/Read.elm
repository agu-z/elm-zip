module Read exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download
import File.Select
import Task
import Zip exposing (Zip)
import Zip.Entry as Entry exposing (Entry)


type Model
    = NoFile
    | BadFile
    | GoodFile Zip


init : () -> ( Model, Cmd Msg )
init () =
    ( NoFile, Cmd.none )


type Msg
    = SelectFile
    | GotFile File
    | GotZip (Maybe Zip)
    | DownloadEntry Entry


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectFile ->
            ( model, File.Select.file [ "application/zip" ] GotFile )

        GotFile file ->
            ( model
            , file
                |> File.toBytes
                |> Task.map Zip.fromBytes
                |> Task.perform GotZip
            )

        GotZip maybeZip ->
            case maybeZip of
                Just zip ->
                    ( GoodFile zip, Cmd.none )

                Nothing ->
                    ( BadFile, Cmd.none )

        DownloadEntry entry ->
            case Entry.toBytes entry of
                Ok bytes ->
                    let
                        name =
                            Entry.basename entry
                    in
                    ( model, File.Download.bytes name "" bytes )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Element Msg
view model =
    column [ centerX, centerY, spacing 20 ]
        [ case model of
            NoFile ->
                none

            BadFile ->
                text "Failed to read file"

            GoodFile zip ->
                let
                    entries =
                        zip
                            |> Zip.entries
                            |> List.filter (not << Entry.isDirectory)
                            |> List.map entryItem
                in
                column
                    [ Background.color <| rgba 0 0 0 0.2
                    , Border.rounded 5
                    , padding 20
                    , spacing 20
                    ]
                    [ row [ spacing 10, width fill ]
                        [ el [ Font.size 24 ] (text "Archive Files")
                        , el [ Font.size 12, alignRight ] (text "Click a file to download it")
                        ]
                    , column
                        [ spacing 10
                        , scrollbars
                        ]
                        entries
                    ]
        , Input.button
            [ centerX
            , padding 14
            , Font.color <| rgb255 0x15 0x1E 0x2D
            , Background.color <| rgb255 0xC5 0x94 0xC5
            , Border.rounded 3
            ]
            { label =
                if model == NoFile then
                    text "Select a .zip file"

                else
                    text "Select another .zip"
            , onPress = Just SelectFile
            }
        ]


entryItem : Entry -> Element Msg
entryItem entry =
    Input.button [ Font.size 16, padding 2 ]
        { label = text <| Entry.path entry
        , onPress = Just <| DownloadEntry entry
        }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view =
            layout
                [ width fill
                , height fill
                , Font.color <| rgb 1 1 1
                , Background.color <| rgb255 0x15 0x1E 0x2D
                ]
                << view
        , subscriptions = always Sub.none
        }
