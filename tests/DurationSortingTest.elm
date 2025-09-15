module DurationSortingTest exposing (..)

import Expect
import Helpers exposing (toggleOrder, toggleOrderHandler)
import Immich exposing (ImageOrder(..), ImmichAsset, MediaTypeFilter(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Duration Sorting Tests"
        [ describe "toggleOrderHandler"
            [ test "cycles through duration options when MediaTypeFilter is VideosOnly" <|
                \_ ->
                    let
                        startOrder =
                            CreatedDesc

                        cycle1 =
                            toggleOrderHandler VideosOnly startOrder

                        cycle2 =
                            toggleOrderHandler VideosOnly cycle1

                        cycle3 =
                            toggleOrderHandler VideosOnly cycle2

                        cycle4 =
                            toggleOrderHandler VideosOnly cycle3

                        cycle5 =
                            toggleOrderHandler VideosOnly cycle4

                        cycle6 =
                            toggleOrderHandler VideosOnly cycle5

                        cycle7 =
                            toggleOrderHandler VideosOnly cycle6

                        cycle8 =
                            toggleOrderHandler VideosOnly cycle7
                    in
                    Expect.all
                        [ \_ -> Expect.equal cycle1 CreatedAsc
                        , \_ -> Expect.equal cycle2 ModifiedDesc
                        , \_ -> Expect.equal cycle3 ModifiedAsc
                        , \_ -> Expect.equal cycle4 DurationAsc
                        , \_ -> Expect.equal cycle5 DurationDesc
                        , \_ -> Expect.equal cycle6 Random
                        , \_ -> Expect.equal cycle7 CreatedDesc
                        , \_ -> Expect.equal cycle8 CreatedAsc
                        ]
                        ()
            , test "cycles without duration options when MediaTypeFilter is AllMedia" <|
                \_ ->
                    let
                        startOrder =
                            CreatedDesc

                        cycle1 =
                            toggleOrderHandler AllMedia startOrder

                        cycle2 =
                            toggleOrderHandler AllMedia cycle1

                        cycle3 =
                            toggleOrderHandler AllMedia cycle2

                        cycle4 =
                            toggleOrderHandler AllMedia cycle3

                        cycle5 =
                            toggleOrderHandler AllMedia cycle4

                        cycle6 =
                            toggleOrderHandler AllMedia cycle5
                    in
                    Expect.all
                        [ \_ -> Expect.equal cycle1 CreatedAsc
                        , \_ -> Expect.equal cycle2 ModifiedDesc
                        , \_ -> Expect.equal cycle3 ModifiedAsc
                        , \_ -> Expect.equal cycle4 Random
                        , \_ -> Expect.equal cycle5 CreatedDesc
                        , \_ -> Expect.equal cycle6 CreatedAsc
                        ]
                        ()
            , test "cycles without duration options when MediaTypeFilter is ImagesOnly" <|
                \_ ->
                    let
                        startOrder =
                            CreatedDesc

                        cycle1 =
                            toggleOrderHandler ImagesOnly startOrder

                        cycle2 =
                            toggleOrderHandler ImagesOnly cycle1

                        cycle3 =
                            toggleOrderHandler ImagesOnly cycle2

                        cycle4 =
                            toggleOrderHandler ImagesOnly cycle3

                        cycle5 =
                            toggleOrderHandler ImagesOnly cycle4

                        cycle6 =
                            toggleOrderHandler ImagesOnly cycle5
                    in
                    Expect.all
                        [ \_ -> Expect.equal cycle1 CreatedAsc
                        , \_ -> Expect.equal cycle2 ModifiedDesc
                        , \_ -> Expect.equal cycle3 ModifiedAsc
                        , \_ -> Expect.equal cycle4 Random
                        , \_ -> Expect.equal cycle5 CreatedDesc
                        , \_ -> Expect.equal cycle6 CreatedAsc
                        ]
                        ()
            , test "handles DurationAsc and DurationDesc correctly in regular toggle" <|
                \_ ->
                    let
                        fromDurationAsc =
                            toggleOrder DurationAsc

                        fromDurationDesc =
                            toggleOrder DurationDesc
                    in
                    Expect.all
                        [ \_ -> Expect.equal fromDurationAsc DurationDesc
                        , \_ -> Expect.equal fromDurationDesc CreatedDesc
                        ]
                        ()
            ]
        , describe "Duration sorting edge cases"
            [ test "parseDurationToSeconds handles various duration formats" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal (Immich.parseDurationToSeconds "00:00:05") (Just 5)
                        , \_ -> Expect.equal (Immich.parseDurationToSeconds "00:01:30") (Just 90)
                        , \_ -> Expect.equal (Immich.parseDurationToSeconds "01:00:00") (Just 3600)
                        , \_ -> Expect.equal (Immich.parseDurationToSeconds "01:23:45") (Just 5025)
                        , \_ -> Expect.equal (Immich.parseDurationToSeconds "00:00:05.123") (Just 5)
                        , \_ -> Expect.equal (Immich.parseDurationToSeconds "invalid") Nothing
                        , \_ -> Expect.equal (Immich.parseDurationToSeconds "") Nothing
                        ]
                        ()
            ]
        ]
