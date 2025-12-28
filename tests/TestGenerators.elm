module TestGenerators exposing
    ( albumIdGenerator
    , albumNameGenerator
    , assetIdGenerator
    , categorisationFilterGenerator
    , durationStringGenerator
    , imageOrderGenerator
    , keybindingGenerator
    , mediaTypeFilterGenerator
    , statusFilterGenerator
    , testAlbumGenerator
    , testAssetGenerator
    )

import Date exposing (Date)
import Fuzz exposing (Fuzzer, andMap, bool, constant, int, list, map, map2, map3, map4, map5, maybe, oneOf, string)
import Immich exposing (CategorisationFilter(..), ImageOrder(..), ImmichAlbum, ImmichAsset, MediaTypeFilter(..), StatusFilter(..))


{-| Generate valid-looking asset IDs (UUID-like)
-}
assetIdGenerator : Fuzzer String
assetIdGenerator =
    map
        (\suffix -> "asset-" ++ String.fromInt (abs suffix))
        int


{-| Generate valid-looking album IDs (UUID-like)
-}
albumIdGenerator : Fuzzer String
albumIdGenerator =
    map
        (\suffix -> "album-" ++ String.fromInt (abs suffix))
        int


{-| Generate album names with various characters
-}
albumNameGenerator : Fuzzer String
albumNameGenerator =
    oneOf
        [ constant "Vacation 2024"
        , constant "Family Photos"
        , constant "Test Album"
        , constant "Nature & Wildlife"
        , constant "Screenshots"
        , constant "Work Documents"
        , constant "Random Collection"
        , string
        ]


{-| Generate lowercase alphanumeric keybindings
-}
keybindingGenerator : Fuzzer String
keybindingGenerator =
    oneOf
        [ constant "a"
        , constant "b"
        , constant "c"
        , constant "d"
        , constant "f"
        , constant "g"
        , constant "h"
        , constant "j"
        , constant "k"
        , constant "l"
        , constant "m"
        , constant "n"
        , constant "p"
        , constant "q"
        , constant "r"
        , constant "s"
        , constant "t"
        , constant "u"
        , constant "v"
        , constant "w"
        , constant "x"
        , constant "y"
        , constant "z"
        , constant "1"
        , constant "2"
        , constant "3"
        , constant "4"
        , constant "5"
        , constant "6"
        , constant "7"
        , constant "8"
        , constant "9"
        , constant "0"
        ]


{-| Generate ImageOrder enum variants
-}
imageOrderGenerator : Fuzzer ImageOrder
imageOrderGenerator =
    oneOf
        [ constant CreatedDesc
        , constant CreatedAsc
        , constant ModifiedDesc
        , constant ModifiedAsc
        , constant Random
        , constant DurationAsc
        , constant DurationDesc
        ]


{-| Generate MediaTypeFilter enum variants
-}
mediaTypeFilterGenerator : Fuzzer MediaTypeFilter
mediaTypeFilterGenerator =
    oneOf
        [ constant AllMedia
        , constant ImagesOnly
        , constant VideosOnly
        ]


{-| Generate StatusFilter enum variants
-}
statusFilterGenerator : Fuzzer StatusFilter
statusFilterGenerator =
    oneOf
        [ constant AllStatuses
        , constant FavoritesOnly
        , constant ArchivedOnly
        ]


{-| Generate CategorisationFilter enum variants
-}
categorisationFilterGenerator : Fuzzer CategorisationFilter
categorisationFilterGenerator =
    oneOf
        [ constant All
        , constant Uncategorised
        ]


{-| Generate duration strings like "0:01:23.456"
Formats include:

  - "H:MM:SS.mmm"
  - "0:MM:SS.mmm"
  - "MM:SS"
  - "SS"

-}
durationStringGenerator : Fuzzer (Maybe String)
durationStringGenerator =
    let
        -- Generate hours (0-9)
        hoursGen =
            map (\h -> abs h |> modBy 10) int

        -- Generate minutes (0-59)
        minutesGen =
            map (\m -> abs m |> modBy 60) int

        -- Generate seconds (0-59)
        secondsGen =
            map (\s -> abs s |> modBy 60) int

        -- Generate milliseconds (0-999)
        millisGen =
            map (\ms -> abs ms |> modBy 1000) int

        -- Format with leading zeros
        pad2 n =
            if n < 10 then
                "0" ++ String.fromInt n

            else
                String.fromInt n

        pad3 n =
            if n < 10 then
                "00" ++ String.fromInt n

            else if n < 100 then
                "0" ++ String.fromInt n

            else
                String.fromInt n

        -- Full format: H:MM:SS.mmm
        fullFormat =
            map4
                (\h m s ms ->
                    String.fromInt h ++ ":" ++ pad2 m ++ ":" ++ pad2 s ++ "." ++ pad3 ms
                )
                hoursGen
                minutesGen
                secondsGen
                millisGen

        -- Short format: MM:SS
        shortFormat =
            map2
                (\m s -> pad2 m ++ ":" ++ pad2 s)
                minutesGen
                secondsGen

        -- Very short: SS
        veryShortFormat =
            map String.fromInt secondsGen
    in
    maybe
        (oneOf
            [ fullFormat
            , shortFormat
            , veryShortFormat
            ]
        )


{-| Generate test assets with random data
-}
testAssetGenerator : Fuzzer ImmichAsset
testAssetGenerator =
    let
        -- Generate file extensions
        extensionGen =
            oneOf
                [ constant ".jpg"
                , constant ".jpeg"
                , constant ".png"
                , constant ".gif"
                , constant ".mp4"
                , constant ".mov"
                , constant ".avi"
                ]

        -- Generate MIME types
        mimeTypeGen =
            oneOf
                [ constant "image/jpeg"
                , constant "image/png"
                , constant "image/gif"
                , constant "video/mp4"
                , constant "video/quicktime"
                , constant "video/x-msvideo"
                ]

        -- Generate dates (using RataDie between 2020-2025)
        dateGen =
            map (\days -> Date.fromRataDie (737790 + abs (modBy 1826 days))) int

        -- Generate timestamp strings
        timestampGen =
            map
                (\days ->
                    let
                        year =
                            2020 + (abs days |> modBy 5)

                        month =
                            1 + (abs days |> modBy 12)

                        day =
                            1 + (abs days |> modBy 28)

                        pad n =
                            if n < 10 then
                                "0" ++ String.fromInt n

                            else
                                String.fromInt n
                    in
                    String.fromInt year ++ "-" ++ pad month ++ "-" ++ pad day ++ "T12:00:00.000Z"
                )
                int

        -- Generate thumbhash
        thumbhashGen =
            maybe string

        -- Constructor function for the full ImmichAsset
        assetConstructor id fileName ext mimeType isFav isArch membership createdDate modifiedDate createdStr modifiedStr thumb dur =
            { id = id
            , path = "/path/to/" ++ fileName ++ ext
            , title = fileName ++ ext
            , mimeType = mimeType
            , isFavourite = isFav
            , isArchived = isArch
            , albumMembership = membership
            , fileCreatedAt = createdDate
            , fileModifiedAt = modifiedDate
            , fileCreatedAtString = createdStr
            , fileModifiedAtString = modifiedStr
            , thumbhash = thumb
            , duration = dur
            }
    in
    Fuzz.map assetConstructor assetIdGenerator
        |> andMap string
        |> andMap extensionGen
        |> andMap mimeTypeGen
        |> andMap bool
        |> andMap bool
        |> andMap (Fuzz.listOfLengthBetween 0 3 albumIdGenerator)
        |> andMap dateGen
        |> andMap dateGen
        |> andMap timestampGen
        |> andMap timestampGen
        |> andMap thumbhashGen
        |> andMap durationStringGenerator


{-| Generate test albums with random data
-}
testAlbumGenerator : Fuzzer ImmichAlbum
testAlbumGenerator =
    let
        -- Generate asset count (0-100)
        assetCountGen =
            map (\count -> abs count |> modBy 101) int

        -- Generate dates (using RataDie between 2020-2025)
        dateGen =
            map (\days -> Date.fromRataDie (737790 + abs (modBy 1826 days))) int

        -- Generate a small list of assets (0-3 assets for testing) - using bounded list to avoid exponential blowup
        assetsGen =
            Fuzz.listOfLengthBetween 0 3 testAssetGenerator
    in
    map5 ImmichAlbum
        albumIdGenerator
        albumNameGenerator
        assetCountGen
        assetsGen
        dateGen
