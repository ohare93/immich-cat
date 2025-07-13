module FilteringTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Fuzz exposing (..)
import Date

-- Import the main module to test filtering functions
import Main exposing 
    ( MediaTypeFilter(..)
    , StatusFilter(..)
    , SearchContext(..)
    , TimelineConfig
    , SearchConfig
    , AlbumConfig
    , filterByMediaType
    , filterByStatus
    , applyTimelineFilters
    , applySearchFilters
    , applyAlbumFilters
    , defaultTimelineConfig
    , defaultSearchConfig
    , defaultAlbumConfig
    )
import Immich exposing (ImmichAsset, ImmichAlbum, ImageOrder(..), CategorisationFilter(..))

-- Test data generators
generateTestAsset : String -> String -> String -> Bool -> Bool -> ImmichAsset
generateTestAsset id fileName mimeType isFavorite isArchived =
    { id = id
    , path = "/path/to/" ++ fileName
    , title = fileName
    , mimeType = mimeType
    , isFavourite = isFavorite
    , isArchived = isArchived
    , albumMembership = []
    }

generateTestAlbum : String -> String -> Int -> ImmichAlbum
generateTestAlbum id name assetCount =
    { id = id
    , albumName = name
    , assetCount = assetCount
    , assets = []
    , createdAt = Date.fromRataDie 737790 -- January 1, 2020
    }

suite : Test
suite =
    describe "New Filtering System Tests"
        [ describe "MediaTypeFilter Tests"
            [ test "AllMedia filter includes both images and videos" <|
                \_ ->
                    let
                        testAssets =
                            [ generateTestAsset "1" "photo.jpg" "image/jpeg" False False
                            , generateTestAsset "2" "video.mp4" "video/mp4" False False
                            , generateTestAsset "3" "picture.png" "image/png" False False
                            ]
                        filtered = filterByMediaType AllMedia testAssets
                    in
                    Expect.equal (List.length filtered) 3

            , test "ImagesOnly filter excludes videos" <|
                \_ ->
                    let
                        testAssets =
                            [ generateTestAsset "1" "photo.jpg" "image/jpeg" False False
                            , generateTestAsset "2" "video.mp4" "video/mp4" False False
                            , generateTestAsset "3" "picture.png" "image/png" False False
                            ]
                        filtered = filterByMediaType ImagesOnly testAssets
                        filteredMimeTypes = List.map .mimeType filtered
                    in
                    Expect.all
                        [ \() -> Expect.equal (List.length filtered) 2
                        , \() -> Expect.equal True 
                            (List.all (\mimeType -> String.startsWith "image/" mimeType) filteredMimeTypes)
                        ]
                        ()

            , test "VideosOnly filter excludes images" <|
                \_ ->
                    let
                        testAssets =
                            [ generateTestAsset "1" "photo.jpg" "image/jpeg" False False
                            , generateTestAsset "2" "video.mp4" "video/mp4" False False
                            , generateTestAsset "3" "movie.avi" "video/avi" False False
                            ]
                        filtered = filterByMediaType VideosOnly testAssets
                        filteredMimeTypes = List.map .mimeType filtered
                    in
                    Expect.all
                        [ \() -> Expect.equal (List.length filtered) 2
                        , \() -> Expect.equal True 
                            (List.all (\mimeType -> String.startsWith "video/" mimeType) filteredMimeTypes)
                        ]
                        ()
            ]

        , describe "StatusFilter Tests"
            [ test "AllStatuses filter includes favorites, archived, and normal assets" <|
                \_ ->
                    let
                        testAssets =
                            [ generateTestAsset "1" "normal.jpg" "image/jpeg" False False
                            , generateTestAsset "2" "favorite.jpg" "image/jpeg" True False
                            , generateTestAsset "3" "archived.jpg" "image/jpeg" False True
                            , generateTestAsset "4" "both.jpg" "image/jpeg" True True
                            ]
                        filtered = filterByStatus AllStatuses testAssets
                    in
                    Expect.equal (List.length filtered) 4

            , test "FavoritesOnly filter includes only favorited assets" <|
                \_ ->
                    let
                        testAssets =
                            [ generateTestAsset "1" "normal.jpg" "image/jpeg" False False
                            , generateTestAsset "2" "favorite.jpg" "image/jpeg" True False
                            , generateTestAsset "3" "archived.jpg" "image/jpeg" False True
                            , generateTestAsset "4" "both.jpg" "image/jpeg" True True
                            ]
                        filtered = filterByStatus FavoritesOnly testAssets
                        allAreFavorites = List.all .isFavourite filtered
                    in
                    Expect.all
                        [ \() -> Expect.equal (List.length filtered) 2
                        , \() -> Expect.equal True allAreFavorites
                        ]
                        ()

            , test "ArchivedOnly filter includes only archived assets" <|
                \_ ->
                    let
                        testAssets =
                            [ generateTestAsset "1" "normal.jpg" "image/jpeg" False False
                            , generateTestAsset "2" "favorite.jpg" "image/jpeg" True False
                            , generateTestAsset "3" "archived.jpg" "image/jpeg" False True
                            , generateTestAsset "4" "both.jpg" "image/jpeg" True True
                            ]
                        filtered = filterByStatus ArchivedOnly testAssets
                        allAreArchived = List.all .isArchived filtered
                    in
                    Expect.all
                        [ \() -> Expect.equal (List.length filtered) 2
                        , \() -> Expect.equal True allAreArchived
                        ]
                        ()
            ]

        , describe "Combined Filter Tests"
            [ test "MediaType and Status filters work together" <|
                \_ ->
                    let
                        testAssets =
                            [ generateTestAsset "1" "photo.jpg" "image/jpeg" True False
                            , generateTestAsset "2" "video.mp4" "video/mp4" True False
                            , generateTestAsset "3" "picture.png" "image/png" False False
                            , generateTestAsset "4" "movie.avi" "video/avi" False True
                            ]
                        -- Filter for favorite images only
                        mediaFiltered = filterByMediaType ImagesOnly testAssets
                        combinedFiltered = filterByStatus FavoritesOnly mediaFiltered
                    in
                    Expect.all
                        [ \() -> Expect.equal (List.length combinedFiltered) 1
                        , \() -> 
                            case List.head combinedFiltered of
                                Just asset -> 
                                    Expect.all
                                        [ \() -> Expect.equal True asset.isFavourite
                                        , \() -> Expect.equal True (String.startsWith "image/" asset.mimeType)
                                        ]
                                        ()
                                Nothing -> 
                                    Expect.fail "Should have one filtered asset"
                        ]
                        ()
            ]

        , describe "Default Config Tests"
            [ test "defaultTimelineConfig has expected values" <|
                \_ ->
                    let
                        config = defaultTimelineConfig
                    in
                    Expect.all
                        [ \() -> Expect.equal config.mediaType AllMedia
                        , \() -> Expect.equal config.categorisation Uncategorised
                        , \() -> Expect.equal config.order Desc
                        , \() -> Expect.equal config.status AllStatuses
                        ]
                        ()

            , test "defaultSearchConfig has expected values" <|
                \_ ->
                    let
                        config = defaultSearchConfig
                    in
                    Expect.all
                        [ \() -> Expect.equal config.mediaType AllMedia
                        , \() -> Expect.equal config.searchContext ContentSearch
                        , \() -> Expect.equal config.status AllStatuses
                        , \() -> Expect.equal config.query ""
                        ]
                        ()

            , test "defaultAlbumConfig has expected values" <|
                \_ ->
                    let
                        config = defaultAlbumConfig
                    in
                    Expect.all
                        [ \() -> Expect.equal config.mediaType AllMedia
                        , \() -> Expect.equal config.order Desc
                        , \() -> Expect.equal config.status AllStatuses
                        ]
                        ()
            ]

        , describe "Edge Case Tests"
            [ test "Empty asset list returns empty list for any filter" <|
                \_ ->
                    let
                        emptyAssets = []
                        mediaFiltered = filterByMediaType ImagesOnly emptyAssets
                        statusFiltered = filterByStatus FavoritesOnly emptyAssets
                    in
                    Expect.all
                        [ \() -> Expect.equal (List.length mediaFiltered) 0
                        , \() -> Expect.equal (List.length statusFiltered) 0
                        ]
                        ()

            , test "Assets with unknown mime types are handled gracefully" <|
                \_ ->
                    let
                        testAssets =
                            [ generateTestAsset "1" "unknown.xyz" "application/unknown" False False
                            , generateTestAsset "2" "photo.jpg" "image/jpeg" False False
                            ]
                        -- Unknown mime types should not match image or video filters
                        imageFiltered = filterByMediaType ImagesOnly testAssets
                        videoFiltered = filterByMediaType VideosOnly testAssets
                        allFiltered = filterByMediaType AllMedia testAssets
                    in
                    Expect.all
                        [ \() -> Expect.equal (List.length imageFiltered) 1
                        , \() -> Expect.equal (List.length videoFiltered) 0
                        , \() -> Expect.equal (List.length allFiltered) 2
                        ]
                        ()
            ]
        ]