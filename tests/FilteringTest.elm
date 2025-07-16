module FilteringTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Fuzz exposing (..)
import Date

-- Import the types from Immich and Menus modules  
import Menus exposing 
    ( SearchContext(..)
    , TimelineConfig
    , SearchConfig
    , AlbumConfig
    , defaultTimelineConfig
    , defaultSearchConfig
    , defaultAlbumConfig
    )
import Immich exposing (ImmichAsset, ImmichAlbum, ImageOrder(..), CategorisationFilter(..), MediaTypeFilter(..), StatusFilter(..))

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
    , fileCreatedAt = Date.fromRataDie 737790 -- January 1, 2020
    , thumbhash = Nothing
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
    describe "Configuration Tests"
        [ describe "Default Config Tests"
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
                        , \() -> Expect.equal config.inputFocused False
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

        , describe "Filter Type Tests"
            [ test "MediaTypeFilter values exist" <|
                \_ ->
                    Expect.all
                        [ \() -> Expect.notEqual AllMedia ImagesOnly
                        , \() -> Expect.notEqual ImagesOnly VideosOnly
                        , \() -> Expect.notEqual VideosOnly AllMedia
                        ]
                        ()

            , test "StatusFilter values exist" <|
                \_ ->
                    Expect.all
                        [ \() -> Expect.notEqual AllStatuses FavoritesOnly
                        , \() -> Expect.notEqual FavoritesOnly ArchivedOnly
                        , \() -> Expect.notEqual ArchivedOnly AllStatuses
                        ]
                        ()

            , test "SearchContext values exist" <|
                \_ ->
                    Expect.all
                        [ \() -> Expect.notEqual ContentSearch FilenameSearch
                        , \() -> Expect.notEqual FilenameSearch DescriptionSearch
                        , \() -> Expect.notEqual DescriptionSearch ContentSearch
                        ]
                        ()
            ]
        ]