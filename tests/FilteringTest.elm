module FilteringTest exposing (..)

-- Import the types from Immich and Menus modules

import Expect
import Immich exposing (CategorisationFilter(..), ImageOrder(..), MediaTypeFilter(..), SearchContext(..), StatusFilter(..))
import Menus
    exposing
        ( defaultAlbumConfig
        , defaultSearchConfig
        , defaultTimelineConfig
        )
import Test exposing (..)



-- Test data generators


suite : Test
suite =
    describe "Configuration Tests"
        [ describe "Default Config Tests"
            [ test "defaultTimelineConfig has expected values" <|
                \_ ->
                    let
                        config =
                            defaultTimelineConfig
                    in
                    Expect.all
                        [ \() -> Expect.equal config.mediaType AllMedia
                        , \() -> Expect.equal config.categorisation Uncategorised
                        , \() -> Expect.equal config.order CreatedDesc
                        , \() -> Expect.equal config.status AllStatuses
                        ]
                        ()
            , test "defaultSearchConfig has expected values" <|
                \_ ->
                    let
                        config =
                            defaultSearchConfig
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
                        config =
                            defaultAlbumConfig
                    in
                    Expect.all
                        [ \() -> Expect.equal config.mediaType AllMedia
                        , \() -> Expect.equal config.order CreatedDesc
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
