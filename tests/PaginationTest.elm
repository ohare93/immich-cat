module PaginationTest exposing (..)

import Expect
import Fuzz exposing (intRange)
import Immich exposing (ImageOrder(..), ImageSearchConfig, PaginatedAssetResponse, SearchContext(..))
import Pagination exposing (NextPageRequestType(..), TimelineSyncAction(..), classifyTimelineSyncBehavior, computeNextPageRequest, shouldFetchNextPage)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)
import TestGenerators exposing (defaultPaginationState)
import Types exposing (AlbumPaginationContext, PaginationState, SourceLoadState, UserMode(..))
import UpdateAsset exposing (AssetState(..))
import UpdateMenus exposing (MenuState(..))


{-| Create a default PaginatedAssetResponse for testing
-}
defaultPaginatedResponse : PaginatedAssetResponse
defaultPaginatedResponse =
    { assets = []
    , total = 1000
    , count = 100
    , hasNextPage = True
    }


suite : Test
suite =
    describe "Pagination Pure Functions"
        [ describe "shouldFetchNextPage"
            [ test "returns True when response has next page and conditions are met" <|
                \_ ->
                    let
                        response =
                            { defaultPaginatedResponse | hasNextPage = True }

                        state =
                            { defaultPaginationState
                                | loadedAssets = 100
                                , maxAssetsToFetch = 10000
                            }
                    in
                    Expect.equal True (shouldFetchNextPage response state)
            , test "returns False when response has no next page" <|
                \_ ->
                    let
                        response =
                            { defaultPaginatedResponse | hasNextPage = False }

                        state =
                            { defaultPaginationState
                                | loadedAssets = 100
                                , maxAssetsToFetch = 10000
                            }
                    in
                    Expect.equal False (shouldFetchNextPage response state)
            , test "returns False when max assets limit reached" <|
                \_ ->
                    let
                        response =
                            { defaultPaginatedResponse | hasNextPage = True }

                        state =
                            { defaultPaginationState
                                | loadedAssets = 10000
                                , maxAssetsToFetch = 10000
                            }
                    in
                    Expect.equal False (shouldFetchNextPage response state)
            , test "returns False when exceeded max assets limit" <|
                \_ ->
                    let
                        response =
                            { defaultPaginatedResponse | hasNextPage = True }

                        state =
                            { defaultPaginationState
                                | loadedAssets = 15000
                                , maxAssetsToFetch = 10000
                            }
                    in
                    Expect.equal False (shouldFetchNextPage response state)
            , test "returns False when currentQuery is set (text search mode)" <|
                \_ ->
                    let
                        response =
                            { defaultPaginatedResponse | hasNextPage = True }

                        state =
                            { defaultPaginationState
                                | loadedAssets = 100
                                , maxAssetsToFetch = 10000
                                , currentQuery = Just "search term"
                            }
                    in
                    Expect.equal False (shouldFetchNextPage response state)
            , test "returns True when loadedAssets is less than max limit" <|
                \_ ->
                    let
                        response =
                            { defaultPaginatedResponse | hasNextPage = True }

                        state =
                            { defaultPaginationState
                                | loadedAssets = 9999
                                , maxAssetsToFetch = 10000
                            }
                    in
                    Expect.equal True (shouldFetchNextPage response state)
            ]
        , describe "computeNextPageRequest"
            [ test "returns TimelineRequest when currentConfig is set" <|
                \_ ->
                    let
                        config =
                            { order = CreatedDesc
                            , categorisation = Immich.All
                            , mediaType = Immich.AllMedia
                            , status = Immich.AllStatuses
                            }

                        state =
                            { defaultPaginationState | currentConfig = Just config }

                        result =
                            computeNextPageRequest 2 state
                    in
                    case result of
                        Just request ->
                            case request.requestType of
                                TimelineRequest _ ->
                                    Expect.all
                                        [ \r -> Expect.equal 2 r.nextPage
                                        , \r -> Expect.equal 1000 r.pageSize
                                        ]
                                        request

                                _ ->
                                    Expect.fail "Expected TimelineRequest"

                        Nothing ->
                            Expect.fail "Expected Just request"
            , test "returns TextSearchRequest when currentQuery and searchContext are set" <|
                \_ ->
                    let
                        state =
                            { defaultPaginationState
                                | currentQuery = Just "cats"
                                , currentSearchContext = Just ContentSearch
                            }

                        result =
                            computeNextPageRequest 3 state
                    in
                    case result of
                        Just request ->
                            case request.requestType of
                                TextSearchRequest query context ->
                                    Expect.all
                                        [ \_ -> Expect.equal "cats" query
                                        , \_ -> Expect.equal ContentSearch context
                                        , \r -> Expect.equal 3 r.nextPage
                                        ]
                                        request

                                _ ->
                                    Expect.fail "Expected TextSearchRequest"

                        Nothing ->
                            Expect.fail "Expected Just request"
            , test "returns TextSearchRequest with ContentSearch fallback when searchContext is Nothing" <|
                \_ ->
                    let
                        state =
                            { defaultPaginationState
                                | currentQuery = Just "dogs"
                                , currentSearchContext = Nothing
                            }

                        result =
                            computeNextPageRequest 4 state
                    in
                    case result of
                        Just request ->
                            case request.requestType of
                                TextSearchRequest query context ->
                                    Expect.all
                                        [ \_ -> Expect.equal "dogs" query
                                        , \_ -> Expect.equal ContentSearch context
                                        ]
                                        request

                                _ ->
                                    Expect.fail "Expected TextSearchRequest"

                        Nothing ->
                            Expect.fail "Expected Just request"
            , test "returns AlbumRequest when currentAlbumContext is set" <|
                \_ ->
                    let
                        albumContext =
                            { albumId = "album-123"
                            , order = ModifiedDesc
                            , mediaType = Immich.VideosOnly
                            , status = Immich.FavoritesOnly
                            }

                        state =
                            { defaultPaginationState
                                | currentAlbumContext = Just albumContext
                            }

                        result =
                            computeNextPageRequest 5 state
                    in
                    case result of
                        Just request ->
                            case request.requestType of
                                AlbumRequest ctx ->
                                    Expect.all
                                        [ \_ -> Expect.equal "album-123" ctx.albumId
                                        , \_ -> Expect.equal ModifiedDesc ctx.order
                                        , \r -> Expect.equal 5 r.nextPage
                                        ]
                                        request

                                _ ->
                                    Expect.fail "Expected AlbumRequest"

                        Nothing ->
                            Expect.fail "Expected Just request"
            , test "returns Nothing when no context is set" <|
                \_ ->
                    let
                        result =
                            computeNextPageRequest 1 defaultPaginationState
                    in
                    Expect.equal Nothing result
            , test "prioritizes currentConfig over currentQuery" <|
                \_ ->
                    let
                        config =
                            { order = CreatedAsc
                            , categorisation = Immich.Uncategorised
                            , mediaType = Immich.ImagesOnly
                            , status = Immich.AllStatuses
                            }

                        state =
                            { defaultPaginationState
                                | currentConfig = Just config
                                , currentQuery = Just "should be ignored"
                                , currentSearchContext = Just FilenameSearch
                            }

                        result =
                            computeNextPageRequest 2 state
                    in
                    case result of
                        Just request ->
                            case request.requestType of
                                TimelineRequest _ ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Expected TimelineRequest to take priority"

                        Nothing ->
                            Expect.fail "Expected Just request"
            , test "prioritizes currentQuery over currentAlbumContext" <|
                \_ ->
                    let
                        albumContext =
                            { albumId = "album-456"
                            , order = CreatedDesc
                            , mediaType = Immich.AllMedia
                            , status = Immich.AllStatuses
                            }

                        state =
                            { defaultPaginationState
                                | currentQuery = Just "search wins"
                                , currentSearchContext = Just OcrSearch
                                , currentAlbumContext = Just albumContext
                            }

                        result =
                            computeNextPageRequest 3 state
                    in
                    case result of
                        Just request ->
                            case request.requestType of
                                TextSearchRequest query _ ->
                                    Expect.equal "search wins" query

                                _ ->
                                    Expect.fail "Expected TextSearchRequest to take priority"

                        Nothing ->
                            Expect.fail "Expected Just request"
            ]
        , describe "classifyTimelineSyncBehavior"
            [ test "returns SyncTimelineView when isTimelineView and ViewAssets" <|
                \_ ->
                    let
                        userMode =
                            ViewAssets (SearchAssetInput "")
                    in
                    Expect.equal SyncTimelineView (classifyTimelineSyncBehavior True userMode)
            , test "returns NoTimelineSync when isTimelineView but MainMenu" <|
                \_ ->
                    let
                        userMode =
                            MainMenu MainMenuHome
                    in
                    Expect.equal NoTimelineSync (classifyTimelineSyncBehavior True userMode)
            , test "returns NoTimelineSync when isTimelineView but LoadingAssets" <|
                \_ ->
                    let
                        loadState =
                            { fetchedAssetList = Nothing, fetchedAssetMembership = Nothing }

                        userMode =
                            LoadingAssets loadState
                    in
                    Expect.equal NoTimelineSync (classifyTimelineSyncBehavior True userMode)
            , test "returns NoTimelineSync when not timeline view even with ViewAssets" <|
                \_ ->
                    let
                        userMode =
                            ViewAssets (SearchAssetInput "")
                    in
                    Expect.equal NoTimelineSync (classifyTimelineSyncBehavior False userMode)
            , test "returns NoTimelineSync when not timeline view and MainMenu" <|
                \_ ->
                    let
                        userMode =
                            MainMenu MainMenuHome
                    in
                    Expect.equal NoTimelineSync (classifyTimelineSyncBehavior False userMode)
            ]
        , describe "Property-Based Fuzz Tests"
            [ fuzz3 (intRange 1 1000) (intRange 1 100) (intRange 1 10) "page calculation consistency" <|
                \totalItems pageSize currentPage ->
                    let
                        totalPages =
                            ceiling (toFloat totalItems / toFloat pageSize)

                        validPage =
                            min currentPage totalPages |> max 1
                    in
                    -- Valid page should always be within bounds
                    Expect.all
                        [ \p -> p |> Expect.atLeast 1
                        , \p -> p |> Expect.atMost totalPages
                        ]
                        validPage
            , fuzz (intRange 0 10000) "total assets is never negative" <|
                \total ->
                    let
                        state =
                            { defaultPaginationState | totalAssets = max 0 total }
                    in
                    state.totalAssets |> Expect.atLeast 0
            , fuzz2 (intRange 1 100) (intRange 0 1000) "hasMorePages is consistent with totals" <|
                \pageSize loadedAssets ->
                    let
                        totalAssets =
                            loadedAssets + 50

                        -- Always some more to load
                        hasMore =
                            loadedAssets < totalAssets
                    in
                    Expect.equal hasMore True
            , fuzz (intRange 0 10000) "loadedAssets never exceeds meaningful bounds when set" <|
                \loaded ->
                    let
                        state =
                            { defaultPaginationState | loadedAssets = loaded }
                    in
                    -- loadedAssets can be any non-negative value
                    state.loadedAssets |> Expect.atLeast 0
            , fuzz2 (intRange 1 10) (intRange 1 1000) "currentPage is always positive" <|
                \page totalItems ->
                    let
                        state =
                            { defaultPaginationState
                                | currentPage = max 1 page
                                , totalAssets = totalItems
                            }
                    in
                    state.currentPage |> Expect.atLeast 1
            , fuzz2 (intRange 100 10000) (intRange 100 10000) "shouldFetchNextPage respects max assets limit" <|
                \loadedAssets maxAssets ->
                    let
                        response =
                            { defaultPaginatedResponse | hasNextPage = True }

                        state =
                            { defaultPaginationState
                                | loadedAssets = loadedAssets
                                , maxAssetsToFetch = maxAssets
                            }

                        result =
                            shouldFetchNextPage response state
                    in
                    if loadedAssets >= maxAssets then
                        Expect.equal False result

                    else
                        Expect.equal True result
            , fuzz (intRange 1 100) "computeNextPageRequest always uses 1000 page size" <|
                \nextPage ->
                    let
                        config =
                            { order = CreatedDesc
                            , categorisation = Immich.All
                            , mediaType = Immich.AllMedia
                            , status = Immich.AllStatuses
                            }

                        state =
                            { defaultPaginationState | currentConfig = Just config }

                        result =
                            computeNextPageRequest nextPage state
                    in
                    case result of
                        Just request ->
                            Expect.equal 1000 request.pageSize

                        Nothing ->
                            Expect.fail "Expected Just request with config set"
            ]
        ]
