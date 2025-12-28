module Pagination exposing
    ( AppendAssetsResult
    , NextPageRequest
    , NextPageRequestType(..)
    , appendAssetsResult
    , computeNextPageRequest
    , shouldFetchNextPage
    , updatePaginationStateFromResponse
    )

{-| Pure functions for managing pagination state.

This module provides functions for working with PaginationState without
depending on Model. Main.elm uses these to update pagination state.

-}

import Dict exposing (Dict)
import Helpers exposing (applySortingToAssets, listOverrideDict)
import Immich exposing (ImageSearchConfig, ImmichAsset, ImmichAssetId, PaginatedAssetResponse, SearchContext)
import Types exposing (AlbumPaginationContext, PaginationState)


{-| Update pagination state from an API response.
Pure function that computes the new pagination state.
-}
updatePaginationStateFromResponse : PaginatedAssetResponse -> Int -> PaginationState -> PaginationState
updatePaginationStateFromResponse paginatedResponse page paginationState =
    let
        newLoadedAssets =
            paginationState.loadedAssets + paginatedResponse.count

        reachedLimit =
            newLoadedAssets >= paginationState.maxAssetsToFetch

        hasMoreToLoad =
            paginatedResponse.hasNextPage && not reachedLimit && paginationState.currentQuery == Nothing
    in
    { currentConfig = paginationState.currentConfig
    , currentQuery = paginationState.currentQuery
    , currentSearchContext = paginationState.currentSearchContext
    , currentAlbumContext = paginationState.currentAlbumContext
    , totalAssets = paginatedResponse.total
    , currentPage = page
    , hasMorePages = hasMoreToLoad
    , isLoadingMore = paginationState.isLoadingMore
    , loadedAssets = newLoadedAssets
    , maxAssetsToFetch = paginationState.maxAssetsToFetch
    }


{-| Result of appending fetched assets.
Contains all the pure computed values that Main.elm uses to update Model.
-}
type alias AppendAssetsResult =
    { knownAssets : Dict ImmichAssetId ImmichAsset
    , currentAssets : List ImmichAssetId
    , imageIndex : Int
    , isTimelineView : Bool
    }


{-| Compute the result of appending newly fetched assets.
Pure function that merges assets and optionally re-sorts for timeline views.
Main.elm interprets the result and decides whether to call switchToEditIfAssetFound.
-}
appendAssetsResult :
    List ImmichAsset
    -> Dict ImmichAssetId ImmichAsset
    -> List ImmichAssetId
    -> Int
    -> Maybe ImageSearchConfig
    -> AppendAssetsResult
appendAssetsResult newAssets knownAssets existingAssetIds currentImageIndex maybeConfig =
    let
        updatedKnownAssets =
            listOverrideDict newAssets (\a -> ( a.id, a )) knownAssets

        newAssetIds =
            List.map .id newAssets

        combinedAssetIds =
            existingAssetIds ++ newAssetIds

        ( finalAssetIds, finalImageIndex, isTimeline ) =
            case maybeConfig of
                Just config ->
                    -- Timeline view: re-sort all assets and reset to index 0
                    let
                        allAssets =
                            combinedAssetIds
                                |> List.filterMap (\id -> Dict.get id updatedKnownAssets)

                        sortedAssets =
                            applySortingToAssets config.order allAssets
                    in
                    ( List.map .id sortedAssets, 0, True )

                Nothing ->
                    -- Non-timeline view: preserve order and current index
                    ( combinedAssetIds, currentImageIndex, False )
    in
    { knownAssets = updatedKnownAssets
    , currentAssets = finalAssetIds
    , imageIndex = finalImageIndex
    , isTimelineView = isTimeline
    }



-- PAGINATION DECISION HELPERS


{-| Determine if we should fetch the next page of results.
Pure function that checks pagination conditions.

Returns True if:

  - Response indicates more pages exist
  - We haven't reached the max assets limit
  - We're not in a text search (text search doesn't auto-paginate)

-}
shouldFetchNextPage : PaginatedAssetResponse -> PaginationState -> Bool
shouldFetchNextPage paginatedResponse paginationState =
    let
        reachedLimit =
            paginationState.loadedAssets >= paginationState.maxAssetsToFetch
    in
    paginatedResponse.hasNextPage && not reachedLimit && paginationState.currentQuery == Nothing


{-| Type of next page request to make.
Main.elm uses this to determine which Immich API function to call.
-}
type NextPageRequestType
    = TimelineRequest ImageSearchConfig
    | TextSearchRequest String SearchContext
    | AlbumRequest AlbumPaginationContext


{-| Parameters for fetching the next page.
-}
type alias NextPageRequest =
    { pageSize : Int
    , nextPage : Int
    , requestType : NextPageRequestType
    }


{-| Compute the next page request based on pagination state.
Returns Nothing if no more pages should be fetched.
Main.elm uses this to generate the appropriate Cmd.
-}
computeNextPageRequest : Int -> PaginationState -> Maybe NextPageRequest
computeNextPageRequest nextPage paginationState =
    case paginationState.currentConfig of
        Just config ->
            Just
                { pageSize = 1000
                , nextPage = nextPage
                , requestType = TimelineRequest config
                }

        Nothing ->
            case ( paginationState.currentQuery, paginationState.currentSearchContext ) of
                ( Just query, Just searchContext ) ->
                    Just
                        { pageSize = 1000
                        , nextPage = nextPage
                        , requestType = TextSearchRequest query searchContext
                        }

                ( Just query, Nothing ) ->
                    -- Fallback to content search
                    Just
                        { pageSize = 1000
                        , nextPage = nextPage
                        , requestType = TextSearchRequest query Immich.ContentSearch
                        }

                _ ->
                    case paginationState.currentAlbumContext of
                        Just albumContext ->
                            Just
                                { pageSize = 1000
                                , nextPage = nextPage
                                , requestType = AlbumRequest albumContext
                                }

                        Nothing ->
                            Nothing
