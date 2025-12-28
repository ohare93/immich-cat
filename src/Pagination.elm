module Pagination exposing
    ( AppendAssetsResult
    , appendAssetsResult
    , updatePaginationStateFromResponse
    )

{-| Pure functions for managing pagination state.

This module provides functions for working with PaginationState without
depending on Model. Main.elm uses these to update pagination state.

-}

import Dict exposing (Dict)
import Helpers exposing (applySortingToAssets, listOverrideDict)
import Immich exposing (ImageSearchConfig, ImmichAsset, ImmichAssetId, PaginatedAssetResponse)
import Types exposing (PaginationState)


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
