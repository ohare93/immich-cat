module UpdateImmich exposing
    ( FetchAlbumsResult
    , FetchAssetsResult
    , handleFetchAlbumsResult
    , handleFetchAssetsResult
    , updateAlbumAssetCount
    )

{-| Pure functions for processing Immich API responses.

This module handles the pure logic for processing fetch results.
Main.elm uses these results to update Model.

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Helpers exposing (applySortingToAssets, listOverrideDict)
import Immich exposing (ImageSearchConfig, ImmichAlbum, ImmichAlbumId, ImmichAsset, ImmichAssetId, ImmichLoadState(..))
import KeybindBranches exposing (generateAlbumKeybindings)
import Types exposing (FeedbackMessage(..))


{-| Result of processing fetched albums.
-}
type alias FetchAlbumsResult =
    { knownAlbums : Dict ImmichAlbumId ImmichAlbum
    , albumsLoadState : ImmichLoadState
    , albumKeybindings : Dict ImmichAlbumId String
    , reloadFeedback : Maybe FeedbackMessage
    }


{-| Process fetched albums into a result.
-}
handleFetchAlbumsResult :
    { showReloadFeedback : Bool
    , albums : List ImmichAlbum
    , currentKnownAlbums : Dict ImmichAlbumId ImmichAlbum
    , currentReloadFeedback : Maybe FeedbackMessage
    }
    -> FetchAlbumsResult
handleFetchAlbumsResult config =
    let
        updatedKnownAlbums =
            listOverrideDict config.albums (\a -> ( a.id, a )) config.currentKnownAlbums

        allAlbums =
            Dict.values updatedKnownAlbums

        albumKeybindings =
            generateAlbumKeybindings allAlbums

        albumCount =
            List.length config.albums

        isFirstLoad =
            Dict.isEmpty config.currentKnownAlbums

        feedbackMessage =
            if config.showReloadFeedback then
                if albumCount > 0 then
                    if isFirstLoad then
                        Just (AlbumsLoaded albumCount)

                    else
                        Just (AlbumsReloaded albumCount)

                else
                    Just NoAlbumsFound

            else
                config.currentReloadFeedback
    in
    { knownAlbums = updatedKnownAlbums
    , albumsLoadState = ImmichLoadSuccess
    , albumKeybindings = albumKeybindings
    , reloadFeedback = feedbackMessage
    }


{-| Result of processing fetched assets.
-}
type alias FetchAssetsResult =
    { knownAssets : Dict ImmichAssetId ImmichAsset
    , currentAssets : Array ImmichAssetId
    , imagesLoadState : ImmichLoadState
    , imageIndex : Int
    , isTimelineView : Bool
    }


{-| Process fetched assets into a result.
Handles client-side sorting for timeline views since Immich API doesn't respect orderBy properly.
-}
handleFetchAssetsResult :
    { assets : List ImmichAsset
    , currentConfig : Maybe ImageSearchConfig
    , currentKnownAssets : Dict ImmichAssetId ImmichAsset
    , currentImageIndex : Int
    }
    -> FetchAssetsResult
handleFetchAssetsResult config =
    let
        -- Apply client-side sorting since Immich API doesn't respect orderBy properly
        sortedAssets =
            case config.currentConfig of
                Just cfg ->
                    applySortingToAssets cfg.order config.assets

                Nothing ->
                    config.assets
    in
    case config.currentConfig of
        Just _ ->
            -- Timeline view with sorting - jump to first asset
            { knownAssets = listOverrideDict sortedAssets (\a -> ( a.id, a )) config.currentKnownAssets
            , currentAssets = Array.fromList (List.map .id sortedAssets)
            , imagesLoadState = ImmichLoadSuccess
            , imageIndex = 0
            , isTimelineView = True
            }

        Nothing ->
            -- No timeline sorting, preserve current index
            { knownAssets = listOverrideDict sortedAssets (\a -> ( a.id, a )) config.currentKnownAssets
            , currentAssets = Array.fromList (List.map .id sortedAssets)
            , imagesLoadState = ImmichLoadSuccess
            , imageIndex = config.currentImageIndex
            , isTimelineView = False
            }


{-| Update the asset count for an album.
Pure function that updates the album's assetCount.
-}
updateAlbumAssetCount : ImmichAlbumId -> Int -> Dict ImmichAlbumId ImmichAlbum -> Dict ImmichAlbumId ImmichAlbum
updateAlbumAssetCount albumId countChange knownAlbums =
    Dict.update albumId
        (\maybeAlbum ->
            case maybeAlbum of
                Just album ->
                    Just { album | assetCount = max 0 (album.assetCount + countChange) }

                Nothing ->
                    Nothing
        )
        knownAlbums
