module AssetNavigation exposing
    ( AssetSwitchResult(..)
    , buildAssetViewState
    , findAssetByIndex
    , preserveVideoLoadedState
    )

{-| Pure functions for navigating between assets.

This module extracts the pure computation from asset switching logic,
leaving only Cmd generation and navigation state updates in Main.elm.

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichAsset, ImmichAssetId)
import Types exposing (ImageIndex, UserMode(..))
import UpdateAsset exposing (AssetState(..))
import ViewAlbums exposing (AlbumSearch, AssetWithActions, InputMode(..), getAlbumSearchWithHeight, getAssetWithActions)


{-| Result of attempting to find and prepare an asset for viewing.
-}
type AssetSwitchResult
    = AssetFound
        { asset : ImmichAsset
        , assetWithActions : AssetWithActions
        , albumSearch : AlbumSearch
        , newIndex : ImageIndex
        }
    | AssetNotFound


{-| Find an asset by index in the current assets array.
Pure function that looks up the asset in the known assets dictionary.
O(1) lookup using Array.get instead of O(n) List.drop.
-}
findAssetByIndex : Array ImmichAssetId -> ImageIndex -> Dict ImmichAssetId ImmichAsset -> Maybe ImmichAsset
findAssetByIndex currentAssets index knownAssets =
    Array.get index currentAssets
        |> Maybe.andThen (\id -> Dict.get id knownAssets)


{-| Build the complete view state for displaying an asset.
Returns an AssetSwitchResult containing all computed values.
-}
buildAssetViewState :
    Array ImmichAssetId
    -> ImageIndex
    -> Dict ImmichAssetId ImmichAsset
    -> Dict ImmichAlbumId ImmichAlbum
    -> Int
    -> AssetSwitchResult
buildAssetViewState currentAssets index knownAssets knownAlbums screenHeight =
    case findAssetByIndex currentAssets index knownAssets of
        Just asset ->
            AssetFound
                { asset = asset
                , assetWithActions = getAssetWithActions asset
                , albumSearch = getAlbumSearchWithHeight "" knownAlbums screenHeight
                , newIndex = index
                }

        Nothing ->
            AssetNotFound


{-| Preserve video loaded state when switching to the same asset.
Checks if we're viewing the same asset and preserves the video loaded state.
-}
preserveVideoLoadedState : UserMode -> ImmichAsset -> AssetWithActions -> AssetWithActions
preserveVideoLoadedState currentUserMode newAsset baseAssetWithActions =
    case currentUserMode of
        ViewAssets (EditAsset _ currentAsset _) ->
            if currentAsset.asset.id == newAsset.id then
                { baseAssetWithActions | isVideoLoaded = currentAsset.isVideoLoaded }

            else
                baseAssetWithActions

        _ ->
            baseAssetWithActions
