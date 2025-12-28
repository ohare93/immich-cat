module Navigation exposing
    ( NavigateResult(..)
    , NavigationFields
    , clearForwardQueueForViewAssets
    , createCurrentNavigationEntry
    , getCurrentAssetWithActions
    , navigateBack
    , navigateForward
    , updateCurrentEntry
    )

{-| Pure functions for managing navigation history.

This module provides functions for working with navigation state without
generating Commands. Main.elm uses these results to update Model and
trigger asset loading.

-}

import Dict exposing (Dict)
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichAsset, ImmichAssetId)
import Types exposing (AssetSource, ImageIndex, NavigationHistoryEntry, PaginationState, UserMode(..))
import ViewAlbums exposing (AlbumSearch, AssetWithActions, getAlbumSearchWithHeight, getAssetWithActions)


{-| Navigation-related fields that get updated together.
-}
type alias NavigationFields =
    { navigationBackStack : List NavigationHistoryEntry
    , currentNavigationState : Maybe NavigationHistoryEntry
    , navigationForwardQueue : List NavigationHistoryEntry
    }


{-| Result of a navigation operation.
-}
type NavigateResult
    = NoHistory
    | RestoredState
        { navFields : NavigationFields
        , userMode : UserMode
        , currentAssetsSource : AssetSource
        , currentAssets : List ImmichAssetId
        , imageIndex : ImageIndex
        , paginationState : PaginationState
        , needsAssetSwitch : Bool
        , assetIndex : ImageIndex
        }


{-| Clear forward queue when entering ViewAssets mode (vim-style branching).
-}
clearForwardQueueForViewAssets : UserMode -> NavigationFields -> NavigationFields
clearForwardQueueForViewAssets newMode navFields =
    case newMode of
        ViewAssets _ ->
            { navFields | navigationForwardQueue = [] }

        _ ->
            navFields


{-| Create a navigation entry from the current state.
Returns Nothing if not in ViewAssets mode.
-}
createCurrentNavigationEntry :
    UserMode
    -> AssetSource
    -> List ImmichAssetId
    -> ImageIndex
    -> PaginationState
    -> Maybe NavigationHistoryEntry
createCurrentNavigationEntry userMode currentAssetsSource currentAssets imageIndex paginationState =
    case userMode of
        ViewAssets _ ->
            Just
                { userMode = userMode
                , currentAssetsSource = currentAssetsSource
                , currentAssets = currentAssets
                , imageIndex = imageIndex
                , paginationState = paginationState
                }

        _ ->
            Nothing


{-| Update the current navigation entry with the latest state.
Used when navigating between assets in the same view.
-}
updateCurrentEntry :
    Maybe NavigationHistoryEntry
    -> UserMode
    -> AssetSource
    -> List ImmichAssetId
    -> ImageIndex
    -> PaginationState
    -> Maybe NavigationHistoryEntry
updateCurrentEntry maybeCurrentEntry userMode currentAssetsSource currentAssets imageIndex paginationState =
    case maybeCurrentEntry of
        Just currentEntry ->
            Just
                { currentEntry
                    | imageIndex = imageIndex
                    , currentAssets = currentAssets
                    , currentAssetsSource = currentAssetsSource
                    , paginationState = paginationState
                }

        Nothing ->
            Just
                { userMode = userMode
                , currentAssetsSource = currentAssetsSource
                , currentAssets = currentAssets
                , imageIndex = imageIndex
                , paginationState = paginationState
                }


{-| Navigate back in history.
Returns the updated navigation fields and state to restore.
-}
navigateBack : NavigationFields -> NavigateResult
navigateBack navFields =
    case navFields.navigationBackStack of
        previousEntry :: remainingBackStack ->
            let
                newForwardQueue =
                    case navFields.currentNavigationState of
                        Just currentEntry ->
                            currentEntry :: navFields.navigationForwardQueue

                        Nothing ->
                            navFields.navigationForwardQueue

                needsSwitch =
                    case previousEntry.userMode of
                        ViewAssets _ ->
                            True

                        _ ->
                            False
            in
            RestoredState
                { navFields =
                    { navigationBackStack = remainingBackStack
                    , currentNavigationState = Just previousEntry
                    , navigationForwardQueue = newForwardQueue
                    }
                , userMode = previousEntry.userMode
                , currentAssetsSource = previousEntry.currentAssetsSource
                , currentAssets = previousEntry.currentAssets
                , imageIndex = previousEntry.imageIndex
                , paginationState = previousEntry.paginationState
                , needsAssetSwitch = needsSwitch
                , assetIndex = previousEntry.imageIndex
                }

        [] ->
            NoHistory


{-| Navigate forward in history.
Returns the updated navigation fields and state to restore.
-}
navigateForward : NavigationFields -> NavigateResult
navigateForward navFields =
    case navFields.navigationForwardQueue of
        nextEntry :: remainingForwardQueue ->
            let
                newBackStack =
                    case navFields.currentNavigationState of
                        Just currentEntry ->
                            currentEntry :: navFields.navigationBackStack

                        Nothing ->
                            navFields.navigationBackStack

                needsSwitch =
                    case nextEntry.userMode of
                        ViewAssets _ ->
                            True

                        _ ->
                            False
            in
            RestoredState
                { navFields =
                    { navigationBackStack = newBackStack
                    , currentNavigationState = Just nextEntry
                    , navigationForwardQueue = remainingForwardQueue
                    }
                , userMode = nextEntry.userMode
                , currentAssetsSource = nextEntry.currentAssetsSource
                , currentAssets = nextEntry.currentAssets
                , imageIndex = nextEntry.imageIndex
                , paginationState = nextEntry.paginationState
                , needsAssetSwitch = needsSwitch
                , assetIndex = nextEntry.imageIndex
                }

        [] ->
            NoHistory


{-| Get the current asset with actions and album search context.
Pure helper for looking up the asset at the current index.
-}
getCurrentAssetWithActions :
    List ImmichAssetId
    -> ImageIndex
    -> Dict ImmichAssetId ImmichAsset
    -> Dict ImmichAlbumId ImmichAlbum
    -> Int
    -> Maybe ( AssetWithActions, AlbumSearch )
getCurrentAssetWithActions currentAssets imageIndex knownAssets knownAlbums screenHeight =
    currentAssets
        |> List.drop imageIndex
        |> List.head
        |> Maybe.andThen (\id -> Dict.get id knownAssets)
        |> Maybe.map (\asset -> ( getAssetWithActions asset, getAlbumSearchWithHeight "" knownAlbums screenHeight ))
