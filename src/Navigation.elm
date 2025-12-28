module Navigation exposing
    ( NavigateResult(..)
    , NavigationFields
    , clearForwardQueueForViewAssets
    , createCurrentNavigationEntry
    , getCurrentAssetWithActions
    , navigateBack
    , navigateForward
    , recordNavigationState
    , setCurrentNavigationState
    , updateCurrentEntry
    , updateCurrentHistoryEntry
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



-- MODEL UPDATE HELPERS
-- These functions update navigation-related fields on any record with the required fields.
-- They use extensible records to work with Main.Model without depending on it.


{-| Record navigation state when entering a new asset view.

Clears the forward queue when entering ViewAssets mode (vim-style branching).
When you navigate to a new asset view after going back in history,
the forward queue is cleared (you've branched off).

INVARIANT: Call BEFORE setCurrentNavigationState when establishing
a new history entry. Never use with updateCurrentHistoryEntry.

Usage pattern for new history entries:

    model |> recordNavigationState newMode |> setCurrentNavigationState

-}
recordNavigationState :
    UserMode
    ->
        { a
            | navigationBackStack : List NavigationHistoryEntry
            , currentNavigationState : Maybe NavigationHistoryEntry
            , navigationForwardQueue : List NavigationHistoryEntry
        }
    ->
        { a
            | navigationBackStack : List NavigationHistoryEntry
            , currentNavigationState : Maybe NavigationHistoryEntry
            , navigationForwardQueue : List NavigationHistoryEntry
        }
recordNavigationState newMode model =
    let
        navFields =
            { navigationBackStack = model.navigationBackStack
            , currentNavigationState = model.currentNavigationState
            , navigationForwardQueue = model.navigationForwardQueue
            }

        updated =
            clearForwardQueueForViewAssets newMode navFields
    in
    { model | navigationForwardQueue = updated.navigationForwardQueue }


{-| Create a snapshot of the current state for navigation history.

Creates a navigation entry containing userMode, asset source, current
assets list, image index, and pagination state. Only creates an entry
when in ViewAssets mode (history only tracks asset viewing states).

INVARIANT: Call AFTER recordNavigationState when establishing a new
history entry. Together they form the complete history entry pattern.

Usage pattern for new history entries:

    model |> recordNavigationState newMode |> setCurrentNavigationState

-}
setCurrentNavigationState :
    { a
        | userMode : UserMode
        , currentAssetsSource : AssetSource
        , currentAssets : List ImmichAssetId
        , imageIndex : ImageIndex
        , paginationState : PaginationState
        , currentNavigationState : Maybe NavigationHistoryEntry
    }
    ->
        { a
            | userMode : UserMode
            , currentAssetsSource : AssetSource
            , currentAssets : List ImmichAssetId
            , imageIndex : ImageIndex
            , paginationState : PaginationState
            , currentNavigationState : Maybe NavigationHistoryEntry
        }
setCurrentNavigationState model =
    let
        maybeEntry =
            createCurrentNavigationEntry
                model.userMode
                model.currentAssetsSource
                model.currentAssets
                model.imageIndex
                model.paginationState
    in
    case maybeEntry of
        Just entry ->
            { model | currentNavigationState = Just entry }

        Nothing ->
            model


{-| Update the current history entry in-place without branching.

Used when navigating between assets via back/forward (history restoration).
Updates the current entry's imageIndex and state without creating a new
history entry or clearing the forward queue.

INVARIANT: Only use in switchToAssetWithoutHistory for history navigation.
Never pair with recordNavigationState or setCurrentNavigationState - this
is an in-place update, not a branch point.

-}
updateCurrentHistoryEntry :
    { a
        | userMode : UserMode
        , currentAssetsSource : AssetSource
        , currentAssets : List ImmichAssetId
        , imageIndex : ImageIndex
        , paginationState : PaginationState
        , currentNavigationState : Maybe NavigationHistoryEntry
    }
    ->
        { a
            | userMode : UserMode
            , currentAssetsSource : AssetSource
            , currentAssets : List ImmichAssetId
            , imageIndex : ImageIndex
            , paginationState : PaginationState
            , currentNavigationState : Maybe NavigationHistoryEntry
        }
updateCurrentHistoryEntry model =
    { model
        | currentNavigationState =
            updateCurrentEntry
                model.currentNavigationState
                model.userMode
                model.currentAssetsSource
                model.currentAssets
                model.imageIndex
                model.paginationState
    }
