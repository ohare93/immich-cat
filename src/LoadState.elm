module LoadState exposing
    ( createInitialLoadState
    , isLoadCompletedForProp
    , isLoadStateCompleted
    , updateLoadStateForFetch
    )

{-| Pure functions for managing loading state transitions.

This module provides functions for working with SourceLoadState without
depending on Model. Main.elm uses these to update the loading state.

-}

import Types exposing (AssetSource(..), AssetSourceUpdate(..), SourceLoadState)


{-| Check if a single load property is completed.
A property is considered complete if it's Nothing (not required) or Just True (completed).
-}
isLoadCompletedForProp : Maybe Bool -> Bool
isLoadCompletedForProp maybeBool =
    maybeBool == Nothing || maybeBool == Just True


{-| Check if all required loading operations are complete.
-}
isLoadStateCompleted : SourceLoadState -> Bool
isLoadStateCompleted loadState =
    isLoadCompletedForProp loadState.fetchedAssetList


{-| Create the initial load state for a given asset source.
Returns Nothing for NoAssets (no loading needed), or Just the initial SourceLoadState.
-}
createInitialLoadState : AssetSource -> Maybe SourceLoadState
createInitialLoadState assetSource =
    case assetSource of
        NoAssets ->
            Nothing

        ImageSearch _ ->
            Just { fetchedAssetList = Just False, fetchedAssetMembership = Nothing }

        Album _ ->
            Just { fetchedAssetList = Just False, fetchedAssetMembership = Nothing }

        FilteredAlbum _ _ ->
            Just { fetchedAssetList = Just False, fetchedAssetMembership = Nothing }

        TextSearch _ _ ->
            Just { fetchedAssetList = Just False, fetchedAssetMembership = Nothing }


{-| Update the load state when a fetch operation completes.
-}
updateLoadStateForFetch : AssetSourceUpdate -> SourceLoadState -> SourceLoadState
updateLoadStateForFetch updateType loadState =
    case updateType of
        FetchedAssetList ->
            { loadState | fetchedAssetList = Just True }
