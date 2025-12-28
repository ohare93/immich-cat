module ProcessImmichMsg exposing
    ( AlbumChangeResult
    , AlbumCreatedResult
    , processAlbumChangeSuccess
    , processAlbumCreatedSuccess
    )

{-| Pure functions for processing ImmichMsg results.

Extracts complex decision logic from Main.elm's ImmichMsg handler,
keeping Cmd generation in Main.elm.

-}

import Dict exposing (Dict)
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichAssetId)
import Types exposing (UserMode(..))
import UpdateAsset exposing (AssetState(..))
import ViewAlbums exposing (AlbumSearch, AssetWithActions, InputMode(..))


{-| Result of processing a successful album membership change.
-}
type alias AlbumChangeResult =
    { updatedPendingChanges : List ( ImmichAlbumId, Bool )
    , albumCountUpdate : Maybe ( ImmichAlbumId, Int )
    , shouldFetchMembership : Bool
    , assetIdForMembership : Maybe ImmichAssetId
    }


{-| Process a successful album assets change.

Handles the queue of pending album changes:

  - Pops one change from the queue
  - Computes the count delta (+1 for add, -1 for remove)
  - Determines if membership should be fetched (only when queue is empty)

-}
processAlbumChangeSuccess :
    List ( ImmichAlbumId, Bool )
    -> UserMode
    -> AlbumChangeResult
processAlbumChangeSuccess pendingChanges userMode =
    case pendingChanges of
        ( albumId, isAddition ) :: rest ->
            let
                countChange =
                    if isAddition then
                        1

                    else
                        -1

                -- Only fetch membership when ALL pending changes are processed
                ( shouldFetch, assetId ) =
                    if List.isEmpty rest then
                        case userMode of
                            ViewAssets assetState ->
                                case assetState of
                                    EditAsset _ asset _ ->
                                        ( True, Just asset.asset.id )

                                    _ ->
                                        ( False, Nothing )

                            _ ->
                                ( False, Nothing )

                    else
                        ( False, Nothing )
            in
            { updatedPendingChanges = rest
            , albumCountUpdate = Just ( albumId, countChange )
            , shouldFetchMembership = shouldFetch
            , assetIdForMembership = assetId
            }

        [] ->
            { updatedPendingChanges = []
            , albumCountUpdate = Nothing
            , shouldFetchMembership = False
            , assetIdForMembership = Nothing
            }


{-| Result of processing a successful album creation.
-}
type alias AlbumCreatedResult =
    { updatedAsset : AssetWithActions
    , newSearch : AlbumSearch
    , pendingChange : ( ImmichAlbumId, Bool )
    }


{-| Process a successful album creation when in LoadingAssets state.

Automatically adds the current asset to the newly created album:

  - Toggles album membership on the asset
  - Creates fresh album search
  - Returns pending change for the API call

-}
processAlbumCreatedSuccess :
    ImmichAlbum
    -> AssetWithActions
    -> Dict ImmichAlbumId ImmichAlbum
    -> AlbumCreatedResult
processAlbumCreatedSuccess album currentAsset knownAlbums =
    let
        updatedAsset =
            ViewAlbums.toggleAssetAlbum currentAsset album

        newSearch =
            ViewAlbums.getAlbumSearch "" knownAlbums
    in
    { updatedAsset = updatedAsset
    , newSearch = newSearch
    , pendingChange = ( album.id, True )
    }
