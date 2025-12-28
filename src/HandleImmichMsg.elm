module HandleImmichMsg exposing (ImmichMsgContext, ImmichMsgResult, handleImmichMsg)

{-| Extract ImmichMsg handling logic from Main.elm to reduce complexity.

This module handles all Immich API responses and computes:

  - Updated context fields (model state)
  - Commands to execute (API calls, delays, etc.)

The Main.elm update function is responsible for:

  - Building the context from the full Model
  - Calling this handler
  - Applying the result back to the full Model

-}

import AssetNavigation
import AssetSourceTypes exposing (AssetSource)
import Dict exposing (Dict)
import Immich exposing (ImageOrder, ImmichAlbum, ImmichAlbumId, ImmichApiPaths, ImmichAsset, ImmichAssetId, ImmichLoadState(..), MediaTypeFilter, StatusFilter)
import KeybindBranches exposing (generateAlbumKeybindings)
import LoadState
import Navigation
import Pagination
import Process
import ProcessImmichMsg
import Task
import Types exposing (AssetSourceUpdate(..), ImageIndex, PaginationState, UserMode(..))
import UpdateAsset exposing (AssetState(..))
import UpdateImmich
import ViewAlbums exposing (AlbumSearch, AssetWithActions, InputMode(..))


{-| Context record containing all model fields needed by ImmichMsg handler.
-}
type alias ImmichMsgContext =
    { immichApiPaths : ImmichApiPaths
    , knownAssets : Dict ImmichAssetId ImmichAsset
    , knownAlbums : Dict ImmichAlbumId ImmichAlbum
    , albumKeybindings : Dict String ImmichAlbumId
    , pendingAlbumChanges : List ( ImmichAlbumId, Bool )
    , userMode : UserMode
    , imageIndex : ImageIndex
    , currentAssets : List ImmichAssetId
    , paginationState : PaginationState
    , reloadFeedback : Maybe String
    , imagesLoadState : ImmichLoadState
    , albumsLoadState : ImmichLoadState
    , currentAssetsSource : AssetSource
    , screenHeight : Int
    }


{-| Result of handling an ImmichMsg - updated context and commands to execute.

The msg type parameter allows this module to avoid a circular dependency with Main.
Main.elm will instantiate this with its own Msg type.

-}
type alias ImmichMsgResult msg =
    { updatedContext : ImmichMsgContext
    , cmd : Cmd msg
    }


{-| Handle an ImmichMsg and return updated context + commands.

This is the main entry point that Main.elm calls when receiving an ImmichMsg.
The msg mapper is used to wrap Immich.Msg in the parent's Msg type.

-}
handleImmichMsg : (Immich.Msg -> msg) -> Immich.Msg -> ImmichMsgContext -> ImmichMsgResult msg
handleImmichMsg toMsg imsg context =
    -- First update context based on message type (newModel equivalent)
    let
        newContext =
            updateContextForMessage imsg context
    in
    -- Then generate commands based on message (second case statement)
    generateCommandsForMessage toMsg imsg context newContext


{-| Update context fields based on the ImmichMsg variant.

This corresponds to the first case statement in Main.elm's ImmichMsg handler
that creates newModel.

-}
updateContextForMessage : Immich.Msg -> ImmichMsgContext -> ImmichMsgContext
updateContextForMessage imsg context =
    case imsg of
        Immich.SingleAlbumFetched (Ok album) ->
            context
                |> handleFetchAlbums False [ album ]
                |> handleFetchAssets album.assets
                |> handleUpdateLoadingState FetchedAssetList

        Immich.AlbumsFetched (Ok albums) ->
            context |> handleFetchAlbums True albums

        Immich.AlbumCreated (Ok album) ->
            context |> handleFetchAlbums False [ album ]

        Immich.ImagesFetched (Ok assets) ->
            context
                |> handleFetchAssets assets
                |> handleUpdateLoadingState FetchedAssetList

        Immich.PaginatedImagesFetched (Ok paginatedResponse) ->
            let
                afterFetch =
                    context |> handleFetchAssets paginatedResponse.assets

                afterUpdate =
                    afterFetch |> handleUpdateLoadingState FetchedAssetList

                finalContext =
                    afterUpdate |> updatePaginationState paginatedResponse 1
            in
            finalContext

        Immich.MoreImagesFetched page (Ok paginatedResponse) ->
            context
                |> appendFetchedAssets paginatedResponse.assets
                |> updatePaginationState paginatedResponse page

        Immich.AssetMembershipFetched (Ok assetWithMembership) ->
            context |> handleFetchAssetMembership assetWithMembership

        Immich.AssetMembershipFetched (Err httpError) ->
            { context | reloadFeedback = Just ("Album membership fetch failed: " ++ Immich.errorToString httpError) }

        Immich.AlbumFetchedWithClientSideFiltering _ _ _ (Ok album) ->
            context |> handleFetchAlbums False [ album ]

        Immich.AlbumFetchedWithClientSideFiltering _ _ _ (Err error) ->
            { context | imagesLoadState = ImmichLoadError error }

        Immich.AssetUpdated (Ok updatedAsset) ->
            { context | knownAssets = Dict.insert updatedAsset.id updatedAsset context.knownAssets }

        Immich.BulkAssetsUpdated (Ok updatedAssets) ->
            let
                updatedKnownAssets =
                    List.foldl
                        (\asset acc -> Dict.insert asset.id asset acc)
                        context.knownAssets
                        updatedAssets
            in
            { context | knownAssets = updatedKnownAssets }

        Immich.AlbumsFetched (Err error) ->
            { context | albumsLoadState = ImmichLoadError error }

        Immich.AlbumCreated (Err error) ->
            case context.userMode of
                LoadingAssets _ ->
                    getCurrentAssetWithActions context
                        |> Maybe.map (\( assetWithActions, search ) -> { context | userMode = ViewAssets (EditAsset NormalMode assetWithActions search) })
                        |> Maybe.withDefault context

                _ ->
                    context

        Immich.ImagesFetched (Err error) ->
            { context | imagesLoadState = ImmichLoadError error }

        Immich.PaginatedImagesFetched (Err error) ->
            { context | imagesLoadState = ImmichLoadError error }

        Immich.MoreImagesFetched _ (Err error) ->
            { context | imagesLoadState = ImmichLoadError error }

        _ ->
            context


{-| Generate commands based on the ImmichMsg variant.

This corresponds to the second case statement in Main.elm's ImmichMsg handler
that generates commands.

-}
generateCommandsForMessage : (Immich.Msg -> msg) -> Immich.Msg -> ImmichMsgContext -> ImmichMsgContext -> ImmichMsgResult msg
generateCommandsForMessage toMsg imsg originalContext newContext =
    case imsg of
        Immich.AlbumAssetsChanged (Ok _) ->
            -- Album membership change succeeded - use pure function to compute updates
            let
                result =
                    ProcessImmichMsg.processAlbumChangeSuccess
                        newContext.pendingAlbumChanges
                        newContext.userMode

                contextWithCount =
                    case result.albumCountUpdate of
                        Just ( albumId, countChange ) ->
                            updateAlbumAssetCount albumId countChange newContext

                        Nothing ->
                            newContext

                updatedContext =
                    { contextWithCount | pendingAlbumChanges = result.updatedPendingChanges }

                membershipCmd =
                    case result.assetIdForMembership of
                        Just assetId ->
                            Immich.fetchMembershipForAsset updatedContext.immichApiPaths assetId |> Cmd.map toMsg

                        Nothing ->
                            Cmd.none
            in
            { updatedContext = updatedContext
            , cmd = membershipCmd
            }

        Immich.AlbumAssetsChanged (Err _) ->
            -- Album membership change failed, clear all pending changes
            -- Note: switchToEditIfAssetFound is handled in Main.elm since it returns (Model, Cmd)
            { updatedContext = { newContext | pendingAlbumChanges = [] }
            , cmd = Cmd.none
            }

        Immich.AlbumsFetched (Ok albums) ->
            -- Note: ClearReloadFeedback command is handled in Main.elm
            { updatedContext = newContext
            , cmd = Cmd.none
            }

        Immich.AlbumCreated (Ok album) ->
            -- Auto-add current asset to newly created album
            case originalContext.userMode of
                LoadingAssets _ ->
                    getCurrentAssetWithActions newContext
                        |> Maybe.map
                            (\( assetWithActions, _ ) ->
                                let
                                    result =
                                        ProcessImmichMsg.processAlbumCreatedSuccess
                                            album
                                            assetWithActions
                                            newContext.knownAlbums

                                    updatedContext =
                                        { newContext
                                            | userMode = ViewAssets (EditAsset NormalMode result.updatedAsset result.newSearch)
                                            , pendingAlbumChanges = [ result.pendingChange ]
                                        }

                                    addToAlbumCmd =
                                        Immich.albumChangeAssetMembership newContext.immichApiPaths album.id [ assetWithActions.asset.id ] True
                                            |> Cmd.map toMsg
                                in
                                { updatedContext = updatedContext
                                , cmd = addToAlbumCmd
                                }
                            )
                        |> Maybe.withDefault
                            { updatedContext = newContext
                            , cmd = Cmd.none
                            }

                _ ->
                    { updatedContext = newContext
                    , cmd = Cmd.none
                    }

        Immich.PaginatedImagesFetched (Ok paginatedResponse) ->
            -- Note: processPaginatedResponse logic is kept in Main.elm
            -- because it needs to call switchToEditIfAssetFound
            { updatedContext = newContext
            , cmd = Cmd.none
            }

        Immich.MoreImagesFetched page (Ok paginatedResponse) ->
            -- Note: processPaginatedResponse logic is kept in Main.elm
            { updatedContext = newContext
            , cmd = Cmd.none
            }

        Immich.PaginatedImagesFetched (Err _) ->
            -- Note: checkIfLoadingComplete logic is kept in Main.elm
            { updatedContext = newContext
            , cmd = Cmd.none
            }

        Immich.MoreImagesFetched _ (Err _) ->
            -- Note: checkIfLoadingComplete logic is kept in Main.elm
            { updatedContext = newContext
            , cmd = Cmd.none
            }

        Immich.AlbumFetchedWithClientSideFiltering order mediaType status (Ok album) ->
            -- Trigger paginated fetch for album assets
            let
                fetchCmd =
                    Immich.fetchAlbumAssetsWithFilters newContext.immichApiPaths album.id order mediaType status
                        |> Cmd.map toMsg
            in
            { updatedContext = newContext
            , cmd = fetchCmd
            }

        _ ->
            -- Note: checkIfLoadingComplete logic is kept in Main.elm
            { updatedContext = newContext
            , cmd = Cmd.none
            }



-- HELPER FUNCTIONS (adapted from Main.elm)


handleFetchAssetMembership : Immich.AssetWithMembership -> ImmichMsgContext -> ImmichMsgContext
handleFetchAssetMembership assetWithMembership context =
    case Dict.get assetWithMembership.assetId context.knownAssets of
        Nothing ->
            context

        Just asset ->
            let
                newAsset =
                    { asset | albumMembership = assetWithMembership.albumIds }

                updatedContext =
                    { context | knownAssets = Dict.insert assetWithMembership.assetId newAsset context.knownAssets }

                -- Check if we're currently viewing this asset and need to update the view state
                currentAssetId =
                    updatedContext.currentAssets
                        |> List.drop updatedContext.imageIndex
                        |> List.head

                isCurrentlyViewingThisAsset =
                    currentAssetId == Just assetWithMembership.assetId

                finalContext =
                    if isCurrentlyViewingThisAsset then
                        case updatedContext.userMode of
                            ViewAssets (EditAsset inputMode currentAsset search) ->
                                let
                                    updatedAsset =
                                        ViewAlbums.getAssetWithActions newAsset
                                            |> (\a -> { a | isVideoLoaded = currentAsset.isVideoLoaded })
                                in
                                { updatedContext | userMode = ViewAssets (EditAsset inputMode updatedAsset search) }

                            ViewAssets (CreateAlbumConfirmation inputMode currentAsset search albumName) ->
                                let
                                    updatedAsset =
                                        ViewAlbums.getAssetWithActions newAsset
                                            |> (\a -> { a | isVideoLoaded = currentAsset.isVideoLoaded })
                                in
                                { updatedContext | userMode = ViewAssets (CreateAlbumConfirmation inputMode updatedAsset search albumName) }

                            ViewAssets (ShowEditAssetHelp inputMode currentAsset search) ->
                                let
                                    updatedAsset =
                                        ViewAlbums.getAssetWithActions newAsset
                                            |> (\a -> { a | isVideoLoaded = currentAsset.isVideoLoaded })
                                in
                                { updatedContext | userMode = ViewAssets (ShowEditAssetHelp inputMode updatedAsset search) }

                            _ ->
                                updatedContext

                    else
                        updatedContext
            in
            finalContext


handleFetchAssets : List ImmichAsset -> ImmichMsgContext -> ImmichMsgContext
handleFetchAssets assets context =
    let
        result =
            UpdateImmich.handleFetchAssetsResult
                { assets = assets
                , currentConfig = context.paginationState.currentConfig
                , currentKnownAssets = context.knownAssets
                , currentImageIndex = context.imageIndex
                }

        updatedContext =
            { context
                | knownAssets = result.knownAssets
                , currentAssets = result.currentAssets
                , imagesLoadState = result.imagesLoadState
                , imageIndex = result.imageIndex
            }
    in
    -- Note: Timeline sync is handled in Main.elm since it calls switchToEditIfAssetFound
    updatedContext


handleFetchAlbums : Bool -> List ImmichAlbum -> ImmichMsgContext -> ImmichMsgContext
handleFetchAlbums showReloadFeedback albums context =
    let
        result =
            UpdateImmich.handleFetchAlbumsResult
                { showReloadFeedback = showReloadFeedback
                , albums = albums
                , currentKnownAlbums = context.knownAlbums
                , currentReloadFeedback = context.reloadFeedback
                }
    in
    { context
        | knownAlbums = result.knownAlbums
        , albumsLoadState = result.albumsLoadState
        , albumKeybindings = result.albumKeybindings
        , reloadFeedback = result.reloadFeedback
    }


handleUpdateLoadingState : AssetSourceUpdate -> ImmichMsgContext -> ImmichMsgContext
handleUpdateLoadingState updateType context =
    case context.userMode of
        LoadingAssets loadState ->
            { context | userMode = LoadingAssets (LoadState.updateLoadStateForFetch updateType loadState) }

        _ ->
            context


updateAlbumAssetCount : ImmichAlbumId -> Int -> ImmichMsgContext -> ImmichMsgContext
updateAlbumAssetCount albumId countChange context =
    { context | knownAlbums = UpdateImmich.updateAlbumAssetCount albumId countChange context.knownAlbums }


getCurrentAssetWithActions : ImmichMsgContext -> Maybe ( AssetWithActions, AlbumSearch )
getCurrentAssetWithActions context =
    Navigation.getCurrentAssetWithActions
        context.currentAssets
        context.imageIndex
        context.knownAssets
        context.knownAlbums
        context.screenHeight


updatePaginationState : Immich.PaginatedAssetResponse -> Int -> ImmichMsgContext -> ImmichMsgContext
updatePaginationState paginatedResponse page context =
    { context
        | paginationState =
            Pagination.updatePaginationStateFromResponse paginatedResponse page context.paginationState
    }


appendFetchedAssets : List ImmichAsset -> ImmichMsgContext -> ImmichMsgContext
appendFetchedAssets newAssets context =
    let
        result =
            Pagination.appendAssetsResult
                newAssets
                context.knownAssets
                context.currentAssets
                context.imageIndex
                context.paginationState.currentConfig

        updatedContext =
            { context
                | knownAssets = result.knownAssets
                , currentAssets = result.currentAssets
                , imageIndex = result.imageIndex
            }
    in
    -- Note: Timeline sync is handled in Main.elm since it calls switchToEditIfAssetFound
    updatedContext
