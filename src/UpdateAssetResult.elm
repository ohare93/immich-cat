module UpdateAssetResult exposing
    ( AssetResultAction(..)
    , AssetResultContext
    , ToggleAlbumData
    , processAssetResult
    , processToggleAlbumMembership
    )

{-| Pure functions for processing AssetResult values.

This module extracts the pure computation from asset result handling,
leaving only Cmd generation in Main.elm.

-}

import AssetSourceTypes exposing (AlbumConfig, AssetSource(..))
import Dict exposing (Dict)
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichAsset, ImmichAssetId, SearchContext(..))
import Menus exposing (defaultSearchConfig)
import Types exposing (ImageIndex, NavigationHistoryEntry, PaginationState, UserMode(..))
import UpdateAsset exposing (AssetResult(..), AssetState(..))
import UpdateMenus exposing (MenuState(..))
import ViewAlbums exposing (AlbumSearch, AssetWithActions, InputMode(..), PropertyChange(..), flipPropertyChange, isAddingToAlbum, isCurrentlyInAlbum, resetPagination, toggleAssetAlbum)
import ViewAsset exposing (TimeViewMode(..))
import ViewGrid


{-| Data needed for album membership toggle operations.
-}
type alias ToggleAlbumData =
    { newAsset : AssetWithActions
    , newSearch : AlbumSearch
    , targetAlbumId : ImmichAlbumId
    , isAddition : Bool
    , moveFromSourceId : Maybe ImmichAlbumId
    , pendingChanges : List ( ImmichAlbumId, Bool )
    }


{-| Data for going to main menu.
-}
type alias GoToMainMenuData =
    { currentEntry : Maybe NavigationHistoryEntry
    , newBackStack : List NavigationHistoryEntry
    }


{-| Actions computed from an AssetResult.
Main.elm pattern-matches on this to generate Cmds.
-}
type AssetResultAction
    = StayInAssetsAction AssetState
    | GoToMainMenuAction GoToMainMenuData
    | GoToSearchViewAction String
    | LoadTextSearchAction String
    | LoadAlbumAction ImmichAlbum
    | SwitchToAssetIndexAction ImageIndex
    | ToggleFavoriteAction AssetWithActions Bool
    | ToggleArchivedAction AssetWithActions Bool
    | ToggleAlbumMembershipAction ToggleAlbumData
    | OpenInImmichAction String
    | YankToClipboardAction String
    | ToggleMoveFromModeAction ImmichAlbum AlbumConfig
    | CreateAlbumAction String
    | ToggleTimeViewAction TimeViewMode
    | SwitchToGridViewAction ViewGrid.GridState
    | SwitchToDetailViewAction (Maybe ImageIndex)
    | GridUpdateAction ViewGrid.GridState
    | BulkFavoriteAction (List ImmichAssetId) Bool
    | BulkArchiveAction (List ImmichAssetId) Bool
    | BulkAddToAlbumAction (List ImmichAssetId) ImmichAlbumId
    | BulkRemoveFromAlbumAction (List ImmichAssetId) ImmichAlbumId
    | RequestLoadMoreAction
    | ReloadAlbumsAction
    | NoAction


{-| Context needed for processing asset results.
-}
type alias AssetResultContext =
    { userMode : UserMode
    , currentAssetsSource : AssetSource
    , currentAssets : List ImmichAssetId
    , imageIndex : ImageIndex
    , paginationState : PaginationState
    , currentNavigationState : Maybe NavigationHistoryEntry
    , navigationBackStack : List NavigationHistoryEntry
    , screenHeight : Int
    , timeViewMode : TimeViewMode
    , baseUrl : String
    }


{-| Process an AssetResult into a pure action.
Returns an AssetResultAction that Main.elm uses for Cmd generation.
-}
processAssetResult : AssetResult msg -> AssetResultContext -> AssetResultAction
processAssetResult assetResult context =
    case assetResult of
        StayInAssets newAssetState ->
            StayInAssetsAction newAssetState

        GoToMainMenu ->
            processGoToMainMenu context

        GoToSearchView query ->
            GoToSearchViewAction query

        AssetLoadTextSearch query ->
            LoadTextSearchAction query

        AssetLoadAlbum album ->
            LoadAlbumAction album

        AssetSwitchToAssetIndex newIndex ->
            SwitchToAssetIndexAction newIndex

        AssetToggleFavorite ->
            processToggleFavorite context

        AssetToggleArchived ->
            processToggleArchived context

        AssetToggleAlbumMembership album ->
            processToggleAlbumMembership album context

        AssetOpenInImmich ->
            processOpenInImmich context

        AssetYankToClipboard ->
            processYankToClipboard context

        AssetToggleMoveFromMode ->
            processToggleMoveFromMode context

        AssetCreateAlbum albumName ->
            CreateAlbumAction albumName

        AssetToggleTimeView ->
            processToggleTimeView context

        AssetSwitchToGridView ->
            processSwitchToGridView context

        AssetSwitchToDetailView assetId ->
            processSwitchToDetailView assetId context

        AssetGridUpdate gridState ->
            GridUpdateAction gridState

        AssetBulkFavorite assetIds isFavorite ->
            BulkFavoriteAction assetIds isFavorite

        AssetBulkArchive assetIds isArchived ->
            BulkArchiveAction assetIds isArchived

        AssetBulkAddToAlbum assetIds albumId ->
            BulkAddToAlbumAction assetIds albumId

        AssetBulkRemoveFromAlbum assetIds albumId ->
            BulkRemoveFromAlbumAction assetIds albumId

        AssetRequestLoadMore ->
            RequestLoadMoreAction

        AssetReloadAlbums ->
            ReloadAlbumsAction


{-| Process GoToMainMenu: save current state to back stack.
-}
processGoToMainMenu : AssetResultContext -> AssetResultAction
processGoToMainMenu context =
    case context.userMode of
        ViewAssets _ ->
            let
                currentEntry =
                    { userMode = context.userMode
                    , currentAssetsSource = context.currentAssetsSource
                    , currentAssets = context.currentAssets
                    , imageIndex = context.imageIndex
                    , paginationState = context.paginationState
                    }

                newBackStack =
                    case context.currentNavigationState of
                        Just existing ->
                            List.take 19 (existing :: context.navigationBackStack)

                        Nothing ->
                            context.navigationBackStack
            in
            GoToMainMenuAction
                { currentEntry = Just currentEntry
                , newBackStack = newBackStack
                }

        _ ->
            GoToMainMenuAction
                { currentEntry = Nothing
                , newBackStack = context.navigationBackStack
                }


{-| Process AssetToggleFavorite.
-}
processToggleFavorite : AssetResultContext -> AssetResultAction
processToggleFavorite context =
    case context.userMode of
        ViewAssets assetState ->
            case assetState of
                EditAsset _ asset _ ->
                    let
                        newAsset =
                            { asset | isFavourite = flipPropertyChange asset.isFavourite }

                        newIsFavorite =
                            propertyChangeToBool newAsset.isFavourite
                    in
                    ToggleFavoriteAction newAsset newIsFavorite

                _ ->
                    NoAction

        _ ->
            NoAction


{-| Process AssetToggleArchived.
-}
processToggleArchived : AssetResultContext -> AssetResultAction
processToggleArchived context =
    case context.userMode of
        ViewAssets assetState ->
            case assetState of
                EditAsset _ asset _ ->
                    let
                        newAsset =
                            { asset | isArchived = flipPropertyChange asset.isArchived }

                        newIsArchived =
                            propertyChangeToBool newAsset.isArchived
                    in
                    ToggleArchivedAction newAsset newIsArchived

                _ ->
                    NoAction

        _ ->
            NoAction


{-| Process album membership toggle with move-from logic.

Used by both:

  - Keyboard path (AssetToggleAlbumMembership via UpdateAsset)
  - UI click path (SelectAlbum message in Main.elm)

Handles move-from mode: when enabled and viewing a FilteredAlbum,
adding to another album also removes from the source album.

-}
processToggleAlbumMembership : ImmichAlbum -> AssetResultContext -> AssetResultAction
processToggleAlbumMembership album context =
    case context.userMode of
        ViewAssets assetState ->
            case assetState of
                EditAsset _ asset search ->
                    let
                        currentPropertyChange =
                            Maybe.withDefault RemainFalse (Dict.get album.id asset.albumMembership)

                        currentlyInAlbum =
                            isCurrentlyInAlbum currentPropertyChange

                        toggledAsset =
                            toggleAssetAlbum asset album

                        newPropertyChange =
                            Maybe.withDefault RemainFalse (Dict.get album.id toggledAsset.albumMembership)

                        isAddition =
                            isAddingToAlbum newPropertyChange

                        newSearch =
                            { search | partialKeybinding = "", pagination = resetPagination search.pagination, invalidInputWarning = Nothing }

                        -- Check if we should also remove from source album (move-from mode)
                        ( finalAsset, moveFromSourceId, pendingChanges ) =
                            case context.currentAssetsSource of
                                FilteredAlbum sourceAlbum config ->
                                    let
                                        sourceAlbumState =
                                            Maybe.withDefault RemainFalse (Dict.get sourceAlbum.id asset.albumMembership)

                                        shouldRemoveFromSource =
                                            sourceAlbumState == RemainTrue
                                    in
                                    if config.moveFromMode && isAddition && album.id /= sourceAlbum.id && shouldRemoveFromSource then
                                        let
                                            assetWithSourceRemoved =
                                                { toggledAsset
                                                    | albumMembership =
                                                        Dict.insert sourceAlbum.id ChangeToFalse toggledAsset.albumMembership
                                                }
                                        in
                                        ( assetWithSourceRemoved
                                        , Just sourceAlbum.id
                                        , [ ( album.id, isAddition ), ( sourceAlbum.id, False ) ]
                                        )

                                    else
                                        ( toggledAsset, Nothing, [ ( album.id, isAddition ) ] )

                                _ ->
                                    ( toggledAsset, Nothing, [ ( album.id, isAddition ) ] )
                    in
                    ToggleAlbumMembershipAction
                        { newAsset = finalAsset
                        , newSearch = newSearch
                        , targetAlbumId = album.id
                        , isAddition = not currentlyInAlbum
                        , moveFromSourceId = moveFromSourceId
                        , pendingChanges = pendingChanges
                        }

                _ ->
                    NoAction

        _ ->
            NoAction


{-| Process AssetOpenInImmich.
-}
processOpenInImmich : AssetResultContext -> AssetResultAction
processOpenInImmich context =
    case context.userMode of
        ViewAssets assetState ->
            case assetState of
                EditAsset _ asset _ ->
                    let
                        immichUrl =
                            context.baseUrl ++ "/photos/" ++ asset.asset.id
                    in
                    OpenInImmichAction immichUrl

                _ ->
                    NoAction

        _ ->
            NoAction


{-| Process AssetYankToClipboard.
-}
processYankToClipboard : AssetResultContext -> AssetResultAction
processYankToClipboard context =
    case context.userMode of
        ViewAssets assetState ->
            case assetState of
                EditAsset _ asset _ ->
                    YankToClipboardAction asset.asset.id

                _ ->
                    NoAction

        _ ->
            NoAction


{-| Process AssetToggleMoveFromMode.
-}
processToggleMoveFromMode : AssetResultContext -> AssetResultAction
processToggleMoveFromMode context =
    case context.currentAssetsSource of
        FilteredAlbum album config ->
            let
                newConfig =
                    { config | moveFromMode = not config.moveFromMode }
            in
            ToggleMoveFromModeAction album newConfig

        _ ->
            NoAction


{-| Process AssetToggleTimeView.
-}
processToggleTimeView : AssetResultContext -> AssetResultAction
processToggleTimeView context =
    let
        newTimeViewMode =
            case context.timeViewMode of
                Absolute ->
                    Relative

                Relative ->
                    Absolute
    in
    ToggleTimeViewAction newTimeViewMode


{-| Process AssetSwitchToGridView.
-}
processSwitchToGridView : AssetResultContext -> AssetResultAction
processSwitchToGridView context =
    let
        -- Assume screen width is roughly 16:9 ratio of height
        screenWidth =
            context.screenHeight * 16 // 9

        gridState =
            ViewGrid.initGridState screenWidth context.screenHeight
    in
    SwitchToGridViewAction gridState


{-| Process AssetSwitchToDetailView.
-}
processSwitchToDetailView : ImmichAssetId -> AssetResultContext -> AssetResultAction
processSwitchToDetailView assetId context =
    let
        maybeIndex =
            context.currentAssets
                |> List.indexedMap
                    (\index id ->
                        if id == assetId then
                            Just index

                        else
                            Nothing
                    )
                |> List.filterMap identity
                |> List.head
    in
    SwitchToDetailViewAction maybeIndex


{-| Convert PropertyChange to Bool for API calls.
-}
propertyChangeToBool : PropertyChange -> Bool
propertyChangeToBool change =
    case change of
        ChangeToTrue ->
            True

        RemainTrue ->
            True

        ChangeToFalse ->
            False

        RemainFalse ->
            False
