module UpdateAssetResultTest exposing (..)

import AssetSourceTypes exposing (AlbumConfig, AssetSource(..), defaultAlbumConfig)
import Date
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Immich exposing (CategorisationFilter(..), ImageOrder(..), ImageSearchConfig, ImmichAlbum, ImmichAsset, ImmichAssetId, MediaTypeFilter(..), SearchContext(..), StatusFilter(..))
import Test exposing (Test, describe, fuzz, test)
import TestGenerators exposing (defaultAlbumSearch, defaultImageSearchConfig, defaultPaginationState)
import Types exposing (ImageIndex, PaginationState, UserMode(..))
import UpdateAsset exposing (AssetResult(..), AssetState(..))
import UpdateAssetResult exposing (AssetResultAction(..), processAssetResult, processToggleAlbumMembership)
import UpdateMenus exposing (MenuState(..))
import ViewAlbums exposing (AssetWithActions, InputMode(..), PropertyChange(..), flipPropertyChange)
import ViewAsset exposing (TimeViewMode(..))
import ViewGrid


{-| Create a test ImmichAsset
-}
createAsset : String -> ImmichAsset
createAsset id =
    let
        dummyDate =
            Date.fromRataDie 0
    in
    { id = id
    , path = "/path/to/asset/" ++ id
    , title = "Test Asset " ++ id
    , mimeType = "image/jpeg"
    , fileCreatedAt = dummyDate
    , fileModifiedAt = dummyDate
    , fileCreatedAtString = "2024-01-01T00:00:00.000Z"
    , fileModifiedAtString = "2024-01-01T00:00:00.000Z"
    , isFavourite = False
    , isArchived = False
    , albumMembership = []
    , duration = Nothing
    , thumbhash = Nothing
    }


{-| Create a test ImmichAlbum
-}
createAlbum : String -> String -> ImmichAlbum
createAlbum id name =
    { id = id
    , albumName = name
    , assetCount = 0
    , assets = []
    , createdAt = Date.fromRataDie 0
    }


{-| Create a test AssetWithActions
-}
createAssetWithActions : String -> Bool -> Bool -> AssetWithActions
createAssetWithActions id isFav isArch =
    { asset = createAsset id
    , isFavourite =
        if isFav then
            RemainTrue

        else
            RemainFalse
    , isArchived =
        if isArch then
            RemainTrue

        else
            RemainFalse
    , albumMembership = Dict.empty
    , isVideoLoaded = False
    }


{-| Create a default context for testing
-}
defaultContext : UpdateAssetResult.AssetResultContext
defaultContext =
    { userMode = MainMenu MainMenuHome
    , currentAssetsSource = ImageSearch defaultImageSearchConfig
    , currentAssets = []
    , imageIndex = 0
    , paginationState = defaultPaginationState
    , currentNavigationState = Nothing
    , navigationBackStack = []
    , screenHeight = 800
    , timeViewMode = Absolute
    , baseUrl = "http://localhost:2283"
    }


{-| Create a context with ViewAssets EditAsset mode
-}
editAssetContext : AssetWithActions -> UpdateAssetResult.AssetResultContext
editAssetContext asset =
    { defaultContext
        | userMode = ViewAssets (EditAsset NormalMode asset defaultAlbumSearch)
        , currentAssets = [ asset.asset.id ]
    }


{-| Toggle favorite state of an AssetWithActions
-}
toggleFavorite : AssetWithActions -> AssetWithActions
toggleFavorite asset =
    { asset | isFavourite = flipPropertyChange asset.isFavourite }


{-| Toggle archived state of an AssetWithActions
-}
toggleArchived : AssetWithActions -> AssetWithActions
toggleArchived asset =
    { asset | isArchived = flipPropertyChange asset.isArchived }


suite : Test
suite =
    describe "UpdateAssetResult Pure Functions"
        [ describe "processAssetResult"
            [ test "StayInAssets returns StayInAssetsAction" <|
                \_ ->
                    let
                        newAssetState =
                            GridView (ViewGrid.initGridState 1024 768)

                        result =
                            processAssetResult (StayInAssets newAssetState) defaultContext
                    in
                    case result of
                        StayInAssetsAction state ->
                            case state of
                                GridView _ ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Expected GridView state"

                        _ ->
                            Expect.fail "Expected StayInAssetsAction"
            , test "GoToMainMenu returns GoToMainMenuAction with navigation data when in ViewAssets" <|
                \_ ->
                    let
                        asset =
                            createAssetWithActions "test-asset" False False

                        context =
                            { defaultContext
                                | userMode = ViewAssets (EditAsset NormalMode asset defaultAlbumSearch)
                                , currentAssets = [ "test-asset" ]
                                , imageIndex = 0
                            }

                        result =
                            processAssetResult GoToMainMenu context
                    in
                    case result of
                        GoToMainMenuAction data ->
                            case data.currentEntry of
                                Just entry ->
                                    Expect.equal [ "test-asset" ] entry.currentAssets

                                Nothing ->
                                    Expect.fail "Expected currentEntry to be Just"

                        _ ->
                            Expect.fail "Expected GoToMainMenuAction"
            , test "GoToSearchView returns GoToSearchViewAction with query" <|
                \_ ->
                    let
                        result =
                            processAssetResult (GoToSearchView "cats") defaultContext
                    in
                    case result of
                        GoToSearchViewAction query ->
                            Expect.equal "cats" query

                        _ ->
                            Expect.fail "Expected GoToSearchViewAction"
            , test "AssetLoadTextSearch returns LoadTextSearchAction" <|
                \_ ->
                    let
                        result =
                            processAssetResult (AssetLoadTextSearch "dogs") defaultContext
                    in
                    case result of
                        LoadTextSearchAction query ->
                            Expect.equal "dogs" query

                        _ ->
                            Expect.fail "Expected LoadTextSearchAction"
            , test "AssetLoadAlbum returns LoadAlbumAction" <|
                \_ ->
                    let
                        album =
                            createAlbum "album-1" "Test Album"

                        result =
                            processAssetResult (AssetLoadAlbum album) defaultContext
                    in
                    case result of
                        LoadAlbumAction a ->
                            Expect.equal "album-1" a.id

                        _ ->
                            Expect.fail "Expected LoadAlbumAction"
            , test "AssetSwitchToAssetIndex returns SwitchToAssetIndexAction" <|
                \_ ->
                    let
                        result =
                            processAssetResult (AssetSwitchToAssetIndex 5) defaultContext
                    in
                    case result of
                        SwitchToAssetIndexAction index ->
                            Expect.equal 5 index

                        _ ->
                            Expect.fail "Expected SwitchToAssetIndexAction"
            , test "AssetToggleFavorite flips property change and returns ToggleFavoriteAction" <|
                \_ ->
                    let
                        asset =
                            createAssetWithActions "test-asset" False False

                        context =
                            editAssetContext asset

                        result =
                            processAssetResult AssetToggleFavorite context
                    in
                    case result of
                        ToggleFavoriteAction newAsset newValue ->
                            Expect.all
                                [ \_ -> Expect.equal True newValue
                                , \_ ->
                                    case newAsset.isFavourite of
                                        ChangeToTrue ->
                                            Expect.pass

                                        _ ->
                                            Expect.fail "Expected ChangeToTrue"
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected ToggleFavoriteAction"
            , test "AssetToggleFavorite returns NoAction when not in EditAsset mode" <|
                \_ ->
                    let
                        result =
                            processAssetResult AssetToggleFavorite defaultContext
                    in
                    Expect.equal NoAction result
            , test "AssetToggleArchived flips property change and returns ToggleArchivedAction" <|
                \_ ->
                    let
                        asset =
                            createAssetWithActions "test-asset" False False

                        context =
                            editAssetContext asset

                        result =
                            processAssetResult AssetToggleArchived context
                    in
                    case result of
                        ToggleArchivedAction newAsset newValue ->
                            Expect.all
                                [ \_ -> Expect.equal True newValue
                                , \_ ->
                                    case newAsset.isArchived of
                                        ChangeToTrue ->
                                            Expect.pass

                                        _ ->
                                            Expect.fail "Expected ChangeToTrue"
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected ToggleArchivedAction"
            , test "AssetOpenInImmich returns OpenInImmichAction with correct URL" <|
                \_ ->
                    let
                        asset =
                            createAssetWithActions "asset-123" False False

                        context =
                            editAssetContext asset

                        result =
                            processAssetResult AssetOpenInImmich context
                    in
                    case result of
                        OpenInImmichAction url ->
                            Expect.equal "http://localhost:2283/photos/asset-123" url

                        _ ->
                            Expect.fail "Expected OpenInImmichAction"
            , test "AssetYankToClipboard returns YankToClipboardAction with asset id" <|
                \_ ->
                    let
                        asset =
                            createAssetWithActions "asset-456" False False

                        context =
                            editAssetContext asset

                        result =
                            processAssetResult AssetYankToClipboard context
                    in
                    case result of
                        YankToClipboardAction assetId ->
                            Expect.equal "asset-456" assetId

                        _ ->
                            Expect.fail "Expected YankToClipboardAction"
            , test "AssetToggleMoveFromMode returns ToggleMoveFromModeAction when in FilteredAlbum" <|
                \_ ->
                    let
                        album =
                            createAlbum "source-album" "Source"

                        config =
                            { defaultAlbumConfig | moveFromMode = False }

                        context =
                            { defaultContext | currentAssetsSource = FilteredAlbum album config }

                        result =
                            processAssetResult AssetToggleMoveFromMode context
                    in
                    case result of
                        ToggleMoveFromModeAction a newConfig ->
                            Expect.all
                                [ \_ -> Expect.equal "source-album" a.id
                                , \_ -> Expect.equal True newConfig.moveFromMode
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected ToggleMoveFromModeAction"
            , test "AssetToggleMoveFromMode returns NoAction when not in FilteredAlbum" <|
                \_ ->
                    let
                        result =
                            processAssetResult AssetToggleMoveFromMode defaultContext
                    in
                    Expect.equal NoAction result
            , test "AssetCreateAlbum returns CreateAlbumAction with album name" <|
                \_ ->
                    let
                        result =
                            processAssetResult (AssetCreateAlbum "New Album") defaultContext
                    in
                    case result of
                        CreateAlbumAction name ->
                            Expect.equal "New Album" name

                        _ ->
                            Expect.fail "Expected CreateAlbumAction"
            , test "AssetToggleTimeView toggles from Absolute to Relative" <|
                \_ ->
                    let
                        context =
                            { defaultContext | timeViewMode = Absolute }

                        result =
                            processAssetResult AssetToggleTimeView context
                    in
                    case result of
                        ToggleTimeViewAction mode ->
                            Expect.equal Relative mode

                        _ ->
                            Expect.fail "Expected ToggleTimeViewAction"
            , test "AssetToggleTimeView toggles from Relative to Absolute" <|
                \_ ->
                    let
                        context =
                            { defaultContext | timeViewMode = Relative }

                        result =
                            processAssetResult AssetToggleTimeView context
                    in
                    case result of
                        ToggleTimeViewAction mode ->
                            Expect.equal Absolute mode

                        _ ->
                            Expect.fail "Expected ToggleTimeViewAction"
            , test "AssetSwitchToGridView returns SwitchToGridViewAction with grid state" <|
                \_ ->
                    let
                        context =
                            { defaultContext | screenHeight = 768 }

                        result =
                            processAssetResult AssetSwitchToGridView context
                    in
                    case result of
                        SwitchToGridViewAction _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected SwitchToGridViewAction"
            , test "AssetSwitchToDetailView returns Just index when asset found" <|
                \_ ->
                    let
                        context =
                            { defaultContext | currentAssets = [ "a1", "a2", "a3" ] }

                        result =
                            processAssetResult (AssetSwitchToDetailView "a2") context
                    in
                    case result of
                        SwitchToDetailViewAction maybeIndex ->
                            Expect.equal (Just 1) maybeIndex

                        _ ->
                            Expect.fail "Expected SwitchToDetailViewAction"
            , test "AssetSwitchToDetailView returns Nothing when asset not found" <|
                \_ ->
                    let
                        context =
                            { defaultContext | currentAssets = [ "a1", "a2", "a3" ] }

                        result =
                            processAssetResult (AssetSwitchToDetailView "not-found") context
                    in
                    case result of
                        SwitchToDetailViewAction maybeIndex ->
                            Expect.equal Nothing maybeIndex

                        _ ->
                            Expect.fail "Expected SwitchToDetailViewAction"
            , test "AssetGridUpdate returns GridUpdateAction" <|
                \_ ->
                    let
                        gridState =
                            ViewGrid.initGridState 1024 768

                        result =
                            processAssetResult (AssetGridUpdate gridState) defaultContext
                    in
                    case result of
                        GridUpdateAction _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected GridUpdateAction"
            , test "AssetBulkFavorite returns BulkFavoriteAction" <|
                \_ ->
                    let
                        result =
                            processAssetResult (AssetBulkFavorite [ "a1", "a2" ] True) defaultContext
                    in
                    case result of
                        BulkFavoriteAction ids isFav ->
                            Expect.all
                                [ \_ -> Expect.equal [ "a1", "a2" ] ids
                                , \_ -> Expect.equal True isFav
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected BulkFavoriteAction"
            , test "AssetBulkArchive returns BulkArchiveAction" <|
                \_ ->
                    let
                        result =
                            processAssetResult (AssetBulkArchive [ "a1" ] False) defaultContext
                    in
                    case result of
                        BulkArchiveAction ids isArch ->
                            Expect.all
                                [ \_ -> Expect.equal [ "a1" ] ids
                                , \_ -> Expect.equal False isArch
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected BulkArchiveAction"
            , test "AssetBulkAddToAlbum returns BulkAddToAlbumAction" <|
                \_ ->
                    let
                        result =
                            processAssetResult (AssetBulkAddToAlbum [ "a1", "a2" ] "album-1") defaultContext
                    in
                    case result of
                        BulkAddToAlbumAction ids albumId ->
                            Expect.all
                                [ \_ -> Expect.equal [ "a1", "a2" ] ids
                                , \_ -> Expect.equal "album-1" albumId
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected BulkAddToAlbumAction"
            , test "AssetBulkRemoveFromAlbum returns BulkRemoveFromAlbumAction" <|
                \_ ->
                    let
                        result =
                            processAssetResult (AssetBulkRemoveFromAlbum [ "a1" ] "album-2") defaultContext
                    in
                    case result of
                        BulkRemoveFromAlbumAction ids albumId ->
                            Expect.all
                                [ \_ -> Expect.equal [ "a1" ] ids
                                , \_ -> Expect.equal "album-2" albumId
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected BulkRemoveFromAlbumAction"
            , test "AssetRequestLoadMore returns RequestLoadMoreAction" <|
                \_ ->
                    let
                        result =
                            processAssetResult AssetRequestLoadMore defaultContext
                    in
                    Expect.equal RequestLoadMoreAction result
            , test "AssetReloadAlbums returns ReloadAlbumsAction" <|
                \_ ->
                    let
                        result =
                            processAssetResult AssetReloadAlbums defaultContext
                    in
                    Expect.equal ReloadAlbumsAction result
            ]
        , describe "Album membership toggle"
            [ test "AssetToggleAlbumMembership adds to album when not in it" <|
                \_ ->
                    let
                        asset =
                            createAssetWithActions "test-asset" False False

                        album =
                            createAlbum "album-1" "Test Album"

                        context =
                            editAssetContext asset

                        result =
                            processAssetResult (AssetToggleAlbumMembership album) context
                    in
                    case result of
                        ToggleAlbumMembershipAction data ->
                            Expect.all
                                [ \d -> Expect.equal "album-1" d.targetAlbumId
                                , \d -> Expect.equal True d.isAddition
                                , \d -> Expect.equal Nothing d.moveFromSourceId
                                , \d -> Expect.equal [ ( "album-1", True ) ] d.pendingChanges
                                ]
                                data

                        _ ->
                            Expect.fail "Expected ToggleAlbumMembershipAction"
            , test "Move-from mode: removes from source album when adding to different album" <|
                \_ ->
                    let
                        sourceAlbum =
                            createAlbum "source-album" "Source"

                        targetAlbum =
                            createAlbum "target-album" "Target"

                        baseAsset =
                            createAssetWithActions "test-asset" False False

                        assetInSourceAlbum =
                            { baseAsset | albumMembership = Dict.insert "source-album" RemainTrue baseAsset.albumMembership }

                        config =
                            { defaultAlbumConfig | moveFromMode = True }

                        baseContext =
                            editAssetContext assetInSourceAlbum

                        context =
                            { baseContext | currentAssetsSource = FilteredAlbum sourceAlbum config }

                        result =
                            processAssetResult (AssetToggleAlbumMembership targetAlbum) context
                    in
                    case result of
                        ToggleAlbumMembershipAction data ->
                            Expect.all
                                [ \d -> Expect.equal "target-album" d.targetAlbumId
                                , \d -> Expect.equal True d.isAddition
                                , \d -> Expect.equal (Just "source-album") d.moveFromSourceId
                                , \d -> Expect.equal [ ( "target-album", True ), ( "source-album", False ) ] d.pendingChanges
                                ]
                                data

                        _ ->
                            Expect.fail "Expected ToggleAlbumMembershipAction"
            , test "Move-from mode: does NOT remove from source when asset already removed from source" <|
                \_ ->
                    let
                        sourceAlbum =
                            createAlbum "source-album" "Source"

                        targetAlbum =
                            createAlbum "target-album" "Target"

                        baseAsset =
                            createAssetWithActions "test-asset" False False

                        assetRemovedFromSource =
                            { baseAsset | albumMembership = Dict.insert "source-album" ChangeToFalse baseAsset.albumMembership }

                        config =
                            { defaultAlbumConfig | moveFromMode = True }

                        baseContext =
                            editAssetContext assetRemovedFromSource

                        context =
                            { baseContext | currentAssetsSource = FilteredAlbum sourceAlbum config }

                        result =
                            processAssetResult (AssetToggleAlbumMembership targetAlbum) context
                    in
                    case result of
                        ToggleAlbumMembershipAction data ->
                            Expect.all
                                [ \d -> Expect.equal "target-album" d.targetAlbumId
                                , \d -> Expect.equal True d.isAddition
                                , \d -> Expect.equal Nothing d.moveFromSourceId
                                , \d -> Expect.equal [ ( "target-album", True ) ] d.pendingChanges
                                ]
                                data

                        _ ->
                            Expect.fail "Expected ToggleAlbumMembershipAction"
            , test "Move-from mode: does NOT remove from source when adding to same album as source" <|
                \_ ->
                    let
                        sourceAlbum =
                            createAlbum "source-album" "Source"

                        baseAsset =
                            createAssetWithActions "test-asset" False False

                        assetInSourceAlbum =
                            { baseAsset | albumMembership = Dict.insert "source-album" RemainTrue baseAsset.albumMembership }

                        config =
                            { defaultAlbumConfig | moveFromMode = True }

                        baseContext =
                            editAssetContext assetInSourceAlbum

                        context =
                            { baseContext | currentAssetsSource = FilteredAlbum sourceAlbum config }

                        result =
                            processAssetResult (AssetToggleAlbumMembership sourceAlbum) context
                    in
                    case result of
                        ToggleAlbumMembershipAction data ->
                            Expect.all
                                [ \d -> Expect.equal "source-album" d.targetAlbumId
                                , \d -> Expect.equal False d.isAddition
                                , \d -> Expect.equal Nothing d.moveFromSourceId
                                ]
                                data

                        _ ->
                            Expect.fail "Expected ToggleAlbumMembershipAction"
            , test "Move-from mode: does NOT trigger when removing from an album" <|
                \_ ->
                    let
                        sourceAlbum =
                            createAlbum "source-album" "Source"

                        targetAlbum =
                            createAlbum "target-album" "Target"

                        baseAsset =
                            createAssetWithActions "test-asset" False False

                        assetInBothAlbums =
                            { baseAsset
                                | albumMembership =
                                    Dict.fromList
                                        [ ( "source-album", RemainTrue )
                                        , ( "target-album", RemainTrue )
                                        ]
                            }

                        config =
                            { defaultAlbumConfig | moveFromMode = True }

                        baseContext =
                            editAssetContext assetInBothAlbums

                        context =
                            { baseContext | currentAssetsSource = FilteredAlbum sourceAlbum config }

                        result =
                            processAssetResult (AssetToggleAlbumMembership targetAlbum) context
                    in
                    case result of
                        ToggleAlbumMembershipAction data ->
                            Expect.all
                                [ \d -> Expect.equal "target-album" d.targetAlbumId
                                , \d -> Expect.equal False d.isAddition
                                , \d -> Expect.equal Nothing d.moveFromSourceId
                                ]
                                data

                        _ ->
                            Expect.fail "Expected ToggleAlbumMembershipAction"
            , test "Move-from mode disabled: does NOT remove from source even when adding to different album" <|
                \_ ->
                    let
                        sourceAlbum =
                            createAlbum "source-album" "Source"

                        targetAlbum =
                            createAlbum "target-album" "Target"

                        baseAsset =
                            createAssetWithActions "test-asset" False False

                        assetInSourceAlbum =
                            { baseAsset | albumMembership = Dict.insert "source-album" RemainTrue baseAsset.albumMembership }

                        config =
                            { defaultAlbumConfig | moveFromMode = False }

                        baseContext =
                            editAssetContext assetInSourceAlbum

                        context =
                            { baseContext | currentAssetsSource = FilteredAlbum sourceAlbum config }

                        result =
                            processAssetResult (AssetToggleAlbumMembership targetAlbum) context
                    in
                    case result of
                        ToggleAlbumMembershipAction data ->
                            Expect.all
                                [ \d -> Expect.equal "target-album" d.targetAlbumId
                                , \d -> Expect.equal True d.isAddition
                                , \d -> Expect.equal Nothing d.moveFromSourceId
                                , \d -> Expect.equal [ ( "target-album", True ) ] d.pendingChanges
                                ]
                                data

                        _ ->
                            Expect.fail "Expected ToggleAlbumMembershipAction"
            , test "Move-from mode: asset not in source album (RemainFalse) does NOT trigger" <|
                \_ ->
                    let
                        sourceAlbum =
                            createAlbum "source-album" "Source"

                        targetAlbum =
                            createAlbum "target-album" "Target"

                        baseAsset =
                            createAssetWithActions "test-asset" False False

                        assetNotInSource =
                            { baseAsset | albumMembership = Dict.insert "source-album" RemainFalse baseAsset.albumMembership }

                        config =
                            { defaultAlbumConfig | moveFromMode = True }

                        baseContext =
                            editAssetContext assetNotInSource

                        context =
                            { baseContext | currentAssetsSource = FilteredAlbum sourceAlbum config }

                        result =
                            processAssetResult (AssetToggleAlbumMembership targetAlbum) context
                    in
                    case result of
                        ToggleAlbumMembershipAction data ->
                            Expect.all
                                [ \d -> Expect.equal "target-album" d.targetAlbumId
                                , \d -> Expect.equal True d.isAddition
                                , \d -> Expect.equal Nothing d.moveFromSourceId
                                ]
                                data

                        _ ->
                            Expect.fail "Expected ToggleAlbumMembershipAction"
            ]
        , describe "Fuzz tests for property toggles"
            [ fuzz Fuzz.bool "toggle favorite twice returns to original state" <|
                \initialFavorite ->
                    let
                        asset =
                            createAssetWithActions "test" initialFavorite False

                        toggled =
                            toggleFavorite asset

                        toggledBack =
                            toggleFavorite toggled
                    in
                    case toggledBack.isFavourite of
                        RemainTrue ->
                            Expect.equal True initialFavorite

                        RemainFalse ->
                            Expect.equal False initialFavorite

                        _ ->
                            Expect.fail "Expected RemainTrue or RemainFalse after double toggle"
            , fuzz Fuzz.bool "toggle archived twice returns to original state" <|
                \initialArchived ->
                    let
                        asset =
                            createAssetWithActions "test" False initialArchived

                        toggled =
                            toggleArchived asset

                        toggledBack =
                            toggleArchived toggled
                    in
                    case toggledBack.isArchived of
                        RemainTrue ->
                            Expect.equal True initialArchived

                        RemainFalse ->
                            Expect.equal False initialArchived

                        _ ->
                            Expect.fail "Expected RemainTrue or RemainFalse after double toggle"
            , fuzz TestGenerators.testAlbumGenerator "album membership toggle creates pending change" <|
                \album ->
                    let
                        asset =
                            createAssetWithActions "test" False False

                        context =
                            editAssetContext asset

                        result =
                            processToggleAlbumMembership album context
                    in
                    case result of
                        ToggleAlbumMembershipAction data ->
                            data.pendingChanges
                                |> List.isEmpty
                                |> Expect.equal False

                        _ ->
                            Expect.pass
            ]
        ]
