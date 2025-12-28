module UpdateMenuResultTest exposing (..)

import AssetSourceTypes exposing (AssetSource(..))
import Date
import Expect
import Immich exposing (CategorisationFilter(..), ImageOrder(..), ImageSearchConfig, ImmichAlbum, MediaTypeFilter(..), SearchContext(..), StatusFilter(..))
import Menus exposing (AlbumConfig)
import Test exposing (Test, describe, test)
import Types exposing (PaginationState)
import UpdateMenuResult exposing (MenuResultAction(..), processMenuResult)
import UpdateMenus exposing (MenuResult(..), MenuState(..))


{-| Create a default PaginationState for testing
-}
defaultPaginationState : PaginationState
defaultPaginationState =
    { currentConfig = Nothing
    , currentQuery = Nothing
    , currentSearchContext = Nothing
    , currentAlbumContext = Nothing
    , totalAssets = 0
    , currentPage = 1
    , hasMorePages = False
    , isLoadingMore = False
    , loadedAssets = 0
    , maxAssetsToFetch = 10000
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


suite : Test
suite =
    describe "UpdateMenuResult Pure Functions"
        [ describe "processMenuResult"
            [ test "StayInMenu returns StayInMenuAction with menu state" <|
                \_ ->
                    let
                        result =
                            processMenuResult (StayInMenu MainMenuHome) defaultPaginationState Nothing
                    in
                    case result of
                        StayInMenuAction state ->
                            case state of
                                MainMenuHome ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Expected MainMenuHome"

                        _ ->
                            Expect.fail "Expected StayInMenuAction"
            , test "MenuLoadAssets with ImageSearch returns LoadAssetsAction" <|
                \_ ->
                    let
                        searchConfig =
                            { order = CreatedDesc
                            , categorisation = Uncategorised
                            , mediaType = AllMedia
                            , status = AllStatuses
                            }

                        result =
                            processMenuResult (MenuLoadAssets (ImageSearch searchConfig)) defaultPaginationState Nothing
                    in
                    case result of
                        LoadAssetsAction config _ ->
                            case config.assetSource of
                                ImageSearch _ ->
                                    Expect.all
                                        [ \c -> Expect.equal (Just searchConfig) c.paginationState.currentConfig
                                        , \c -> Expect.equal Nothing c.paginationState.currentQuery
                                        , \c -> Expect.equal 10000 c.paginationState.maxAssetsToFetch
                                        ]
                                        config

                                _ ->
                                    Expect.fail "Expected ImageSearch asset source"

                        _ ->
                            Expect.fail "Expected LoadAssetsAction"
            , test "MenuLoadAssets with TextSearch returns LoadAssetsAction with query" <|
                \_ ->
                    let
                        query =
                            "cats"

                        searchContext =
                            ContentSearch

                        result =
                            processMenuResult (MenuLoadAssets (TextSearch query searchContext)) defaultPaginationState Nothing
                    in
                    case result of
                        LoadAssetsAction config _ ->
                            case config.assetSource of
                                TextSearch q ctx ->
                                    Expect.all
                                        [ \_ -> Expect.equal "cats" q
                                        , \_ -> Expect.equal ContentSearch ctx
                                        , \c -> Expect.equal (Just "cats") c.paginationState.currentQuery
                                        , \c -> Expect.equal (Just ContentSearch) c.paginationState.currentSearchContext
                                        ]
                                        config

                                _ ->
                                    Expect.fail "Expected TextSearch asset source"

                        _ ->
                            Expect.fail "Expected LoadAssetsAction"
            , test "MenuLoadAssets with FilteredAlbum returns LoadAssetsAction with album context" <|
                \_ ->
                    let
                        album =
                            createAlbum "album-123" "Test Album"

                        albumConfig =
                            { order = ModifiedDesc
                            , mediaType = VideosOnly
                            , status = FavoritesOnly
                            , moveFromMode = False
                            }

                        result =
                            processMenuResult (MenuLoadAssets (FilteredAlbum album albumConfig)) defaultPaginationState Nothing
                    in
                    case result of
                        LoadAssetsAction config _ ->
                            case config.assetSource of
                                FilteredAlbum a c ->
                                    Expect.all
                                        [ \_ -> Expect.equal "album-123" a.id
                                        , \_ -> Expect.equal ModifiedDesc c.order
                                        , \_ -> Expect.equal VideosOnly c.mediaType
                                        , \c2 ->
                                            case c2.paginationState.currentAlbumContext of
                                                Just ctx ->
                                                    Expect.equal "album-123" ctx.albumId

                                                Nothing ->
                                                    Expect.fail "Expected album context"
                                        ]
                                        config

                                _ ->
                                    Expect.fail "Expected FilteredAlbum asset source"

                        _ ->
                            Expect.fail "Expected LoadAssetsAction"
            , test "MenuReloadAlbums returns ReloadAlbumsAction" <|
                \_ ->
                    let
                        result =
                            processMenuResult MenuReloadAlbums defaultPaginationState Nothing
                    in
                    Expect.equal ReloadAlbumsAction result
            , test "MenuUpdateSearchInput returns UpdateSearchFocusAction" <|
                \_ ->
                    let
                        result =
                            processMenuResult (MenuUpdateSearchInput True) defaultPaginationState Nothing
                    in
                    case result of
                        UpdateSearchFocusAction focused ->
                            Expect.equal True focused

                        _ ->
                            Expect.fail "Expected UpdateSearchFocusAction"
            , test "preserves maxAssetsToFetch from current pagination state" <|
                \_ ->
                    let
                        customPaginationState =
                            { defaultPaginationState | maxAssetsToFetch = 5000 }

                        searchConfig =
                            { order = CreatedAsc
                            , categorisation = All
                            , mediaType = ImagesOnly
                            , status = AllStatuses
                            }

                        result =
                            processMenuResult (MenuLoadAssets (ImageSearch searchConfig)) customPaginationState Nothing
                    in
                    case result of
                        LoadAssetsAction config _ ->
                            Expect.equal 5000 config.paginationState.maxAssetsToFetch

                        _ ->
                            Expect.fail "Expected LoadAssetsAction"
            , test "resets pagination counters for new load" <|
                \_ ->
                    let
                        stateWithProgress =
                            { defaultPaginationState
                                | currentPage = 5
                                , loadedAssets = 4000
                                , totalAssets = 10000
                                , hasMorePages = True
                            }

                        searchConfig =
                            { order = CreatedDesc
                            , categorisation = Uncategorised
                            , mediaType = AllMedia
                            , status = AllStatuses
                            }

                        result =
                            processMenuResult (MenuLoadAssets (ImageSearch searchConfig)) stateWithProgress Nothing
                    in
                    case result of
                        LoadAssetsAction config _ ->
                            Expect.all
                                [ \c -> Expect.equal 1 c.paginationState.currentPage
                                , \c -> Expect.equal 0 c.paginationState.loadedAssets
                                , \c -> Expect.equal 0 c.paginationState.totalAssets
                                , \c -> Expect.equal False c.paginationState.hasMorePages
                                ]
                                config

                        _ ->
                            Expect.fail "Expected LoadAssetsAction"
            ]
        ]
