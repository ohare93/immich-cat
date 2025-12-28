module UpdateMenuResult exposing
    ( LoadAssetsConfig
    , LoadType(..)
    , MenuResultAction(..)
    , processMenuResult
    )

{-| Pure functions for processing MenuResult values.

This module extracts the pure computation from menu result handling,
leaving only Cmd generation in Main.elm.

-}

import AssetSourceTypes exposing (AlbumConfig, AssetSource(..), defaultAlbumConfig)
import Immich exposing (ImageSearchConfig, ImmichAlbum, SearchContext)
import Menus
import Types exposing (AlbumPaginationContext, PaginationState, SourceLoadState, UserMode(..))
import UpdateMenus exposing (MenuResult(..), MenuState(..))


{-| Configuration for loading assets.
Contains all the pure data needed to generate the load command.
-}
type alias LoadAssetsConfig =
    { assetSource : AssetSource
    , paginationState : PaginationState
    , loadType : LoadType
    }


{-| Type of asset loading to perform.
Main.elm uses this to generate the appropriate Cmd.
-}
type LoadType
    = TimelineLoad ImageSearchConfig
    | TextSearchLoad String SearchContext
    | AlbumLoad ImmichAlbum AlbumConfig


{-| Actions computed from a MenuResult.
Main.elm pattern-matches on this to generate Cmds.
-}
type MenuResultAction
    = StayInMenuAction MenuState
    | LoadAssetsAction LoadAssetsConfig LoadType
    | ReloadAlbumsAction
    | UpdateSearchFocusAction Bool


{-| Process a MenuResult into a pure action.
Returns a MenuResultAction that Main.elm uses for Cmd generation.
-}
processMenuResult : MenuResult msg -> PaginationState -> Maybe UserMode -> MenuResultAction
processMenuResult menuResult currentPaginationState maybeUserMode =
    case menuResult of
        StayInMenu newMenuState ->
            StayInMenuAction newMenuState

        MenuLoadAssets assetSource ->
            let
                ( loadType, paginationConfig ) =
                    case assetSource of
                        ImageSearch searchConfig ->
                            ( TimelineLoad searchConfig
                            , { currentConfig = Just searchConfig
                              , currentQuery = Nothing
                              , currentSearchContext = Nothing
                              , currentAlbumContext = Nothing
                              , totalAssets = 0
                              , currentPage = 1
                              , hasMorePages = False
                              , isLoadingMore = False
                              , loadedAssets = 0
                              , maxAssetsToFetch = currentPaginationState.maxAssetsToFetch
                              }
                            )

                        TextSearch query searchContext ->
                            ( TextSearchLoad query searchContext
                            , { currentConfig = Nothing
                              , currentQuery = Just query
                              , currentSearchContext = Just searchContext
                              , currentAlbumContext = Nothing
                              , totalAssets = 0
                              , currentPage = 1
                              , hasMorePages = False
                              , isLoadingMore = False
                              , loadedAssets = 0
                              , maxAssetsToFetch = currentPaginationState.maxAssetsToFetch
                              }
                            )

                        FilteredAlbum album config ->
                            let
                                albumContext =
                                    { albumId = album.id
                                    , order = config.order
                                    , mediaType = config.mediaType
                                    , status = config.status
                                    }
                            in
                            ( AlbumLoad album config
                            , { currentConfig = Nothing
                              , currentQuery = Nothing
                              , currentSearchContext = Nothing
                              , currentAlbumContext = Just albumContext
                              , totalAssets = 0
                              , currentPage = 1
                              , hasMorePages = False
                              , isLoadingMore = False
                              , loadedAssets = 0
                              , maxAssetsToFetch = currentPaginationState.maxAssetsToFetch
                              }
                            )

                        NoAssets ->
                            -- NoAssets shouldn't be produced by menus, but handle it defensively
                            ( TimelineLoad { order = Immich.CreatedDesc, categorisation = Immich.All, mediaType = Immich.AllMedia, status = Immich.AllStatuses }
                            , { currentConfig = Nothing
                              , currentQuery = Nothing
                              , currentSearchContext = Nothing
                              , currentAlbumContext = Nothing
                              , totalAssets = 0
                              , currentPage = 1
                              , hasMorePages = False
                              , isLoadingMore = False
                              , loadedAssets = 0
                              , maxAssetsToFetch = currentPaginationState.maxAssetsToFetch
                              }
                            )

                        Album album ->
                            -- Album without config shouldn't be produced by menus, use default config
                            let
                                config =
                                    defaultAlbumConfig

                                albumContext =
                                    { albumId = album.id
                                    , order = config.order
                                    , mediaType = config.mediaType
                                    , status = config.status
                                    }
                            in
                            ( AlbumLoad album config
                            , { currentConfig = Nothing
                              , currentQuery = Nothing
                              , currentSearchContext = Nothing
                              , currentAlbumContext = Just albumContext
                              , totalAssets = 0
                              , currentPage = 1
                              , hasMorePages = False
                              , isLoadingMore = False
                              , loadedAssets = 0
                              , maxAssetsToFetch = currentPaginationState.maxAssetsToFetch
                              }
                            )
            in
            LoadAssetsAction
                { assetSource = assetSource
                , paginationState = paginationConfig
                , loadType = loadType
                }
                loadType

        MenuReloadAlbums ->
            ReloadAlbumsAction

        MenuUpdateSearchInput focused ->
            UpdateSearchFocusAction focused


{-| Update search input focus in a menu state.
Pure function that returns updated MenuState if applicable.
-}
updateSearchFocus : Bool -> MenuState -> Maybe MenuState
updateSearchFocus focused menuState =
    case menuState of
        SearchView config ->
            Just (SearchView { config | inputFocused = focused })

        _ ->
            Nothing
