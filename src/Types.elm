module Types exposing
    ( AlbumPaginationContext
    , AssetSource(..)
    , AssetSourceUpdate(..)
    , ImageIndex
    , NavigationHistoryEntry
    , PaginationState
    , SourceLoadState
    , UserMode(..)
    )

import Immich exposing (ImageOrder, ImageSearchConfig, ImmichAlbum, ImmichAlbumId, ImmichAssetId, MediaTypeFilter, SearchContext, StatusFilter)
import Menus exposing (AlbumConfig)
import UpdateAsset exposing (AssetState)
import UpdateMenus exposing (MenuState)


{-| Simple type alias for image index within current assets list
-}
type alias ImageIndex =
    Int


{-| Pagination context for album-based asset loading
-}
type alias AlbumPaginationContext =
    { albumId : ImmichAlbumId
    , order : ImageOrder
    , mediaType : MediaTypeFilter
    , status : StatusFilter
    }


{-| State of pagination for the current asset source
-}
type alias PaginationState =
    { currentConfig : Maybe ImageSearchConfig
    , currentQuery : Maybe String
    , currentSearchContext : Maybe SearchContext
    , currentAlbumContext : Maybe AlbumPaginationContext
    , totalAssets : Int
    , currentPage : Int
    , hasMorePages : Bool
    , isLoadingMore : Bool
    , loadedAssets : Int
    , maxAssetsToFetch : Int
    }


{-| Source of the current assets being viewed
-}
type AssetSource
    = NoAssets
    | ImageSearch ImageSearchConfig
    | TextSearch String SearchContext
    | Album ImmichAlbum
    | FilteredAlbum ImmichAlbum AlbumConfig


{-| Loading state for asset sources
-}
type alias SourceLoadState =
    { fetchedAssetList : Maybe Bool
    , fetchedAssetMembership : Maybe Bool
    }


{-| Enum for tracking which part of loading completed
-}
type AssetSourceUpdate
    = FetchedAssetList


{-| The current user mode/view state
-}
type UserMode
    = MainMenu MenuState
    | ViewAssets AssetState
    | LoadingAssets SourceLoadState


{-| Entry in the navigation history stack
-}
type alias NavigationHistoryEntry =
    { userMode : UserMode
    , currentAssetsSource : AssetSource
    , currentAssets : List ImmichAssetId
    , imageIndex : ImageIndex
    , paginationState : PaginationState
    }
