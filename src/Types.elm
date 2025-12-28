module Types exposing
    ( AlbumPaginationContext
    , AssetSourceUpdate(..)
    , FeedbackMessage(..)
    , ImageIndex
    , NavigationHistoryEntry
    , PaginationState
    , SourceLoadState
    , UserMode(..)
    , feedbackMessageToString
    )

import Array exposing (Array)
import AssetSourceTypes exposing (AlbumConfig, AssetSource(..))
import Immich exposing (ImageOrder, ImageSearchConfig, ImmichAlbum, ImmichAlbumId, ImmichAssetId, MediaTypeFilter, SearchContext, StatusFilter)
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


{-| Loading state for asset sources
-}
type alias SourceLoadState =
    { fetchedAssetList : Maybe Bool
    , fetchedAssetMembership : Maybe Bool
    }


{-| Feedback messages for user notifications
-}
type FeedbackMessage
    = AlbumsLoaded Int
    | AlbumsReloaded Int
    | NoAlbumsFound
    | AlbumMembershipFetchFailed String


{-| Convert FeedbackMessage to a displayable string
-}
feedbackMessageToString : FeedbackMessage -> String
feedbackMessageToString msg =
    case msg of
        AlbumsLoaded count ->
            "Loaded " ++ String.fromInt count ++ " albums"

        AlbumsReloaded count ->
            "Reloaded " ++ String.fromInt count ++ " albums"

        NoAlbumsFound ->
            "No albums found"

        AlbumMembershipFetchFailed error ->
            "Album membership fetch failed: " ++ error


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
    , currentAssets : Array ImmichAssetId
    , imageIndex : ImageIndex
    , paginationState : PaginationState
    }
