module Types exposing
    ( AssetSource(..)
    , AssetSourceUpdate(..)
    , DeviceClass(..)
    , Flags
    , ImageIndex
    , Model
    , Msg(..)
    , NavigationHistoryEntry
    , PaginationState
    , SourceLoadState
    , Theme(..)
    , UserMode(..)
    )

import Browser.Events
import Dict exposing (Dict)
import Immich exposing (CategorisationFilter, ImageOrder, ImageSearchConfig, ImmichAlbum, ImmichAlbumId, ImmichApiPaths, ImmichAsset, ImmichAssetId, ImmichLoadState, MediaTypeFilter, StatusFilter)
import Menus exposing (AlbumConfig, SearchContext)
import UpdateAlbums exposing (AlbumMsg)
import UpdateAsset exposing (AssetMsg, AssetState)
import UpdateMenus exposing (MenuMsg, MenuState)
import ViewAsset exposing (TimeViewMode)


type Msg
    = KeyPress String
    | KeyRelease String
    | VisibilityChanged Browser.Events.Visibility
    | ImmichMsg Immich.Msg
    | LoadDataAgain
    | ClearReloadFeedback
    | SelectAlbum ImmichAlbum
    | WindowResize Int Int
    | ChangeTimelineMediaType MediaTypeFilter
    | ChangeTimelineCategorisation CategorisationFilter
    | ChangeTimelineOrder ImageOrder
    | ChangeTimelineStatus StatusFilter
    | ChangeSearchMediaType MediaTypeFilter
    | ChangeSearchContext SearchContext
    | ChangeSearchStatus StatusFilter
    | ChangeSearchQuery String
    | SelectSearchSuggestion String
    | ClearSearchQuery
    | ChangeAlbumMediaType MediaTypeFilter
    | ChangeAlbumOrder ImageOrder
    | ChangeAlbumStatus StatusFilter
    | LoadTimelineAssets
    | ExecuteSearch
    | LoadAlbumAssets ImmichAlbum
    | SearchInputFocused
    | SearchInputBlurred
      -- Config-related messages
    | SaveConfig String String
    | LoadConfig String
    | ConfigLoaded String (Maybe String)
    | ClearConfig
    | UpdateSettingsApiUrl String
    | UpdateSettingsApiKey String
      -- Module-specific messages
    | MenuMsg MenuMsg
    | AlbumMsg AlbumMsg
    | AssetMsg AssetMsg
      -- Theme messages
    | ToggleTheme


type alias Flags =
    { currentDateMillis : Int
    , immichApiKey : String
    , immichApiUrl : String
    }


type DeviceClass
    = Mobile
    | Tablet
    | Desktop


type Theme
    = Light
    | Dark
    | System


type alias Model =
    { key : String
    , currentAssetsSource : AssetSource
    , userMode : UserMode
    , currentDateMillis : Int
    , imageIndex : ImageIndex
    , imageSearchConfig : ImageSearchConfig
    , timeViewMode : TimeViewMode
    , reloadFeedback : Maybe String
    , controlPressed : Bool
    , altPressed : Bool

    -- Navigation history (stack + current + queue model)
    , navigationBackStack : List NavigationHistoryEntry
    , currentNavigationState : Maybe NavigationHistoryEntry
    , navigationForwardQueue : List NavigationHistoryEntry

    -- Configuration fields
    , configuredApiUrl : Maybe String
    , configuredApiKey : Maybe String
    , settingsApiUrl : String
    , settingsApiKey : String
    , configValidationMessage : Maybe String

    -- Immich fields
    , currentAssets : List ImmichAssetId
    , knownAssets : Dict ImmichAssetId ImmichAsset
    , imagesLoadState : ImmichLoadState
    , knownAlbums : Dict ImmichAlbumId ImmichAlbum
    , albumKeybindings : Dict ImmichAlbumId String
    , albumsLoadState : ImmichLoadState
    , baseUrl : String
    , apiKey : String
    , envBaseUrl : String -- Original env values for defaulting back
    , envApiKey : String
    , immichApiPaths : ImmichApiPaths
    , screenHeight : Int
    , screenWidth : Int
    , deviceClass : DeviceClass
    , theme : Theme
    , pendingAlbumChange : Maybe ( ImmichAlbumId, Bool ) -- (albumId, isAddition)
    , paginationState : PaginationState
    }


type alias PaginationState =
    { currentConfig : Maybe ImageSearchConfig
    , currentQuery : Maybe String
    , totalAssets : Int
    , currentPage : Int
    , hasMorePages : Bool
    , isLoadingMore : Bool
    , loadedAssets : Int
    , maxAssetsToFetch : Int -- Configurable limit
    }


type AssetSource
    = NoAssets
    | ImageSearch ImageSearchConfig
    | TextSearch String
    | Album ImmichAlbum
    | FilteredAlbum ImmichAlbum AlbumConfig


type alias SourceLoadState =
    { fetchedAssetList : Maybe Bool
    , fetchedAssetMembership : Maybe Bool
    }


type AssetSourceUpdate
    = FetchedAssetList



-- | FetchedAlbums


type UserMode
    = MainMenu MenuState
    | ViewAssets AssetState
    | LoadingAssets SourceLoadState


type alias NavigationHistoryEntry =
    { userMode : UserMode
    , currentAssetsSource : AssetSource
    , currentAssets : List ImmichAssetId
    , imageIndex : ImageIndex
    , paginationState : PaginationState
    }


type alias ImageIndex =
    Int
