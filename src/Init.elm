module Init exposing (init)

import Dict
import Immich exposing (CategorisationFilter(..), ImageOrder(..), ImmichLoadState(..), MediaTypeFilter(..), StatusFilter(..), getImmichApiPaths)
import Theme exposing (classifyDevice)
import Types exposing (Flags, Model, Msg(..))
import UpdateMenus exposing (MenuState(..))
import ViewAsset exposing (TimeViewMode(..))


init : (String -> Cmd Msg) -> Flags -> ( Model, Cmd Msg )
init loadFromStorage flags =
    ( { key = ""
      , userMode = Types.MainMenu MainMenuHome
      , currentAssetsSource = Types.NoAssets
      , currentDateMillis = flags.currentDateMillis
      , imageIndex = 0
      , imageSearchConfig = { order = CreatedDesc, categorisation = Uncategorised, mediaType = AllMedia, status = AllStatuses }
      , timeViewMode = Absolute
      , reloadFeedback = Nothing
      , controlPressed = False
      , altPressed = False

      -- Navigation history (stack + current + queue model)
      , navigationBackStack = []
      , currentNavigationState = Nothing
      , navigationForwardQueue = []

      -- Configuration fields
      , configuredApiUrl = Nothing
      , configuredApiKey = Nothing
      , settingsApiUrl = flags.immichApiUrl
      , settingsApiKey = flags.immichApiKey
      , configValidationMessage = Nothing

      -- Immich fields
      , currentAssets = []
      , knownAssets = Dict.empty
      , imagesLoadState = ImmichLoading
      , knownAlbums = Dict.empty
      , albumKeybindings = Dict.empty
      , albumsLoadState = ImmichLoading
      , baseUrl = flags.immichApiUrl
      , apiKey = flags.immichApiKey
      , envBaseUrl = flags.immichApiUrl
      , envApiKey = flags.immichApiKey
      , immichApiPaths = getImmichApiPaths flags.immichApiUrl flags.immichApiKey
      , screenHeight = 800 -- Default, will be updated by window resize
      , screenWidth = 1200 -- Default, will be updated by window resize
      , deviceClass = classifyDevice 1200 800 -- Will be updated by WindowResize
      , theme = Types.Dark -- Default to dark theme
      , pendingAlbumChange = Nothing
      , paginationState =
            { currentConfig = Nothing
            , currentQuery = Nothing
            , totalAssets = 0
            , currentPage = 1
            , hasMorePages = False
            , isLoadingMore = False
            , loadedAssets = 0
            , maxAssetsToFetch = 10000 -- Default limit of 10,000 assets
            }
      }
    , Cmd.batch
        [ loadFromStorage "immichApiUrl"
        , loadFromStorage "immichApiKey"

        -- Don't load albums immediately - wait for localStorage config to load first
        -- to avoid duplicate API calls with different credentials
        ]
    )
