module UpdateMenus exposing
    ( handleMainMenuInput
    , handleTimelineViewInput
    , handleSearchViewInput
    , handleAlbumViewInput
    , handleSettingsInput
    , updateMenus
    , MenuAction(..)
    , UserMode(..)
    , AssetSource(..)
    , MenuMsg(..)
    )

import Dict exposing (Dict)
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichApiPaths, ImmichAsset, ImmichAssetId, ImageOrder(..), CategorisationFilter(..), MediaTypeFilter(..), StatusFilter(..), ImageSearchConfig)
import Menus exposing (SearchContext(..), TimelineConfig, SearchConfig, AlbumConfig, defaultTimelineConfig, defaultSearchConfig, defaultAlbumConfig, toggleMediaType, toggleCategorisation, toggleOrder, toggleStatus, toggleSearchContext)
import Helpers exposing (isSupportedSearchLetter)
import ViewAlbums exposing (AlbumSearch, getAlbumSearchWithHeight)

-- Import types from Main module - we need to define these here or reference them properly
-- For now, I'll add the minimal types needed

-- Action type that represents what the menu wants to do
type MenuAction
    = ChangeMode UserMode
    | LoadAssets AssetSource
    | UpdateSearchInput Bool
    | NoMenuAction

-- Simplified types needed for menu handling
type UserMode
    = MainMenu
    | TimelineView TimelineConfig
    | SearchView SearchConfig
    | AlbumView ImmichAlbum AlbumConfig
    | AlbumBrowse AlbumSearch
    | Settings

type AssetSource
    = ImageSearch ImageSearchConfig
    | TextSearch String
    | FilteredAlbum ImmichAlbum AlbumConfig

-- Message type for menu-related actions
type MenuMsg
    = MainMenuKeyPress String
    | TimelineKeyPress String TimelineConfig
    | SearchKeyPress String SearchConfig
    | AlbumViewKeyPress String ImmichAlbum AlbumConfig
    | SettingsKeyPress String

-- Main Menu keyboard handling
handleMainMenuInput : String -> Dict ImmichAlbumId ImmichAlbum -> Int -> MenuAction
handleMainMenuInput key knownAlbums screenHeight =
    case key of
        "t" ->
            ChangeMode (TimelineView defaultTimelineConfig)
        "s" ->
            ChangeMode (SearchView defaultSearchConfig)
        "a" ->
            ChangeMode (AlbumBrowse <| getAlbumSearchWithHeight "" knownAlbums screenHeight)
        "g" ->
            ChangeMode Settings
        _ ->
            NoMenuAction

-- Timeline View keyboard handling
handleTimelineViewInput : String -> TimelineConfig -> MenuAction
handleTimelineViewInput key config =
    case key of
        "Escape" ->
            ChangeMode MainMenu
        "m" ->
            ChangeMode (TimelineView { config | mediaType = toggleMediaType config.mediaType })
        "c" ->
            ChangeMode (TimelineView { config | categorisation = toggleCategorisation config.categorisation })
        "o" ->
            ChangeMode (TimelineView { config | order = toggleOrder config.order })
        "s" ->
            ChangeMode (TimelineView { config | status = toggleStatus config.status })
        "Enter" ->
            let
                searchConfig =
                    { order = config.order, categorisation = config.categorisation, mediaType = config.mediaType, status = config.status }
            in
            LoadAssets (ImageSearch searchConfig)
        " " ->
            -- Space key
            let
                searchConfig =
                    { order = config.order, categorisation = config.categorisation, mediaType = config.mediaType, status = config.status }
            in
            LoadAssets (ImageSearch searchConfig)
        _ ->
            NoMenuAction

-- Search View keyboard handling  
handleSearchViewInput : String -> SearchConfig -> MenuAction
handleSearchViewInput key config =
    case key of
        "Escape" ->
            if config.inputFocused then
                ChangeMode (SearchView { config | inputFocused = False })
            else
                ChangeMode MainMenu
        "i" ->
            if not config.inputFocused then
                UpdateSearchInput True
            else
                NoMenuAction
        "m" ->
            if config.inputFocused then
                ChangeMode (SearchView { config | query = config.query ++ key })
            else
                ChangeMode (SearchView { config | mediaType = toggleMediaType config.mediaType })
        "c" ->
            if config.inputFocused then
                ChangeMode (SearchView { config | query = config.query ++ key })
            else
                ChangeMode (SearchView { config | searchContext = toggleSearchContext config.searchContext })
        "s" ->
            if config.inputFocused then
                ChangeMode (SearchView { config | query = config.query ++ key })
            else
                ChangeMode (SearchView { config | status = toggleStatus config.status })
        "Enter" ->
            if String.isEmpty config.query then
                NoMenuAction
            else
                LoadAssets (TextSearch config.query)
        " " ->
            -- Space key
            if String.isEmpty config.query then
                NoMenuAction
            else
                LoadAssets (TextSearch config.query)
        _ ->
            if config.inputFocused then
                if key == "Backspace" then
                    ChangeMode (SearchView { config | query = String.slice 0 (String.length config.query - 1) config.query })
                else if isSupportedSearchLetter key then
                    ChangeMode (SearchView { config | query = config.query ++ key })
                else
                    NoMenuAction
            else
                NoMenuAction

-- Album View keyboard handling
handleAlbumViewInput : String -> ImmichAlbum -> AlbumConfig -> Dict ImmichAlbumId ImmichAlbum -> Int -> MenuAction
handleAlbumViewInput key album config knownAlbums screenHeight =
    case key of
        "Escape" ->
            ChangeMode (AlbumBrowse <| getAlbumSearchWithHeight "" knownAlbums screenHeight)
        "m" ->
            ChangeMode (AlbumView album { config | mediaType = toggleMediaType config.mediaType })
        "o" ->
            ChangeMode (AlbumView album { config | order = toggleOrder config.order })
        "s" ->
            ChangeMode (AlbumView album { config | status = toggleStatus config.status })
        "Enter" ->
            LoadAssets (FilteredAlbum album config)
        " " ->
            -- Space key
            LoadAssets (FilteredAlbum album config)
        _ ->
            NoMenuAction

-- Settings keyboard handling
handleSettingsInput : String -> MenuAction
handleSettingsInput key =
    case key of
        "Escape" ->
            ChangeMode MainMenu
        _ ->
            NoMenuAction


-- Update function that processes MenuMsg and returns an action
-- This consolidates the menu input handling logic from Main.elm
updateMenus : MenuMsg -> Dict ImmichAlbumId ImmichAlbum -> ImmichApiPaths -> Int -> MenuAction
updateMenus menuMsg knownAlbums apiPaths screenHeight =
    case menuMsg of
        MainMenuKeyPress key ->
            handleMainMenuInput key knownAlbums screenHeight
        TimelineKeyPress key config ->
            handleTimelineViewInput key config
        SearchKeyPress key config ->
            handleSearchViewInput key config
        AlbumViewKeyPress key album config ->
            handleAlbumViewInput key album config knownAlbums screenHeight
        SettingsKeyPress key ->
            handleSettingsInput key