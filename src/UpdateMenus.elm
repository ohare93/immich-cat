module UpdateMenus exposing
    ( AssetSource(..)
    , LegacyUserMode(..)
    , MenuAction(..)
    , MenuMsg(..)
    , MenuResult(..)
    , MenuState(..)
    , updateMenus
    )

import Dict exposing (Dict)
import Helpers exposing (isSupportedSearchLetter, toggleCategorisation, toggleMediaType, toggleOrder, toggleOrderHandler, toggleStatus)
import Immich exposing (ImageSearchConfig, ImmichAlbum, ImmichAlbumId, ImmichApiPaths, SearchContext(..))
import KeybindBranches exposing (generateAlbumKeybindings)
import Menus exposing (AlbumConfig, SearchConfig, TimelineConfig, defaultAlbumConfig, defaultSearchConfig, defaultTimelineConfig, toggleSearchContext)
import UpdateAlbums
import ViewAlbums exposing (AlbumSearch, createAlbumSearchWithWarning, getAlbumSearchWithHeight)



-- Define the menu state type that encapsulates all menu modes


type MenuState
    = MainMenuHome
    | TimelineView TimelineConfig
    | SearchView SearchConfig
    | AlbumBrowse AlbumSearch
    | AlbumView ImmichAlbum AlbumConfig
    | Settings



-- Simplified message type for menu


type MenuMsg
    = MenuKeyPress String



-- Result type that communicates what the menu wants to do


type MenuResult msg
    = StayInMenu MenuState
    | MenuLoadAssets AssetSource
    | MenuUpdateSearchInput Bool
    | MenuReloadAlbums



-- Action type that represents what the menu wants to do (legacy)


type MenuAction
    = ChangeMode LegacyUserMode
    | LoadAssets AssetSource
    | UpdateSearchInput Bool
    | ReloadAlbums
    | NoMenuAction



-- Legacy types needed for backward compatibility


type LegacyUserMode
    = LegacyMainMenu
    | LegacyTimelineView TimelineConfig
    | LegacySearchView SearchConfig
    | LegacyAlbumView ImmichAlbum AlbumConfig
    | LegacyAlbumBrowse AlbumSearch
    | LegacySettings


type AssetSource
    = ImageSearch ImageSearchConfig
    | TextSearch String SearchContext
    | FilteredAlbum ImmichAlbum AlbumConfig



-- Main Menu keyboard handling


handleMainMenuInput : String -> Dict ImmichAlbumId ImmichAlbum -> Int -> MenuAction
handleMainMenuInput key knownAlbums screenHeight =
    case key of
        "t" ->
            ChangeMode (LegacyTimelineView defaultTimelineConfig)

        " " ->
            ChangeMode (LegacyTimelineView defaultTimelineConfig)

        "Enter" ->
            ChangeMode (LegacyTimelineView defaultTimelineConfig)

        "s" ->
            ChangeMode (LegacySearchView defaultSearchConfig)

        "a" ->
            ChangeMode (LegacyAlbumBrowse <| getAlbumSearchWithHeight "" knownAlbums screenHeight)

        "g" ->
            ChangeMode LegacySettings

        "r" ->
            ReloadAlbums

        _ ->
            NoMenuAction



-- Timeline View keyboard handling


handleTimelineViewInput : String -> TimelineConfig -> MenuAction
handleTimelineViewInput key config =
    case key of
        "Escape" ->
            ChangeMode LegacyMainMenu

        "m" ->
            ChangeMode (LegacyTimelineView { config | mediaType = toggleMediaType config.mediaType })

        "c" ->
            ChangeMode (LegacyTimelineView { config | categorisation = toggleCategorisation config.categorisation })

        "o" ->
            let
                newOrder =
                    toggleOrderHandler config.mediaType config.order
            in
            ChangeMode (LegacyTimelineView { config | order = newOrder })

        "s" ->
            ChangeMode (LegacyTimelineView { config | status = toggleStatus config.status })

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
                ChangeMode (LegacySearchView { config | inputFocused = False })

            else
                ChangeMode LegacyMainMenu

        "i" ->
            if not config.inputFocused then
                UpdateSearchInput True

            else
                NoMenuAction

        "m" ->
            if config.inputFocused then
                ChangeMode (LegacySearchView { config | query = config.query ++ key })

            else
                ChangeMode (LegacySearchView { config | mediaType = toggleMediaType config.mediaType })

        "c" ->
            if config.inputFocused then
                ChangeMode (LegacySearchView { config | query = config.query ++ key })

            else
                ChangeMode (LegacySearchView { config | searchContext = toggleSearchContext config.searchContext })

        "s" ->
            if config.inputFocused then
                ChangeMode (LegacySearchView { config | query = config.query ++ key })

            else
                ChangeMode (LegacySearchView { config | status = toggleStatus config.status })

        "Enter" ->
            if String.isEmpty config.query then
                NoMenuAction

            else
                LoadAssets (TextSearch config.query config.searchContext)

        " " ->
            -- Space key
            if String.isEmpty config.query then
                NoMenuAction

            else
                LoadAssets (TextSearch config.query config.searchContext)

        _ ->
            if config.inputFocused then
                if key == "Backspace" then
                    ChangeMode (LegacySearchView { config | query = String.slice 0 (String.length config.query - 1) config.query })

                else if isSupportedSearchLetter key then
                    ChangeMode (LegacySearchView { config | query = config.query ++ key })

                else
                    NoMenuAction

            else
                NoMenuAction



-- Album View keyboard handling


handleAlbumViewInput : String -> ImmichAlbum -> AlbumConfig -> Dict ImmichAlbumId ImmichAlbum -> Int -> MenuAction
handleAlbumViewInput key album config knownAlbums screenHeight =
    case key of
        "Escape" ->
            ChangeMode (LegacyAlbumBrowse <| getAlbumSearchWithHeight "" knownAlbums screenHeight)

        "m" ->
            ChangeMode (LegacyAlbumView album { config | mediaType = toggleMediaType config.mediaType })

        "o" ->
            ChangeMode (LegacyAlbumView album { config | order = toggleOrderHandler config.mediaType config.order })

        "s" ->
            ChangeMode (LegacyAlbumView album { config | status = toggleStatus config.status })

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
            ChangeMode LegacyMainMenu

        _ ->
            NoMenuAction



-- Main update function that handles all menu logic internally
-- This function now takes a MenuState and returns a MenuResult


updateMenus : MenuMsg -> MenuState -> Dict ImmichAlbumId ImmichAlbum -> ImmichApiPaths -> Int -> MenuResult msg
updateMenus menuMsg menuState knownAlbums apiPaths screenHeight =
    case menuMsg of
        MenuKeyPress key ->
            case menuState of
                MainMenuHome ->
                    handleMainMenuKeyPress key knownAlbums screenHeight

                TimelineView config ->
                    handleTimelineViewKeyPress key config

                SearchView config ->
                    handleSearchViewKeyPress key config

                AlbumBrowse search ->
                    handleAlbumBrowseKeyPress key search knownAlbums screenHeight

                AlbumView album config ->
                    handleAlbumViewKeyPress key album config knownAlbums screenHeight

                Settings ->
                    handleSettingsKeyPress key



-- Helper functions that convert menu actions to menu results


handleMainMenuKeyPress : String -> Dict ImmichAlbumId ImmichAlbum -> Int -> MenuResult msg
handleMainMenuKeyPress key knownAlbums screenHeight =
    let
        action =
            handleMainMenuInput key knownAlbums screenHeight
    in
    case action of
        ChangeMode newMode ->
            StayInMenu (convertUserModeToMenuState newMode)

        LoadAssets assetSource ->
            MenuLoadAssets assetSource

        ReloadAlbums ->
            MenuReloadAlbums

        _ ->
            StayInMenu MainMenuHome


handleTimelineViewKeyPress : String -> TimelineConfig -> MenuResult msg
handleTimelineViewKeyPress key config =
    let
        action =
            handleTimelineViewInput key config
    in
    case action of
        ChangeMode newMode ->
            StayInMenu (convertUserModeToMenuState newMode)

        LoadAssets assetSource ->
            MenuLoadAssets assetSource

        _ ->
            StayInMenu (TimelineView config)


handleSearchViewKeyPress : String -> SearchConfig -> MenuResult msg
handleSearchViewKeyPress key config =
    let
        action =
            handleSearchViewInput key config
    in
    case action of
        ChangeMode newMode ->
            StayInMenu (convertUserModeToMenuState newMode)

        LoadAssets assetSource ->
            MenuLoadAssets assetSource

        UpdateSearchInput focused ->
            MenuUpdateSearchInput focused

        _ ->
            StayInMenu (SearchView config)


handleAlbumBrowseKeyPress : String -> AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> Int -> MenuResult msg
handleAlbumBrowseKeyPress key search knownAlbums screenHeight =
    let
        -- Generate keybindings for the album browse functionality
        albumKeybindings =
            generateAlbumKeybindings (Dict.values knownAlbums)

        -- Call the UpdateAlbums functionality
        action =
            UpdateAlbums.handleAlbumBrowseInput key search albumKeybindings knownAlbums
    in
    case action of
        UpdateAlbums.ChangeToMainMenu ->
            StayInMenu MainMenuHome

        UpdateAlbums.SelectAlbumForView album ->
            StayInMenu (AlbumView album defaultAlbumConfig)

        UpdateAlbums.UpdateAlbumSearch newSearch ->
            StayInMenu (AlbumBrowse newSearch)

        UpdateAlbums.InvalidKeybindingInput invalidInput clearedSearch ->
            -- Invalid keybinding input - show warning and stay in current state
            let
                searchWithWarning =
                    createAlbumSearchWithWarning clearedSearch invalidInput
            in
            StayInMenu (AlbumBrowse searchWithWarning)

        UpdateAlbums.NoAlbumAction ->
            StayInMenu (AlbumBrowse search)


handleAlbumViewKeyPress : String -> ImmichAlbum -> AlbumConfig -> Dict ImmichAlbumId ImmichAlbum -> Int -> MenuResult msg
handleAlbumViewKeyPress key album config knownAlbums screenHeight =
    let
        action =
            handleAlbumViewInput key album config knownAlbums screenHeight
    in
    case action of
        ChangeMode newMode ->
            StayInMenu (convertUserModeToMenuState newMode)

        LoadAssets assetSource ->
            MenuLoadAssets assetSource

        _ ->
            StayInMenu (AlbumView album config)


handleSettingsKeyPress : String -> MenuResult msg
handleSettingsKeyPress key =
    let
        action =
            handleSettingsInput key
    in
    case action of
        ChangeMode newMode ->
            StayInMenu (convertUserModeToMenuState newMode)

        _ ->
            StayInMenu Settings



-- Helper to convert legacy UserMode to MenuState


convertUserModeToMenuState : LegacyUserMode -> MenuState
convertUserModeToMenuState userMode =
    case userMode of
        LegacyMainMenu ->
            MainMenuHome

        LegacyTimelineView config ->
            TimelineView config

        LegacySearchView config ->
            SearchView config

        LegacyAlbumView album config ->
            AlbumView album config

        LegacyAlbumBrowse search ->
            AlbumBrowse search

        LegacySettings ->
            Settings
