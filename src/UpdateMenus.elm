module UpdateMenus exposing
    ( MenuMsg(..)
    , MenuResult(..)
    , MenuState(..)
    , updateMenus
    )

import AssetSourceTypes exposing (AssetSource(..))
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



-- Main Menu keyboard handling
-- Timeline View keyboard handling
-- Search View keyboard handling
-- Album View keyboard handling
-- Settings keyboard handling
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



-- Handler functions that return menu results


handleMainMenuKeyPress : String -> Dict ImmichAlbumId ImmichAlbum -> Int -> MenuResult msg
handleMainMenuKeyPress key knownAlbums screenHeight =
    case key of
        "t" ->
            StayInMenu (TimelineView defaultTimelineConfig)

        " " ->
            StayInMenu (TimelineView defaultTimelineConfig)

        "Enter" ->
            StayInMenu (TimelineView defaultTimelineConfig)

        "s" ->
            StayInMenu (SearchView defaultSearchConfig)

        "a" ->
            StayInMenu (AlbumBrowse <| getAlbumSearchWithHeight "" knownAlbums screenHeight)

        "g" ->
            StayInMenu Settings

        "r" ->
            MenuReloadAlbums

        _ ->
            StayInMenu MainMenuHome


handleTimelineViewKeyPress : String -> TimelineConfig -> MenuResult msg
handleTimelineViewKeyPress key config =
    case key of
        "Escape" ->
            StayInMenu MainMenuHome

        "m" ->
            StayInMenu (TimelineView { config | mediaType = toggleMediaType config.mediaType })

        "c" ->
            StayInMenu (TimelineView { config | categorisation = toggleCategorisation config.categorisation })

        "o" ->
            let
                newOrder =
                    toggleOrderHandler config.mediaType config.order
            in
            StayInMenu (TimelineView { config | order = newOrder })

        "s" ->
            StayInMenu (TimelineView { config | status = toggleStatus config.status })

        "Enter" ->
            let
                searchConfig =
                    { order = config.order, categorisation = config.categorisation, mediaType = config.mediaType, status = config.status }
            in
            MenuLoadAssets (ImageSearch searchConfig)

        " " ->
            -- Space key
            let
                searchConfig =
                    { order = config.order, categorisation = config.categorisation, mediaType = config.mediaType, status = config.status }
            in
            MenuLoadAssets (ImageSearch searchConfig)

        _ ->
            StayInMenu (TimelineView config)


handleSearchViewKeyPress : String -> SearchConfig -> MenuResult msg
handleSearchViewKeyPress key config =
    case key of
        "Escape" ->
            if config.inputFocused then
                StayInMenu (SearchView { config | inputFocused = False })

            else
                StayInMenu MainMenuHome

        "i" ->
            if config.inputFocused then
                StayInMenu (SearchView { config | query = config.query ++ key })

            else
                MenuUpdateSearchInput True

        "m" ->
            if config.inputFocused then
                StayInMenu (SearchView { config | query = config.query ++ key })

            else
                StayInMenu (SearchView { config | mediaType = toggleMediaType config.mediaType })

        "c" ->
            if config.inputFocused then
                StayInMenu (SearchView { config | query = config.query ++ key })

            else
                StayInMenu (SearchView { config | searchContext = toggleSearchContext config.searchContext })

        "s" ->
            if config.inputFocused then
                StayInMenu (SearchView { config | query = config.query ++ key })

            else
                StayInMenu (SearchView { config | status = toggleStatus config.status })

        "Enter" ->
            if String.isEmpty config.query then
                StayInMenu (SearchView config)

            else
                MenuLoadAssets (TextSearch config.query config.searchContext)

        " " ->
            -- Space key
            if String.isEmpty config.query then
                StayInMenu (SearchView config)

            else
                MenuLoadAssets (TextSearch config.query config.searchContext)

        _ ->
            if config.inputFocused then
                if key == "Backspace" then
                    StayInMenu (SearchView { config | query = String.slice 0 (String.length config.query - 1) config.query })

                else if isSupportedSearchLetter key then
                    StayInMenu (SearchView { config | query = config.query ++ key })

                else
                    StayInMenu (SearchView config)

            else
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

        UpdateAlbums.NoAlbumResult ->
            StayInMenu (AlbumBrowse search)


handleAlbumViewKeyPress : String -> ImmichAlbum -> AlbumConfig -> Dict ImmichAlbumId ImmichAlbum -> Int -> MenuResult msg
handleAlbumViewKeyPress key album config knownAlbums screenHeight =
    case key of
        "Escape" ->
            StayInMenu (AlbumBrowse <| getAlbumSearchWithHeight "" knownAlbums screenHeight)

        "m" ->
            StayInMenu (AlbumView album { config | mediaType = toggleMediaType config.mediaType })

        "o" ->
            StayInMenu (AlbumView album { config | order = toggleOrderHandler config.mediaType config.order })

        "s" ->
            StayInMenu (AlbumView album { config | status = toggleStatus config.status })

        "x" ->
            StayInMenu (AlbumView album { config | moveFromMode = not config.moveFromMode })

        "Enter" ->
            MenuLoadAssets (FilteredAlbum album config)

        " " ->
            -- Space key
            MenuLoadAssets (FilteredAlbum album config)

        _ ->
            StayInMenu (AlbumView album config)


handleSettingsKeyPress : String -> MenuResult msg
handleSettingsKeyPress key =
    case key of
        "Escape" ->
            StayInMenu MainMenuHome

        _ ->
            StayInMenu Settings
