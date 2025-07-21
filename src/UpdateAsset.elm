module UpdateAsset exposing
    ( AssetAction(..)
    , AssetMsg(..)
    , AssetResult(..)
    , AssetState(..)
    , handleEditAssetInput
    , updateAsset
    )

import Dict exposing (Dict)
import Helpers exposing (isKeybindingLetter, isSupportedSearchLetter, loopImageIndexOverArray)
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichAsset, ImmichAssetId)
import KeybindingValidation exposing (KeybindingValidationResult(..), validateKeybindingInput)
import ViewAlbums exposing (AlbumSearch, AssetWithActions, InputMode(..), clearAlbumSearchWarning, getAlbumByExactKeybinding, getSelectedAlbumForAsset, halfPageDown, halfPageUp, moveSelectionDownForAsset, moveSelectionUpForAsset, pageDown, pageUp, resetPagination, updateAlbumSearchString)
import ViewGrid exposing (GridMsg, GridState)



-- Define the asset state type that encapsulates all asset viewing/editing modes


type AssetState
    = EditAsset InputMode AssetWithActions AlbumSearch
    | CreateAlbumConfirmation InputMode AssetWithActions AlbumSearch String
    | ShowEditAssetHelp InputMode AssetWithActions AlbumSearch
    | SearchAssetInput String
    | SelectAlbumInput AlbumSearch
    | GridView GridState



-- Simplified message type for asset


type AssetMsg
    = AssetKeyPress String
    | AssetGridMsg GridMsg



-- Result type that communicates what the asset module wants to do


type AssetResult msg
    = StayInAssets AssetState
    | GoToMainMenu
    | GoToSearchView String
    | AssetLoadTextSearch String
    | AssetLoadAlbum ImmichAlbum
    | AssetSwitchToAssetIndex Int
    | AssetToggleFavorite
    | AssetToggleArchived
    | AssetToggleAlbumMembership ImmichAlbum
    | AssetOpenInImmich
    | AssetCreateAlbum String
    | AssetToggleTimeView
    | AssetSwitchToGridView
    | AssetSwitchToDetailView ImmichAssetId
    | AssetGridUpdate GridState
    | AssetBulkFavorite (List ImmichAssetId) Bool
    | AssetBulkArchive (List ImmichAssetId) Bool
    | AssetBulkAddToAlbum (List ImmichAssetId) ImmichAlbumId
    | AssetBulkRemoveFromAlbum (List ImmichAssetId) ImmichAlbumId
    | AssetRequestLoadMore



-- Action type for asset editing


type AssetAction
    = ChangeAssetToMainMenu
    | ChangeToSearchView String
    | ChangeToLoadingTextSearch String
    | ChangeToLoadingAlbum ImmichAlbum
    | ChangeInputMode InputMode
    | ChangeImageIndex Int
    | UpdateAsset AssetWithActions
    | UpdateAssetSearch AlbumSearch
    | ToggleFavorite
    | ToggleArchived
    | ToggleAlbumMembership ImmichAlbum
    | OpenInImmich
    | ShowAssetHelp
    | CreateAlbumWithName String
    | LoadAlbum ImmichAlbum
    | SwitchToAssetIndex Int
    | ToggleTimeView
    | SwitchToGridView
    | SwitchToDetailView ImmichAssetId
    | UpdateGridState GridState
    | InvalidKeybindingInput String AlbumSearch
    | ExitToNormalMode
    | ToggleVideoLoaded
    | ToggleScrollView
    | ScrollImage Int Int
    | NoAssetAction



-- Handle EditAsset keyboard input


handleEditAssetInput : String -> InputMode -> AssetWithActions -> AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> Int -> List ImmichAssetId -> AssetAction
handleEditAssetInput key inputMode asset search albumKeybindings knownAlbums screenHeight currentAssets =
    if inputMode == InsertMode then
        handleInsertModeInput key inputMode asset search albumKeybindings knownAlbums

    else if inputMode == KeybindingMode then
        handleKeybindingModeInput key inputMode asset search albumKeybindings knownAlbums

    else
        case inputMode of
            ScrollViewMode scrollState ->
                handleScrollViewModeInput key scrollState

            _ ->
                -- Normal mode: check for normal mode commands first, then keybinding mode
                case key of
                    "I" ->
                        ChangeInputMode InsertMode

                    "F" ->
                        ToggleFavorite

                    "D" ->
                        ToggleArchived

                    "K" ->
                        OpenInImmich

                    "T" ->
                        ToggleTimeView

                    "G" ->
                        SwitchToGridView

                    "L" ->
                        ToggleVideoLoaded

                    "?" ->
                        ShowAssetHelp

                    _ ->
                        if isKeybindingLetter key then
                            -- Start keybinding mode with this lowercase letter
                            handleStartKeybindingMode key asset search albumKeybindings knownAlbums

                        else
                            -- Handle other navigation keys (arrows, space, etc.)
                            handleNormalModeInput key inputMode asset search screenHeight currentAssets



-- Handle Insert Mode (album selection) input


handleInsertModeInput : String -> InputMode -> AssetWithActions -> AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AssetAction
handleInsertModeInput key inputMode asset search albumKeybindings knownAlbums =
    case key of
        "Escape" ->
            ExitToNormalMode

        "Enter" ->
            applySelectedAlbum inputMode asset search albumKeybindings knownAlbums

        "ArrowUp" ->
            UpdateAssetSearch (moveSelectionUpForAsset search albumKeybindings knownAlbums asset)

        "ArrowDown" ->
            UpdateAssetSearch (moveSelectionDownForAsset search albumKeybindings knownAlbums asset)

        "PageUp" ->
            UpdateAssetSearch { search | pagination = pageUp search.pagination }

        "PageDown" ->
            UpdateAssetSearch { search | pagination = pageDown search.pagination }

        "?" ->
            ShowAssetHelp

        _ ->
            if isSupportedSearchLetter key then
                -- Add letter to search string
                let
                    newSearchString =
                        search.searchString ++ key

                    updatedSearch =
                        updateAlbumSearchString newSearchString search knownAlbums
                in
                UpdateAssetSearch updatedSearch

            else if key == "Backspace" then
                -- Remove last character from search string
                let
                    newSearchString =
                        String.slice 0 (String.length search.searchString - 1) search.searchString

                    updatedSearch =
                        updateAlbumSearchString newSearchString search knownAlbums
                in
                UpdateAssetSearch updatedSearch

            else if String.contains "Control" key then
                case String.replace "Control+" "" key of
                    "d" ->
                        UpdateAssetSearch { search | pagination = halfPageDown search.pagination }

                    "u" ->
                        UpdateAssetSearch { search | pagination = halfPageUp search.pagination }

                    "f" ->
                        UpdateAssetSearch { search | pagination = pageDown search.pagination }

                    "b" ->
                        UpdateAssetSearch { search | pagination = pageUp search.pagination }

                    _ ->
                        NoAssetAction

            else
                NoAssetAction



-- Handle Keybinding Mode input


handleKeybindingModeInput : String -> InputMode -> AssetWithActions -> AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AssetAction
handleKeybindingModeInput key inputMode asset search albumKeybindings knownAlbums =
    if isKeybindingLetter key then
        let
            -- Clear any existing warning when typing ANY keybinding letter
            searchWithoutWarning =
                clearAlbumSearchWarning search
        in
        case validateKeybindingInput search.partialKeybinding key albumKeybindings knownAlbums of
            ExactMatch album ->
                ToggleAlbumMembership album

            ValidKeybinding newPartialKeybinding ->
                let
                    updatedSearch =
                        { searchWithoutWarning | partialKeybinding = newPartialKeybinding, pagination = resetPagination searchWithoutWarning.pagination }
                in
                UpdateAssetSearch updatedSearch

            InvalidCharacter invalidChar ->
                -- Check if we have keybindings at all - if not, don't start keybinding mode
                if Dict.isEmpty albumKeybindings then
                    NoAssetAction

                else
                    InvalidKeybindingInput invalidChar searchWithoutWarning

    else
        case key of
            "Escape" ->
                ExitToNormalMode

            "Backspace" ->
                let
                    newPartialKeybinding =
                        String.slice 0 (String.length search.partialKeybinding - 1) search.partialKeybinding

                    updatedSearch =
                        { search | partialKeybinding = newPartialKeybinding }
                in
                if newPartialKeybinding == "" then
                    -- Clear the partialKeybinding when returning to NormalMode
                    let
                        clearedSearch =
                            { search | partialKeybinding = "" }
                    in
                    UpdateAssetSearch clearedSearch

                else
                    UpdateAssetSearch updatedSearch

            "Enter" ->
                applySelectedAlbum inputMode asset search albumKeybindings knownAlbums

            "PageUp" ->
                UpdateAssetSearch { search | pagination = pageUp search.pagination }

            "PageDown" ->
                UpdateAssetSearch { search | pagination = pageDown search.pagination }

            "?" ->
                ShowAssetHelp

            _ ->
                if String.contains "Control" key then
                    case String.replace "Control+" "" key of
                        "d" ->
                            UpdateAssetSearch { search | pagination = halfPageDown search.pagination }

                        "u" ->
                            UpdateAssetSearch { search | pagination = halfPageUp search.pagination }

                        "f" ->
                            UpdateAssetSearch { search | pagination = pageDown search.pagination }

                        "b" ->
                            UpdateAssetSearch { search | pagination = pageUp search.pagination }

                        _ ->
                            NoAssetAction

                else
                    NoAssetAction



-- Handle starting keybinding mode with a key


handleStartKeybindingMode : String -> AssetWithActions -> AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AssetAction
handleStartKeybindingMode partialKey asset search albumKeybindings knownAlbums =
    let
        -- Clear any existing warning when typing ANY keybinding letter
        searchWithoutWarning =
            clearAlbumSearchWarning search
    in
    case validateKeybindingInput "" partialKey albumKeybindings knownAlbums of
        ExactMatch album ->
            ToggleAlbumMembership album

        ValidKeybinding newPartialKeybinding ->
            let
                updatedSearch =
                    { searchWithoutWarning | partialKeybinding = newPartialKeybinding, pagination = resetPagination searchWithoutWarning.pagination }
            in
            UpdateAssetSearch updatedSearch

        InvalidCharacter invalidChar ->
            -- Check if we have keybindings at all - if not, don't start keybinding mode
            if Dict.isEmpty albumKeybindings then
                NoAssetAction

            else
                InvalidKeybindingInput invalidChar searchWithoutWarning



-- Handle Normal Mode input


handleNormalModeInput : String -> InputMode -> AssetWithActions -> AlbumSearch -> Int -> List ImmichAssetId -> AssetAction
handleNormalModeInput key inputMode asset search screenHeight currentAssets =
    case key of
        "ArrowLeft" ->
            ChangeImageIndex -1

        "ArrowRight" ->
            ChangeImageIndex 1

        " " ->
            ChangeImageIndex 1

        "Enter" ->
            ChangeImageIndex 1

        "Escape" ->
            ChangeAssetToMainMenu

        "PageUp" ->
            UpdateAssetSearch { search | pagination = pageUp search.pagination }

        "PageDown" ->
            UpdateAssetSearch { search | pagination = pageDown search.pagination }

        "S" ->
            ToggleScrollView

        _ ->
            if String.contains "Control" key then
                case String.replace "Control+" "" key of
                    "d" ->
                        UpdateAssetSearch { search | pagination = halfPageDown search.pagination }

                    "u" ->
                        UpdateAssetSearch { search | pagination = halfPageUp search.pagination }

                    "f" ->
                        UpdateAssetSearch { search | pagination = pageDown search.pagination }

                    "b" ->
                        UpdateAssetSearch { search | pagination = pageUp search.pagination }

                    _ ->
                        NoAssetAction

            else
                NoAssetAction



-- Handle Scroll View Mode input for hjkl navigation


handleScrollViewModeInput : String -> { scrollX : Int, scrollY : Int } -> AssetAction
handleScrollViewModeInput key scrollState =
    let
        scrollStep =
            50

        -- pixels to scroll per key press
    in
    case key of
        "j" ->
            ScrollImage 0 -scrollStep

        -- Scroll image up to see more below
        "k" ->
            ScrollImage 0 scrollStep

        -- Scroll image down to see more above
        "h" ->
            ScrollImage scrollStep 0

        -- Scroll image right to see more to the left
        "l" ->
            ScrollImage -scrollStep 0

        -- Scroll image left to see more to the right
        "Escape" ->
            ChangeInputMode NormalMode

        "S" ->
            ChangeInputMode NormalMode

        _ ->
            NoAssetAction



-- Helper function to apply selected album


applySelectedAlbum : InputMode -> AssetWithActions -> AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AssetAction
applySelectedAlbum inputMode asset search albumKeybindings knownAlbums =
    let
        maybeMatch =
            if inputMode == KeybindingMode then
                getAlbumByExactKeybinding search.partialKeybinding albumKeybindings knownAlbums

            else
                getSelectedAlbumForAsset search albumKeybindings knownAlbums asset
    in
    case maybeMatch of
        Just album ->
            ToggleAlbumMembership album

        Nothing ->
            if inputMode == KeybindingMode then
                NoAssetAction

            else if String.trim search.searchString /= "" then
                CreateAlbumWithName (String.trim search.searchString)

            else
                NoAssetAction



-- Handle SearchAssetInput


handleSearchAssetInput : String -> String -> AssetAction
handleSearchAssetInput key searchString =
    if isSupportedSearchLetter key then
        NoAssetAction
        -- Will be handled by caller to update search string

    else
        case key of
            "Backspace" ->
                NoAssetAction

            -- Will be handled by caller to update search string
            "Escape" ->
                ChangeToSearchView searchString

            "Enter" ->
                ChangeToLoadingTextSearch searchString

            _ ->
                NoAssetAction



-- Handle SelectAlbumInput


handleSelectAlbumInput : String -> AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AssetAction
handleSelectAlbumInput key searchResults albumKeybindings knownAlbums =
    if isSupportedSearchLetter key then
        -- Handle keybinding addition - will be handled by caller
        NoAssetAction

    else
        case key of
            "Backspace" ->
                NoAssetAction

            -- Will be handled by caller
            "Escape" ->
                ChangeAssetToMainMenu

            "Enter" ->
                -- Select album if matching
                let
                    maybeMatch =
                        getTopMatchFromSearch searchResults albumKeybindings knownAlbums
                in
                case maybeMatch of
                    Just album ->
                        LoadAlbum album

                    Nothing ->
                        NoAssetAction

            "ArrowUp" ->
                UpdateAssetSearch (moveSelectionUpForSearch searchResults albumKeybindings knownAlbums)

            "ArrowDown" ->
                UpdateAssetSearch (moveSelectionDownForSearch searchResults albumKeybindings knownAlbums)

            "PageUp" ->
                UpdateAssetSearch { searchResults | pagination = pageUp searchResults.pagination }

            "PageDown" ->
                UpdateAssetSearch { searchResults | pagination = pageDown searchResults.pagination }

            _ ->
                if String.contains "Control" key then
                    case String.replace "Control+" "" key of
                        "d" ->
                            UpdateAssetSearch { searchResults | pagination = halfPageDown searchResults.pagination }

                        "u" ->
                            UpdateAssetSearch { searchResults | pagination = halfPageUp searchResults.pagination }

                        "f" ->
                            UpdateAssetSearch { searchResults | pagination = pageDown searchResults.pagination }

                        "b" ->
                            UpdateAssetSearch { searchResults | pagination = pageUp searchResults.pagination }

                        _ ->
                            NoAssetAction

                else
                    NoAssetAction



-- Handle CreateAlbumConfirmation
-- Handle ShowEditAssetHelp


handleShowEditAssetHelpInput : String -> AssetAction
handleShowEditAssetHelpInput key =
    case key of
        "Escape" ->
            NoAssetAction

        -- Will return to EditAsset mode in caller
        "?" ->
            NoAssetAction

        -- Will return to EditAsset mode in caller
        _ ->
            NoAssetAction



-- Helper functions that need to be implemented or imported


getTopMatchFromSearch : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> Maybe ImmichAlbum
getTopMatchFromSearch search albumKeybindings albums =
    let
        matchesDict =
            ViewAlbums.filterToOnlySearchedForAlbums search albumKeybindings albums
    in
    if Dict.isEmpty matchesDict then
        Nothing

    else
        matchesDict
            |> Dict.toList
            |> List.map (\( id, album ) -> ( Dict.get id search.albumScores |> Maybe.withDefault 0, album ))
            |> List.sortBy (\( score, _ ) -> -score)
            |> List.head
            |> Maybe.map (\( _, album ) -> album)


moveSelectionUpForSearch : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AlbumSearch
moveSelectionUpForSearch search albumKeybindings albums =
    { search | selectedIndex = max 0 (search.selectedIndex - 1) }


moveSelectionDownForSearch : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AlbumSearch
moveSelectionDownForSearch search albumKeybindings albums =
    let
        filteredCount =
            List.length (ViewAlbums.getFilteredAlbumsList search albumKeybindings albums)

        maxIndex =
            max 0 (filteredCount - 1)
    in
    { search | selectedIndex = min maxIndex (search.selectedIndex + 1) }



-- Main update function that handles all asset logic internally
-- This function now takes an AssetState and returns an AssetResult


updateAsset : AssetMsg -> AssetState -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> Int -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> AssetResult msg
updateAsset assetMsg assetState albumKeybindings knownAlbums screenHeight currentAssets knownAssets =
    case assetMsg of
        AssetKeyPress key ->
            case assetState of
                EditAsset inputMode asset search ->
                    handleEditAssetKeyPress key inputMode asset search albumKeybindings knownAlbums screenHeight currentAssets

                SearchAssetInput searchString ->
                    handleSearchAssetKeyPress key searchString

                SelectAlbumInput searchResults ->
                    handleSelectAlbumKeyPress key searchResults albumKeybindings knownAlbums

                CreateAlbumConfirmation inputMode asset search albumName ->
                    handleCreateAlbumConfirmationKeyPress key inputMode asset search albumName

                ShowEditAssetHelp inputMode asset search ->
                    handleShowEditAssetHelpKeyPress key inputMode asset search

                GridView gridState ->
                    handleGridViewKeyPress key gridState currentAssets knownAssets

        AssetGridMsg gridMsg ->
            case assetState of
                GridView gridState ->
                    handleGridViewMessage gridMsg gridState currentAssets knownAssets

                _ ->
                    StayInAssets assetState



-- Helper functions that convert asset actions to asset results


handleEditAssetKeyPress : String -> InputMode -> AssetWithActions -> AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> Int -> List ImmichAssetId -> AssetResult msg
handleEditAssetKeyPress key inputMode asset search albumKeybindings knownAlbums screenHeight currentAssets =
    let
        action =
            handleEditAssetInput key inputMode asset search albumKeybindings knownAlbums screenHeight currentAssets
    in
    convertAssetActionToResult action inputMode asset search currentAssets


handleSearchAssetKeyPress : String -> String -> AssetResult msg
handleSearchAssetKeyPress key searchString =
    let
        action =
            handleSearchAssetInput key searchString
    in
    case action of
        ChangeAssetToMainMenu ->
            GoToMainMenu

        ChangeToSearchView query ->
            GoToSearchView query

        ChangeToLoadingTextSearch query ->
            AssetLoadTextSearch query

        _ ->
            StayInAssets (SearchAssetInput searchString)


handleSelectAlbumKeyPress : String -> AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AssetResult msg
handleSelectAlbumKeyPress key searchResults albumKeybindings knownAlbums =
    let
        action =
            handleSelectAlbumInput key searchResults albumKeybindings knownAlbums
    in
    case action of
        ChangeAssetToMainMenu ->
            GoToMainMenu

        LoadAlbum album ->
            AssetLoadAlbum album

        UpdateAssetSearch newSearch ->
            StayInAssets (SelectAlbumInput newSearch)

        _ ->
            StayInAssets (SelectAlbumInput searchResults)


handleCreateAlbumConfirmationKeyPress : String -> InputMode -> AssetWithActions -> AlbumSearch -> String -> AssetResult msg
handleCreateAlbumConfirmationKeyPress key inputMode asset search albumName =
    case key of
        "Enter" ->
            AssetCreateAlbum albumName

        "Escape" ->
            StayInAssets (EditAsset inputMode asset search)

        _ ->
            StayInAssets (CreateAlbumConfirmation inputMode asset search albumName)


handleShowEditAssetHelpKeyPress : String -> InputMode -> AssetWithActions -> AlbumSearch -> AssetResult msg
handleShowEditAssetHelpKeyPress key inputMode asset search =
    let
        action =
            handleShowEditAssetHelpInput key
    in
    case action of
        NoAssetAction ->
            if key == "Escape" || key == "?" then
                StayInAssets (EditAsset inputMode asset search)

            else
                StayInAssets (ShowEditAssetHelp inputMode asset search)

        _ ->
            StayInAssets (ShowEditAssetHelp inputMode asset search)



-- Helper to convert legacy AssetAction to AssetResult


convertAssetActionToResult : AssetAction -> InputMode -> AssetWithActions -> AlbumSearch -> List ImmichAssetId -> AssetResult msg
convertAssetActionToResult action inputMode asset search currentAssets =
    case action of
        ChangeAssetToMainMenu ->
            GoToMainMenu

        ChangeToSearchView query ->
            GoToSearchView query

        ChangeToLoadingTextSearch query ->
            AssetLoadTextSearch query

        ChangeToLoadingAlbum album ->
            AssetLoadAlbum album

        ChangeInputMode newInputMode ->
            StayInAssets (EditAsset newInputMode asset search)

        ChangeImageIndex indexChange ->
            let
                currentIndex =
                    currentAssets
                        |> List.indexedMap
                            (\index assetId ->
                                if assetId == asset.asset.id then
                                    Just index

                                else
                                    Nothing
                            )
                        |> List.filterMap identity
                        |> List.head
                        |> Maybe.withDefault 0

                newIndex =
                    loopImageIndexOverArray currentIndex indexChange (List.length currentAssets)
            in
            AssetSwitchToAssetIndex newIndex

        UpdateAsset newAsset ->
            StayInAssets (EditAsset inputMode newAsset search)

        UpdateAssetSearch newSearch ->
            -- Preserve the current InputMode unless there's a specific reason to change it
            let
                newInputMode =
                    case inputMode of
                        InsertMode ->
                            -- Stay in InsertMode unless explicitly changing modes
                            InsertMode

                        KeybindingMode ->
                            -- Stay in KeybindingMode unless partialKeybinding becomes empty
                            if String.isEmpty newSearch.partialKeybinding then
                                NormalMode

                            else
                                KeybindingMode

                        NormalMode ->
                            -- Switch to KeybindingMode only if partialKeybinding is populated
                            if String.isEmpty newSearch.partialKeybinding then
                                NormalMode

                            else
                                KeybindingMode

                        ScrollViewMode scrollState ->
                            -- Preserve scroll view mode when updating search
                            ScrollViewMode scrollState
            in
            StayInAssets (EditAsset newInputMode asset newSearch)

        ToggleFavorite ->
            AssetToggleFavorite

        ToggleArchived ->
            AssetToggleArchived

        ToggleVideoLoaded ->
            let
                updatedAsset =
                    { asset | isVideoLoaded = True }
            in
            StayInAssets (EditAsset inputMode updatedAsset search)

        ToggleAlbumMembership album ->
            AssetToggleAlbumMembership album

        OpenInImmich ->
            AssetOpenInImmich

        ShowAssetHelp ->
            StayInAssets (ShowEditAssetHelp inputMode asset search)

        CreateAlbumWithName albumName ->
            StayInAssets (CreateAlbumConfirmation inputMode asset search albumName)

        LoadAlbum album ->
            AssetLoadAlbum album

        SwitchToAssetIndex newIndex ->
            AssetSwitchToAssetIndex newIndex

        ToggleTimeView ->
            AssetToggleTimeView

        SwitchToGridView ->
            AssetSwitchToGridView

        SwitchToDetailView assetId ->
            AssetSwitchToDetailView assetId

        UpdateGridState gridState ->
            AssetGridUpdate gridState

        InvalidKeybindingInput invalidInput clearedSearch ->
            -- Invalid keybinding input - show warning and stay in current state
            let
                searchWithWarning =
                    ViewAlbums.createAlbumSearchWithWarning clearedSearch invalidInput
            in
            StayInAssets (EditAsset inputMode asset searchWithWarning)

        ExitToNormalMode ->
            -- Exit to normal mode and clear all search state
            let
                clearedSearch =
                    { search
                        | searchString = ""
                        , partialKeybinding = ""
                        , invalidInputWarning = Nothing
                        , pagination = ViewAlbums.resetPagination search.pagination
                    }
            in
            StayInAssets (EditAsset NormalMode asset clearedSearch)

        NoAssetAction ->
            StayInAssets (EditAsset inputMode asset search)

        ToggleScrollView ->
            case inputMode of
                NormalMode ->
                    StayInAssets (EditAsset (ScrollViewMode { scrollX = 0, scrollY = 0 }) asset search)

                ScrollViewMode _ ->
                    StayInAssets (EditAsset NormalMode asset search)

                _ ->
                    -- Keep existing mode if not Normal or ScrollView
                    StayInAssets (EditAsset inputMode asset search)

        ScrollImage deltaX deltaY ->
            case inputMode of
                ScrollViewMode scrollState ->
                    let
                        newScrollState =
                            { scrollX = scrollState.scrollX + deltaX
                            , scrollY = scrollState.scrollY + deltaY
                            }
                    in
                    StayInAssets (EditAsset (ScrollViewMode newScrollState) asset search)

                _ ->
                    -- Ignore scroll commands if not in scroll view mode
                    StayInAssets (EditAsset inputMode asset search)



-- Grid view handler functions


handleGridViewKeyPress : String -> GridState -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> AssetResult msg
handleGridViewKeyPress key gridState currentAssets knownAssets =
    let
        assets =
            List.filterMap (\id -> Dict.get id knownAssets) currentAssets
    in
    case key of
        "Escape" ->
            GoToMainMenu

        "Enter" ->
            case ViewGrid.getSelectedAsset gridState of
                Just selectedAssetId ->
                    AssetSwitchToDetailView selectedAssetId

                Nothing ->
                    StayInAssets (GridView gridState)

        "G" ->
            -- Toggle back to detail view (will be handled by parent)
            GoToMainMenu

        _ ->
            let
                updatedGridState =
                    ViewGrid.updateGridState (ViewGrid.GridKeyPressed key) gridState assets
            in
            StayInAssets (GridView updatedGridState)


handleGridViewMessage : GridMsg -> GridState -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> AssetResult msg
handleGridViewMessage gridMsg gridState currentAssets knownAssets =
    let
        assets =
            List.filterMap (\id -> Dict.get id knownAssets) currentAssets

        updatedGridState =
            ViewGrid.updateGridState gridMsg gridState assets

        selectedAssetIds =
            Dict.keys updatedGridState.selectedAssets
    in
    case gridMsg of
        ViewGrid.GridItemClicked assetId ->
            -- Single click switches to detail view
            AssetSwitchToDetailView assetId

        ViewGrid.GridBulkFavorite isFavorite ->
            if List.isEmpty selectedAssetIds then
                StayInAssets (GridView updatedGridState)

            else
                AssetBulkFavorite selectedAssetIds isFavorite

        ViewGrid.GridBulkArchive isArchived ->
            if List.isEmpty selectedAssetIds then
                StayInAssets (GridView updatedGridState)

            else
                AssetBulkArchive selectedAssetIds isArchived

        ViewGrid.GridBulkAddToAlbum ->
            if List.isEmpty selectedAssetIds then
                StayInAssets (GridView updatedGridState)

            else
                -- Switch to album selection for bulk add
                StayInAssets (SelectAlbumInput (ViewAlbums.getAlbumSearch "" Dict.empty))

        ViewGrid.GridBulkRemoveFromAlbum ->
            if List.isEmpty selectedAssetIds then
                StayInAssets (GridView updatedGridState)

            else
                -- Switch to album selection for bulk remove
                StayInAssets (SelectAlbumInput (ViewAlbums.getAlbumSearch "" Dict.empty))

        ViewGrid.GridScrolled _ ->
            -- Scroll events are handled by updateGridState
            StayInAssets (GridView updatedGridState)

        ViewGrid.GridRequestLoadMore ->
            -- Trigger load more request - this will be handled by Main.elm
            AssetRequestLoadMore

        _ ->
            StayInAssets (GridView updatedGridState)
