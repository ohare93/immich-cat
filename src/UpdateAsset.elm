module UpdateAsset exposing
    ( AssetAction(..)
    , handleCreateAlbumConfirmationInput
    , handleEditAssetInput
    , handleSearchAssetInput
    , handleSelectAlbumInput
    , handleShowEditAssetHelpInput
    )

import Dict exposing (Dict)
import Helpers exposing (isKeybindingLetter, isSupportedSearchLetter, loopImageIndexOverArray)
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichApiPaths, ImmichAsset, ImmichAssetId)
import ViewAlbums exposing (AlbumSearch, AssetWithActions, InputMode(..), PropertyChange(..), filterToOnlySearchedForAlbums, flipPropertyChange, getAlbumByExactKeybinding, getAlbumSearchWithHeight, getFilteredAlbumsList, getSelectedAlbumForAsset, halfPageDown, halfPageUp, isAddingToAlbum, isCurrentlyInAlbum, moveSelectionDownForAsset, moveSelectionUpForAsset, pageDown, pageUp, resetPagination, toggleAssetAlbum, updateAlbumSearchString)


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
    | NoAssetAction


-- Handle EditAsset keyboard input

handleEditAssetInput : String -> InputMode -> AssetWithActions -> AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> Int -> List ImmichAssetId -> AssetAction
handleEditAssetInput key inputMode asset search albumKeybindings knownAlbums screenHeight currentAssets =
    if inputMode == InsertMode then
        handleInsertModeInput key inputMode asset search albumKeybindings knownAlbums
    else if inputMode == KeybindingMode then
        handleKeybindingModeInput key inputMode asset search albumKeybindings knownAlbums
    else
        -- Normal mode: check for normal mode commands first, then keybinding mode
        case key of
            "I" -> ChangeInputMode InsertMode
            "F" -> ToggleFavorite  
            "D" -> ToggleArchived
            "K" -> OpenInImmich
            "?" -> ShowAssetHelp
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
            ChangeInputMode NormalMode
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
                    newSearchString = search.searchString ++ key
                    updatedSearch = updateAlbumSearchString newSearchString search knownAlbums
                in
                UpdateAssetSearch updatedSearch
            else if key == "Backspace" then
                -- Remove last character from search string
                let
                    newSearchString = String.slice 0 (String.length search.searchString - 1) search.searchString
                    updatedSearch = updateAlbumSearchString newSearchString search knownAlbums
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
            newPartialKeybinding =
                search.partialKeybinding ++ key
            updatedSearch =
                { search | partialKeybinding = newPartialKeybinding, pagination = resetPagination search.pagination }
            maybeExactMatch =
                getAlbumByExactKeybinding newPartialKeybinding albumKeybindings knownAlbums
        in
        case maybeExactMatch of
            Just album ->
                ToggleAlbumMembership album
            Nothing ->
                UpdateAssetSearch updatedSearch
    else
        case key of
            "Escape" ->
                ChangeInputMode NormalMode
            "Backspace" ->
                let
                    newPartialKeybinding =
                        String.slice 0 (String.length search.partialKeybinding - 1) search.partialKeybinding
                    updatedSearch =
                        { search | partialKeybinding = newPartialKeybinding }
                in
                if newPartialKeybinding == "" then
                    ChangeInputMode NormalMode
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
        updatedSearch =
            { search | partialKeybinding = partialKey, pagination = resetPagination search.pagination }
        maybeExactMatch =
            getAlbumByExactKeybinding partialKey albumKeybindings knownAlbums
    in
    case maybeExactMatch of
        Just album ->
            ToggleAlbumMembership album
        Nothing ->
            UpdateAssetSearch updatedSearch


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
        "Escape" ->
            ChangeAssetToMainMenu
        "PageUp" ->
            UpdateAssetSearch { search | pagination = pageUp search.pagination }
        "PageDown" ->
            UpdateAssetSearch { search | pagination = pageDown search.pagination }
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

handleCreateAlbumConfirmationInput : String -> AssetAction
handleCreateAlbumConfirmationInput key =
    case key of
        "Enter" ->
            NoAssetAction

        -- Will trigger album creation in caller
        "Escape" ->
            NoAssetAction

        -- Will return to EditAsset mode in caller
        _ ->
            NoAssetAction


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

