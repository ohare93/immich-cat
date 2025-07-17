module UpdateAlbums exposing
    ( handleAlbumBrowseInput
    , updateAlbums
    , AlbumAction(..)
    , AlbumMsg(..)
    )

import Dict exposing (Dict)
import Helpers exposing (isSupportedSearchLetter, isKeybindingLetter)
import Immich exposing (ImmichAlbum, ImmichAlbumId)
import KeybindingValidation exposing (validateKeybindingInput, KeybindingValidationResult(..), couldStartKeybinding)
import Menus exposing (AlbumConfig, defaultAlbumConfig)
import ViewAlbums exposing (AlbumSearch, getAlbumByExactKeybinding, halfPageDown, halfPageUp, pageDown, pageUp, resetPagination, updateAlbumSearchString, clearAlbumSearchWarning)

-- Action type for album browsing
type AlbumAction
    = ChangeToMainMenu
    | SelectAlbumForView ImmichAlbum
    | UpdateAlbumSearch AlbumSearch
    | InvalidKeybindingInput String AlbumSearch -- New action for invalid input
    | NoAlbumAction

-- Message type for album-related actions
type AlbumMsg
    = AlbumBrowseKeyPress String AlbumSearch

-- Function to get top search match  
getTopMatchToSearch : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> Maybe ImmichAlbum
getTopMatchToSearch search albumKeybindings albums =
    let
        matchesDict = ViewAlbums.filterToOnlySearchedForAlbums search albumKeybindings albums
    in
    if Dict.isEmpty matchesDict then
        Nothing
    else
        -- Find the album with the highest score
        matchesDict
            |> Dict.toList
            |> List.map (\( id, album ) -> ( Dict.get id search.albumScores |> Maybe.withDefault 0, album ))
            |> List.sortBy (\( score, _ ) -> -score)
            |> List.head
            |> Maybe.map (\( _, album ) -> album)

-- Album Browse keyboard handling
handleAlbumBrowseInput : String -> AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AlbumAction
handleAlbumBrowseInput key search albumKeybindings knownAlbums =
    case key of
        "Escape" ->
            ChangeToMainMenu
        "Enter" ->
            let
                maybeMatch = getTopMatchToSearch search albumKeybindings knownAlbums
            in
            case maybeMatch of
                Just album ->
                    SelectAlbumForView album
                Nothing ->
                    NoAlbumAction
        "PageDown" ->
            let
                newSearch = { search | pagination = pageDown search.pagination }
            in
            UpdateAlbumSearch newSearch
        "PageUp" ->
            let
                newSearch = { search | pagination = pageUp search.pagination }
            in
            UpdateAlbumSearch newSearch
        _ ->
            if String.contains "Control" key then
                case String.replace "Control+" "" key of
                    "d" ->
                        let
                            newSearch = { search | pagination = halfPageDown search.pagination }
                        in
                        UpdateAlbumSearch newSearch
                    "u" ->
                        let
                            newSearch = { search | pagination = halfPageUp search.pagination }
                        in
                        UpdateAlbumSearch newSearch
                    "f" ->
                        let
                            newSearch = { search | pagination = pageDown search.pagination }
                        in
                        UpdateAlbumSearch newSearch
                    "b" ->
                        let
                            newSearch = { search | pagination = pageUp search.pagination }
                        in
                        UpdateAlbumSearch newSearch
                    _ ->
                        NoAlbumAction
            else if key == " " then
                let
                    newSearch = { search | pagination = pageDown search.pagination }
                in
                UpdateAlbumSearch newSearch
            else if not (String.isEmpty search.searchString) && isSupportedSearchLetter key then
                -- We're already in text search mode, continue with text search
                let
                    newSearch = updateAlbumSearchString (search.searchString ++ key) search knownAlbums
                in
                UpdateAlbumSearch newSearch
            else if isKeybindingLetter key then
                -- Always try keybinding mode for keybinding letters
                let
                    -- Clear any existing warning when typing ANY keybinding letter
                    searchWithoutWarning = clearAlbumSearchWarning search
                in
                case validateKeybindingInput search.partialKeybinding key albumKeybindings knownAlbums of
                    ExactMatch album ->
                        SelectAlbumForView album
                    ValidKeybinding newPartialKeybinding ->
                        let
                            newSearch = { searchWithoutWarning | partialKeybinding = newPartialKeybinding, pagination = resetPagination searchWithoutWarning.pagination }
                        in
                        UpdateAlbumSearch newSearch
                    InvalidCharacter invalidChar ->
                        -- Check if we have keybindings at all - if not, fall back to text search
                        if Dict.isEmpty albumKeybindings then
                            let
                                newSearch = updateAlbumSearchString (searchWithoutWarning.searchString ++ key) searchWithoutWarning knownAlbums
                            in
                            UpdateAlbumSearch newSearch
                        else
                            -- Reject invalid keybinding characters (but warning is already cleared)
                            InvalidKeybindingInput invalidChar searchWithoutWarning
            else if isSupportedSearchLetter key then
                -- Non-keybinding supported search letter, start text search
                let
                    newSearch = updateAlbumSearchString (search.searchString ++ key) search knownAlbums
                in
                UpdateAlbumSearch newSearch
            else if key == "Backspace" then
                -- Handle backspace for both keybinding and text search
                if String.length search.partialKeybinding > 0 then
                    -- Clear partial keybinding first
                    let
                        newPartialKeybinding = String.slice 0 (String.length search.partialKeybinding - 1) search.partialKeybinding
                        newSearch = { search | partialKeybinding = newPartialKeybinding, pagination = resetPagination search.pagination }
                            |> clearAlbumSearchWarning
                    in
                    UpdateAlbumSearch newSearch
                else
                    -- Clear text search
                    let
                        newSearchString = String.slice 0 (String.length search.searchString - 1) search.searchString
                        newSearch = updateAlbumSearchString newSearchString search knownAlbums
                            |> clearAlbumSearchWarning
                    in
                    UpdateAlbumSearch newSearch
            else
                NoAlbumAction


-- Update function that processes AlbumMsg and returns an action
-- This consolidates the album input handling logic from Main.elm
updateAlbums : AlbumMsg -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AlbumAction
updateAlbums albumMsg albumKeybindings knownAlbums =
    case albumMsg of
        AlbumBrowseKeyPress key search ->
            handleAlbumBrowseInput key search albumKeybindings knownAlbums