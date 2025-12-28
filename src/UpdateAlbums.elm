module UpdateAlbums exposing
    ( AlbumMsg(..)
    , AlbumResult(..)
    , getTopMatchToSearch
    , handleAlbumBrowseInput
    , updateAlbums
    )

import Dict exposing (Dict)
import Helpers exposing (isKeybindingLetter, isSupportedSearchLetter)
import Immich exposing (ImmichAlbum, ImmichAlbumId)
import KeybindingValidation exposing (KeybindingValidationResult(..), validateKeybindingInput)
import ViewAlbums exposing (AlbumSearch, clearAlbumSearchWarning, halfPageDown, halfPageUp, pageDown, pageUp, resetPagination, updateAlbumSearchString)



-- Result type for album browsing


type AlbumResult
    = ChangeToMainMenu
    | SelectAlbumForView ImmichAlbum
    | UpdateAlbumSearch AlbumSearch
    | InvalidKeybindingInput String AlbumSearch -- New action for invalid input
    | NoAlbumResult



-- Message type for album-related actions


type AlbumMsg
    = AlbumBrowseKeyPress String AlbumSearch



-- Function to get top search match


getTopMatchToSearch : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> Maybe ImmichAlbum
getTopMatchToSearch search albumKeybindings albums =
    let
        matchesDict =
            ViewAlbums.filterToOnlySearchedForAlbums search albumKeybindings albums
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


handleAlbumBrowseInput : String -> AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AlbumResult
handleAlbumBrowseInput key search albumKeybindings knownAlbums =
    case key of
        "Escape" ->
            if search.inputFocused then
                -- Exit text search mode, clear search
                UpdateAlbumSearch { search | inputFocused = False, searchString = "", partialKeybinding = "" }

            else
                ChangeToMainMenu

        "Enter" ->
            let
                maybeMatch =
                    getTopMatchToSearch search albumKeybindings knownAlbums
            in
            case maybeMatch of
                Just album ->
                    SelectAlbumForView album

                Nothing ->
                    NoAlbumResult

        "PageDown" ->
            let
                newSearch =
                    { search | pagination = pageDown search.pagination }
            in
            UpdateAlbumSearch newSearch

        "PageUp" ->
            let
                newSearch =
                    { search | pagination = pageUp search.pagination }
            in
            UpdateAlbumSearch newSearch

        "Control+d" ->
            let
                newSearch =
                    { search | pagination = halfPageDown search.pagination }
            in
            UpdateAlbumSearch newSearch

        "Control+u" ->
            let
                newSearch =
                    { search | pagination = halfPageUp search.pagination }
            in
            UpdateAlbumSearch newSearch

        "Control+f" ->
            let
                newSearch =
                    { search | pagination = pageDown search.pagination }
            in
            UpdateAlbumSearch newSearch

        "Control+b" ->
            let
                newSearch =
                    { search | pagination = pageUp search.pagination }
            in
            UpdateAlbumSearch newSearch

        " " ->
            let
                newSearch =
                    { search | pagination = pageDown search.pagination }
            in
            UpdateAlbumSearch newSearch

        "I" ->
            if not search.inputFocused then
                -- Enter text search mode without adding 'I' to search
                UpdateAlbumSearch { search | inputFocused = True }

            else
                -- Already in text search mode, add 'I' to search
                let
                    newSearch =
                        updateAlbumSearchString (search.searchString ++ key) search albumKeybindings knownAlbums
                in
                UpdateAlbumSearch newSearch

        _ ->
            if search.inputFocused then
                -- In text search mode: all letters go to search, no keybinding processing
                if isSupportedSearchLetter key then
                    let
                        newSearch =
                            updateAlbumSearchString (search.searchString ++ key) search albumKeybindings knownAlbums
                    in
                    UpdateAlbumSearch newSearch

                else if key == "Backspace" then
                    let
                        newSearchString =
                            String.slice 0 (String.length search.searchString - 1) search.searchString

                        newSearch =
                            updateAlbumSearchString newSearchString search albumKeybindings knownAlbums
                    in
                    UpdateAlbumSearch newSearch

                else
                    NoAlbumResult

            else if not (String.isEmpty search.searchString) && isSupportedSearchLetter key then
                -- We're already in text search mode (legacy path), continue with text search
                let
                    newSearch =
                        updateAlbumSearchString (search.searchString ++ key) search albumKeybindings knownAlbums
                in
                UpdateAlbumSearch newSearch

            else if isKeybindingLetter key then
                -- Normal mode: try keybinding mode for keybinding letters
                let
                    -- Clear any existing warning when typing ANY keybinding letter
                    searchWithoutWarning =
                        clearAlbumSearchWarning search
                in
                case validateKeybindingInput search.partialKeybinding key albumKeybindings knownAlbums of
                    ExactMatch album ->
                        SelectAlbumForView album

                    ValidKeybinding newPartialKeybinding ->
                        let
                            newSearch =
                                { searchWithoutWarning | partialKeybinding = newPartialKeybinding, pagination = resetPagination searchWithoutWarning.pagination }
                        in
                        UpdateAlbumSearch newSearch

                    InvalidCharacter invalidChar ->
                        -- Check if we have keybindings at all - if not, fall back to text search
                        if Dict.isEmpty albumKeybindings then
                            let
                                newSearch =
                                    updateAlbumSearchString (searchWithoutWarning.searchString ++ key) searchWithoutWarning albumKeybindings knownAlbums
                            in
                            UpdateAlbumSearch newSearch

                        else
                            -- Reject invalid keybinding characters (but warning is already cleared)
                            InvalidKeybindingInput invalidChar searchWithoutWarning

            else if isSupportedSearchLetter key then
                -- Non-keybinding supported search letter, start text search
                let
                    newSearch =
                        updateAlbumSearchString (search.searchString ++ key) search albumKeybindings knownAlbums
                in
                UpdateAlbumSearch newSearch

            else if key == "Backspace" then
                -- Handle backspace for both keybinding and text search
                if String.length search.partialKeybinding > 0 then
                    -- Clear partial keybinding first
                    let
                        newPartialKeybinding =
                            String.slice 0 (String.length search.partialKeybinding - 1) search.partialKeybinding

                        newSearch =
                            { search | partialKeybinding = newPartialKeybinding, pagination = resetPagination search.pagination }
                                |> clearAlbumSearchWarning
                    in
                    UpdateAlbumSearch newSearch

                else
                    -- Clear text search
                    let
                        newSearchString =
                            String.slice 0 (String.length search.searchString - 1) search.searchString

                        newSearch =
                            updateAlbumSearchString newSearchString search albumKeybindings knownAlbums
                                |> clearAlbumSearchWarning
                    in
                    UpdateAlbumSearch newSearch

            else
                NoAlbumResult



-- Update function that processes AlbumMsg and returns an action
-- This consolidates the album input handling logic from Main.elm


updateAlbums : AlbumMsg -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AlbumResult
updateAlbums albumMsg albumKeybindings knownAlbums =
    case albumMsg of
        AlbumBrowseKeyPress key search ->
            handleAlbumBrowseInput key search albumKeybindings knownAlbums
