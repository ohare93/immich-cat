module UpdateMenuConfig exposing
    ( updateAlbumMediaType
    , updateAlbumOrder
    , updateAlbumStatus
    , updateSearchClear
    , updateSearchContext
    , updateSearchInputBlurred
    , updateSearchInputFocused
    , updateSearchMediaType
    , updateSearchQuery
    , updateSearchSelectSuggestion
    , updateSearchStatus
    , updateTimelineCategorisation
    , updateTimelineMediaType
    , updateTimelineOrder
    , updateTimelineStatus
    )

{-| Pure functions for updating menu configuration state.

These functions update MenuState without generating Commands.
Main.elm uses a helper to apply these to model.userMode.

-}

import AssetSourceTypes exposing (AlbumConfig)
import Dict exposing (Dict)
import Immich exposing (CategorisationFilter, ImageOrder, ImmichAlbum, ImmichAsset, ImmichAssetId, MediaTypeFilter, SearchContext, StatusFilter)
import Menus exposing (SearchConfig, TimelineConfig, addToRecentSearches, generateSearchSuggestions)
import UpdateMenus exposing (MenuState(..))



-- Timeline Config Updates


updateTimelineMediaType : MediaTypeFilter -> MenuState -> MenuState
updateTimelineMediaType newMediaType menuState =
    case menuState of
        TimelineView config ->
            TimelineView { config | mediaType = newMediaType }

        _ ->
            menuState


updateTimelineCategorisation : CategorisationFilter -> MenuState -> MenuState
updateTimelineCategorisation newCategorisation menuState =
    case menuState of
        TimelineView config ->
            TimelineView { config | categorisation = newCategorisation }

        _ ->
            menuState


updateTimelineOrder : ImageOrder -> MenuState -> MenuState
updateTimelineOrder newOrder menuState =
    case menuState of
        TimelineView config ->
            TimelineView { config | order = newOrder }

        _ ->
            menuState


updateTimelineStatus : StatusFilter -> MenuState -> MenuState
updateTimelineStatus newStatus menuState =
    case menuState of
        TimelineView config ->
            TimelineView { config | status = newStatus }

        _ ->
            menuState



-- Search Config Updates


updateSearchMediaType : MediaTypeFilter -> MenuState -> MenuState
updateSearchMediaType newMediaType menuState =
    case menuState of
        SearchView config ->
            SearchView { config | mediaType = newMediaType }

        _ ->
            menuState


updateSearchContext : SearchContext -> MenuState -> MenuState
updateSearchContext newContext menuState =
    case menuState of
        SearchView config ->
            SearchView { config | searchContext = newContext }

        _ ->
            menuState


updateSearchStatus : StatusFilter -> MenuState -> MenuState
updateSearchStatus newStatus menuState =
    case menuState of
        SearchView config ->
            SearchView { config | status = newStatus }

        _ ->
            menuState


updateSearchQuery : Dict ImmichAssetId ImmichAsset -> String -> MenuState -> MenuState
updateSearchQuery knownAssets newQuery menuState =
    case menuState of
        SearchView config ->
            let
                suggestions =
                    if String.length newQuery > 1 then
                        generateSearchSuggestions knownAssets

                    else
                        []
            in
            SearchView
                { config
                    | query = newQuery
                    , suggestions = suggestions
                    , showSuggestions = String.length newQuery > 1
                }

        _ ->
            menuState


updateSearchSelectSuggestion : String -> MenuState -> MenuState
updateSearchSelectSuggestion suggestion menuState =
    case menuState of
        SearchView config ->
            let
                updatedRecentSearches =
                    addToRecentSearches suggestion config.recentSearches
            in
            SearchView
                { config
                    | query = suggestion
                    , recentSearches = updatedRecentSearches
                    , showSuggestions = False
                }

        _ ->
            menuState


updateSearchClear : MenuState -> MenuState
updateSearchClear menuState =
    case menuState of
        SearchView config ->
            SearchView
                { config
                    | query = ""
                    , showSuggestions = False
                }

        _ ->
            menuState


updateSearchInputFocused : MenuState -> MenuState
updateSearchInputFocused menuState =
    case menuState of
        SearchView config ->
            SearchView { config | inputFocused = True }

        _ ->
            menuState


updateSearchInputBlurred : MenuState -> MenuState
updateSearchInputBlurred menuState =
    case menuState of
        SearchView config ->
            SearchView { config | inputFocused = False }

        _ ->
            menuState



-- Album Config Updates


updateAlbumMediaType : MediaTypeFilter -> MenuState -> MenuState
updateAlbumMediaType newMediaType menuState =
    case menuState of
        AlbumView album config ->
            AlbumView album { config | mediaType = newMediaType }

        _ ->
            menuState


updateAlbumOrder : ImageOrder -> MenuState -> MenuState
updateAlbumOrder newOrder menuState =
    case menuState of
        AlbumView album config ->
            AlbumView album { config | order = newOrder }

        _ ->
            menuState


updateAlbumStatus : StatusFilter -> MenuState -> MenuState
updateAlbumStatus newStatus menuState =
    case menuState of
        AlbumView album config ->
            AlbumView album { config | status = newStatus }

        _ ->
            menuState
