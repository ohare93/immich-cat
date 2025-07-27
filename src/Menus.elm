module Menus exposing
    ( AlbumConfig
    , SearchConfig
    , SearchContext(..)
    , TimelineConfig
    , addToRecentSearches
    , defaultAlbumConfig
    , defaultSearchConfig
    , defaultTimelineConfig
    , generateSearchSuggestions
    , toggleSearchContext
    , viewAlbumView
    , viewMainMenu
    , viewSearchView
    , viewSettings
    , viewTimelineView
    )

import Date
import Dict exposing (Dict)
import Element exposing (Element, centerX, centerY, column, el, fill, fillPortion, height, minimum, paddingXY, px, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import HelpText exposing (AlbumBrowseState(..), ViewContext(..), viewContextHelp)
import Helpers exposing (categorisationToString, mediaTypeToString, orderToString, statusToString)
import Immich exposing (CategorisationFilter(..), ImageOrder(..), ImmichAlbum, ImmichAsset, ImmichAssetId, ImmichLoadState(..), MediaTypeFilter(..), StatusFilter(..))



-- Types that need to be imported from Main


type SearchContext
    = ContentSearch
    | FilenameSearch
    | DescriptionSearch


type alias TimelineConfig =
    { mediaType : MediaTypeFilter
    , categorisation : CategorisationFilter
    , order : ImageOrder
    , status : StatusFilter
    }


type alias SearchConfig =
    { mediaType : MediaTypeFilter
    , searchContext : SearchContext
    , status : StatusFilter
    , query : String
    , inputFocused : Bool
    , recentSearches : List String
    , showSuggestions : Bool
    , suggestions : List String
    }


type alias AlbumConfig =
    { mediaType : MediaTypeFilter
    , order : ImageOrder
    , status : StatusFilter
    }



-- Main menu view


viewMainMenu : Bool -> Maybe String -> Bool -> Element msg
viewMainMenu isMobile reloadFeedback isConfigured =
    if isMobile then
        column [ width fill, height fill, paddingXY 10 20, Element.spacingXY 0 20 ]
            [ el [ Font.size 20, Font.bold, Element.centerX ] (text "Image Categorizer")
            , column [ width fill, Element.spacingXY 0 15 ]
                [ el [ Font.size 16, Font.bold ] (text "Choose View Mode")
                , viewMainMenuOption "t" "📅 Timeline View" "Browse all assets with timeline filters"
                , viewMainMenuOption "s" "🔍 Search Assets" "Smart search with context options"
                , viewMainMenuOption "a" "📁 Browse Albums" "Select and view album contents"
                , viewMainMenuOption "r" "↻ Reload Albums" "Refresh album list from server"
                , viewMainMenuOption "g" "⚙️ Settings" "Configure preferences and options"
                ]
            , if not isConfigured then
                column [ Element.spacingXY 0 8, Element.centerX ]
                    [ el [ Font.size 14, Font.bold, Font.color (Element.rgb 0.8 0.2 0.2), Element.centerX ] (text "⚠️ Configuration Required")
                    , el [ Font.size 12, Element.centerX ] (text "Please configure server URL and API key in Settings (press 'g')")
                    ]

              else
                Element.none
            , case reloadFeedback of
                Just feedback ->
                    el [ Font.size 12, Element.centerX ] (text feedback)

                Nothing ->
                    Element.none
            ]

    else
        row [ width fill, height fill ]
            [ column [ width <| fillPortion 1, height fill, Element.spacingXY 0 20, paddingXY 20 20 ]
                [ el [ Font.size 24, Font.bold ] (text "Image Categorizer")
                , column [ Element.spacingXY 0 15 ]
                    [ el [ Font.size 18, Font.bold ] (text "Choose View Mode")
                    , viewMainMenuOption "t" "📅 Timeline View" "Browse all assets with timeline filters"
                    , viewMainMenuOption "s" "🔍 Search Assets" "Smart search with context options"
                    , viewMainMenuOption "a" "📁 Browse Albums" "Select and view album contents"
                    , viewMainMenuOption "r" "↻ Reload Albums" "Refresh album list from server"
                    , viewMainMenuOption "g" "⚙️ Settings" "Configure preferences and options"
                    ]
                , column [ Element.spacingXY 0 10 ]
                    [ if not isConfigured then
                        column [ Element.spacingXY 0 5 ]
                            [ el [ Font.size 16, Font.bold, Font.color (Element.rgb 0.8 0.2 0.2) ] (text "⚠️ Configuration Required")
                            , el [ Font.size 13 ] (text "Please configure server URL and API key in Settings (press 'g')")
                            ]

                      else
                        Element.none
                    , case reloadFeedback of
                        Just message ->
                            el [ Font.size 14, Font.color (Element.rgb 0.2 0.6 1.0) ] (text message)

                        Nothing ->
                            Element.none
                    , el [ Font.size 12 ] (text "Press the highlighted key or click to navigate")
                    ]
                ]
            , el [ width <| fillPortion 1, height fill, paddingXY 20 20 ] <| viewContextHelp MainMenuContext
            ]


viewMainMenuOption : String -> String -> String -> Element msg
viewMainMenuOption key title description =
    row [ width fill, Element.spacingXY 10 0, paddingXY 10 8 ]
        [ el [ Font.bold, Font.color (Element.rgb 0.2 0.6 1.0), width (px 20) ] (text key)
        , column [ Element.spacingXY 0 3 ]
            [ el [ Font.size 16, Font.bold ] (text title)
            , el [ Font.size 14, Font.color (Element.rgb 0.6 0.6 0.6) ] (text description)
            ]
        ]



-- Timeline view


viewTimelineView : { a | albumKeybindings : Dict ImmichAssetId String, currentAssets : List ImmichAssetId, imagesLoadState : ImmichLoadState, knownAlbums : Dict ImmichAssetId ImmichAlbum, knownAssets : Dict ImmichAssetId ImmichAsset, imageIndex : Int } -> TimelineConfig -> msg -> msg -> Element msg
viewTimelineView model config loadDataMsg loadTimelineAssetsMsg =
    row [ width fill, height fill ]
        [ column [ width (fillPortion 3 |> minimum 220), height fill, paddingXY 15 15, Element.spacingXY 0 15 ]
            [ el [ Font.size 20, Font.bold ] (text "📅 Timeline View")
            , viewTimelineFilters config
            , button [] { onPress = Just loadTimelineAssetsMsg, label = text "[Enter/Space] Load & View Assets" }
            ]
        , column [ width (fillPortion 4 |> minimum 300), height fill, paddingXY 15 15 ]
            [ viewContextHelp TimelineContext
            ]
        ]



-- Search view


viewSearchView : { a | albumKeybindings : Dict ImmichAssetId String, currentAssets : List ImmichAssetId, imagesLoadState : ImmichLoadState, knownAlbums : Dict ImmichAssetId ImmichAlbum, knownAssets : Dict ImmichAssetId ImmichAsset } -> SearchConfig -> (String -> msg) -> (String -> msg) -> msg -> msg -> Element msg
viewSearchView model config onQueryChange onSuggestionSelect executeSearchMsg clearSearchMsg =
    row [ width fill, height fill ]
        [ column [ width (fillPortion 3 |> minimum 220), height fill, paddingXY 15 15, Element.spacingXY 0 15 ]
            [ el [ Font.size 20, Font.bold ] (text "🔍 Search Assets")
            , viewSearchFilters config
            , viewEnhancedSearchInput config onQueryChange onSuggestionSelect clearSearchMsg
            , button [] { onPress = Just executeSearchMsg, label = text "[Enter/Space] Search & View Results" }
            ]
        , column [ width (fillPortion 4 |> minimum 300), height fill, paddingXY 15 15 ]
            [ viewContextHelp (SearchContext { inputFocused = config.inputFocused })
            ]
        ]



-- Album view


viewAlbumView : { a | albumKeybindings : Dict ImmichAssetId String, currentAssets : List ImmichAssetId, imagesLoadState : ImmichLoadState, knownAlbums : Dict ImmichAssetId ImmichAlbum } -> ImmichAlbum -> AlbumConfig -> (ImmichAlbum -> msg) -> Element msg
viewAlbumView model album config loadAlbumAssetsMsg =
    row [ width fill, height fill ]
        [ column [ width (fillPortion 3 |> minimum 220), height fill, paddingXY 15 15, Element.spacingXY 0 15 ]
            [ el [ Font.size 20, Font.bold ] (text ("📁 " ++ album.albumName))
            , el [ Font.size 14 ] (text (String.fromInt album.assetCount ++ " assets"))
            , viewAlbumFilters config
            , button [] { onPress = Just (loadAlbumAssetsMsg album), label = text "[Enter/Space] Load & View Assets" }
            ]
        , column [ width (fillPortion 6), height fill ]
            [ checkForEmptyFilterResults model config album
            ]
        , column [ width (fillPortion 4 |> minimum 300), height fill, paddingXY 15 15 ]
            [ viewContextHelp (AlbumBrowseContext ConfiguringView)
            ]
        ]


checkForEmptyFilterResults : { a | albumKeybindings : Dict ImmichAssetId String, currentAssets : List ImmichAssetId, imagesLoadState : ImmichLoadState, knownAlbums : Dict ImmichAssetId ImmichAlbum } -> AlbumConfig -> ImmichAlbum -> Element msg
checkForEmptyFilterResults model config album =
    case model.imagesLoadState of
        ImmichLoadSuccess ->
            if List.isEmpty model.currentAssets then
                column [ centerX, centerY, Element.spacingXY 0 20 ]
                    [ el [ Font.size 18, Font.bold, Font.color <| Element.rgb 0.7 0.7 0.7 ] (text "No assets found")
                    , el [ Font.size 14, Font.color <| Element.rgb 0.5 0.5 0.5 ] (text "Try adjusting your filters:")
                    , column [ Element.spacingXY 0 8 ]
                        [ viewFilterValue "Media Type" (mediaTypeToString config.mediaType)
                        , viewFilterValue "Order" (orderToString config.order)
                        , viewFilterValue "Status" (statusToString config.status)
                        ]
                    , el [ Font.size 12, Font.color <| Element.rgb 0.5 0.5 0.5 ] (text "Press [m], [o], or [s] to change filters")
                    ]

            else
                text "Album assets will appear here"

        _ ->
            text "Loading album assets..."



-- Settings view


viewSettings : { a | albumKeybindings : Dict ImmichAssetId String, currentAssets : List ImmichAssetId, imagesLoadState : ImmichLoadState, knownAlbums : Dict ImmichAssetId ImmichAlbum, configuredApiUrl : Maybe String, configuredApiKey : Maybe String, settingsApiUrl : String, settingsApiKey : String, configValidationMessage : Maybe String } -> (String -> msg) -> (String -> msg) -> (String -> String -> msg) -> msg -> Element msg
viewSettings model onUrlChange onApiKeyChange onSaveConfig onClearConfig =
    row [ width fill, height fill ]
        [ column [ width (fillPortion 3 |> minimum 400), height fill, paddingXY 20 20, Element.spacingXY 0 20 ]
            [ el [ Font.size 24, Font.bold ] (text "⚙️ Settings")
            , column [ Element.spacingXY 0 20 ]
                [ -- Immich Configuration Section
                  column [ Element.spacingXY 0 15 ]
                    [ el [ Font.size 18, Font.bold ] (text "Immich Configuration")
                    , el [ Font.size 14 ] (text "Configure your Immich server connection:")
                    , column [ Element.spacingXY 0 10 ]
                        [ Input.text [ width fill ]
                            { onChange = onUrlChange
                            , text = model.settingsApiUrl
                            , placeholder = Just (Input.placeholder [] (text "https://your-immich-server.com"))
                            , label = Input.labelAbove [] (text "Immich Server URL")
                            }
                        , Input.currentPassword [ width fill ]
                            { onChange = onApiKeyChange
                            , text = model.settingsApiKey
                            , placeholder = Just (Input.placeholder [] (text "Your API key"))
                            , label = Input.labelAbove [] (text "API Key")
                            , show = False
                            }
                        ]
                    ]
                , -- Action Buttons
                  row [ Element.spacingXY 15 0 ]
                    [ button []
                        { onPress = Just (onSaveConfig model.settingsApiUrl model.settingsApiKey)
                        , label = text "Save Configuration"
                        }
                    , button []
                        { onPress = Just onClearConfig
                        , label = text "Clear Configuration"
                        }
                    ]
                , -- Validation message
                  case model.configValidationMessage of
                    Just message ->
                        el
                            [ Font.size 14
                            , Font.color
                                (if String.startsWith "✅" message then
                                    Element.rgb 0.2 0.7 0.2

                                 else
                                    Element.rgb 0.8 0.2 0.2
                                )
                            ]
                            (text message)

                    Nothing ->
                        Element.none
                , -- Current Configuration Status
                  column [ Element.spacingXY 0 8 ]
                    [ el [ Font.size 16, Font.bold ] (text "Current Configuration")
                    , case ( model.configuredApiUrl, model.configuredApiKey ) of
                        ( Just url, Just _ ) ->
                            column [ Element.spacingXY 0 5 ]
                                [ el [ Font.size 14 ] (text ("✅ Connected to: " ++ url))
                                , el [ Font.size 14 ] (text "✅ API key configured")
                                ]

                        _ ->
                            el [ Font.size 14, Font.color (Element.rgb 0.7 0.5 0.1) ] (text "⚠️ Using development server configuration")
                    ]
                ]
            , el [ Font.size 12 ] (text "Press Escape to return to main menu")
            ]
        , column [ width (fillPortion 2 |> minimum 300), height fill, paddingXY 15 15 ]
            [ viewContextHelp HelpText.SettingsContext
            ]
        ]



-- Filter panel views


viewTimelineFilters : TimelineConfig -> Element msg
viewTimelineFilters config =
    column [ Element.spacingXY 0 12 ]
        [ el [ Font.size 16, Font.bold ] (text "Current Filters")
        , viewFilterValue "Media Type" (mediaTypeToString config.mediaType)
        , viewFilterValue "Categorisation" (categorisationToString config.categorisation)
        , viewFilterValue "Order" (orderToString config.order)
        , viewFilterValue "Status" (statusToString config.status)
        ]


viewSearchFilters : SearchConfig -> Element msg
viewSearchFilters config =
    column [ Element.spacingXY 0 12 ]
        [ el [ Font.size 16, Font.bold ] (text "Current Filters")
        , viewFilterValue "Media Type" (mediaTypeToString config.mediaType)
        , viewFilterValue "Search Context" (searchContextToString config.searchContext)
        , viewFilterValue "Status" (statusToString config.status)
        ]


viewAlbumFilters : AlbumConfig -> Element msg
viewAlbumFilters config =
    column [ Element.spacingXY 0 12 ]
        [ el [ Font.size 16, Font.bold ] (text "Current Filters")
        , viewFilterValue "Media Type" (mediaTypeToString config.mediaType)
        , viewFilterValue "Order" (orderToString config.order)
        , viewFilterValue "Status" (statusToString config.status)
        ]


viewFilterValue : String -> String -> Element msg
viewFilterValue label value =
    row [ Element.spacingXY 10 0 ]
        [ el [ Font.size 14, Font.bold, width (px 100) ] (text (label ++ ":"))
        , el [ Font.size 14, Font.color (Element.rgb 0.2 0.6 1.0) ] (text value)
        ]



-- String conversion functions


searchContextToString : SearchContext -> String
searchContextToString context =
    case context of
        ContentSearch ->
            "Content"

        FilenameSearch ->
            "Filename"

        DescriptionSearch ->
            "Description"



-- Toggle functions


toggleSearchContext : SearchContext -> SearchContext
toggleSearchContext current =
    case current of
        ContentSearch ->
            FilenameSearch

        FilenameSearch ->
            DescriptionSearch

        DescriptionSearch ->
            ContentSearch



-- Default configurations


defaultTimelineConfig : TimelineConfig
defaultTimelineConfig =
    { mediaType = AllMedia
    , categorisation = Uncategorised
    , order = CreatedDesc
    , status = AllStatuses
    }


defaultSearchConfig : SearchConfig
defaultSearchConfig =
    { mediaType = AllMedia
    , searchContext = ContentSearch
    , status = AllStatuses
    , query = ""
    , inputFocused = False
    , recentSearches = []
    , showSuggestions = False
    , suggestions = []
    }


defaultAlbumConfig : AlbumConfig
defaultAlbumConfig =
    { mediaType = AllMedia
    , order = CreatedDesc
    , status = AllStatuses
    }



-- Enhanced search input with suggestions and recent searches


viewEnhancedSearchInput : SearchConfig -> (String -> msg) -> (String -> msg) -> msg -> Element msg
viewEnhancedSearchInput config onQueryChange onSuggestionSelect clearSearchMsg =
    let
        inputField =
            Input.text
                [ width fill
                , Element.below
                    (if config.showSuggestions && (not (List.isEmpty config.suggestions) || not (List.isEmpty config.recentSearches)) then
                        viewSearchSuggestions config onSuggestionSelect

                     else
                        Element.none
                    )
                ]
                { onChange = onQueryChange
                , text = config.query
                , placeholder = Just (Input.placeholder [] (text "Type to search assets..."))
                , label = Input.labelAbove [ Font.size 14, Font.bold ] (text "Search Query")
                }

        clearButton =
            if String.isEmpty config.query then
                Element.none

            else
                Input.button
                    [ Element.alignRight
                    , paddingXY 8 4
                    , Font.size 12
                    , Font.color (Element.rgb 0.6 0.6 0.6)
                    , Element.mouseOver [ Font.color (Element.rgb 0.8 0.2 0.2) ]
                    ]
                    { onPress = Just clearSearchMsg
                    , label = text "✕ Clear"
                    }
    in
    column [ width fill, Element.spacingXY 0 5 ]
        [ row [ width fill, Element.spacingXY 10 0 ]
            [ el [ width fill ] inputField
            , clearButton
            ]
        , if not (List.isEmpty config.recentSearches) && String.isEmpty config.query then
            viewRecentSearches config onSuggestionSelect

          else
            Element.none
        ]


viewSearchSuggestions : SearchConfig -> (String -> msg) -> Element msg
viewSearchSuggestions config onSuggestionSelect =
    let
        allSuggestions =
            config.suggestions ++ config.recentSearches

        limitedSuggestions =
            allSuggestions
                |> List.filter (\suggestion -> String.contains (String.toLower config.query) (String.toLower suggestion))
                |> List.take 8
    in
    if List.isEmpty limitedSuggestions then
        Element.none

    else
        column
            [ width fill
            , Background.color (Element.rgb 1 1 1)
            , Border.color (Element.rgb 0.8 0.8 0.8)
            , Border.width 1
            , Border.rounded 4
            , Element.spacingXY 0 0
            , Element.moveDown 2
            , Element.inFront Element.none
            ]
            (List.map (viewSuggestionItem onSuggestionSelect) limitedSuggestions)


viewSuggestionItem : (String -> msg) -> String -> Element msg
viewSuggestionItem onSuggestionSelect suggestion =
    Input.button
        [ width fill
        , paddingXY 12 8
        , Element.mouseOver [ Background.color (Element.rgb 0.95 0.95 0.95) ]
        , Font.size 14
        ]
        { onPress = Just (onSuggestionSelect suggestion)
        , label =
            row [ width fill, Element.spacingXY 8 0 ]
                [ el [ Font.color (Element.rgb 0.4 0.4 0.4), Font.size 12 ] (text "🔍")
                , el [] (text suggestion)
                ]
        }


viewRecentSearches : SearchConfig -> (String -> msg) -> Element msg
viewRecentSearches config onSuggestionSelect =
    if List.isEmpty config.recentSearches then
        Element.none

    else
        column [ width fill, Element.spacingXY 0 8 ]
            [ el [ Font.size 12, Font.bold, Font.color (Element.rgb 0.6 0.6 0.6) ] (text "Recent Searches:")
            , Element.wrappedRow [ Element.spacingXY 8 4 ]
                (List.take 5 config.recentSearches
                    |> List.map (viewRecentSearchChip onSuggestionSelect)
                )
            ]


viewRecentSearchChip : (String -> msg) -> String -> Element msg
viewRecentSearchChip onSuggestionSelect search =
    Input.button
        [ Background.color (Element.rgb 0.9 0.9 0.9)
        , Font.color (Element.rgb 0.4 0.4 0.4)
        , paddingXY 8 4
        , Border.rounded 12
        , Font.size 12
        , Element.mouseOver [ Background.color (Element.rgb 0.8 0.8 0.8) ]
        ]
        { onPress = Just (onSuggestionSelect search)
        , label = text search
        }



-- Helper functions for search suggestions


dateToString : Date.Date -> String
dateToString date =
    String.fromInt (Date.year date)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt (Date.monthNumber date))
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt (Date.day date))


generateSearchSuggestions : Dict ImmichAssetId ImmichAsset -> List String
generateSearchSuggestions knownAssets =
    let
        extractTerms asset =
            [ asset.title
            , extractDateTerms (dateToString asset.fileCreatedAt)
            , extractFileTypeTerms asset.mimeType
            , asset.duration |> Maybe.withDefault ""
            ]
                |> List.filter (not << String.isEmpty)

        extractDateTerms dateString =
            -- Extract year, month from date string like "2023-05-15T10:30:00Z"
            case String.split "-" (String.left 10 dateString) of
                year :: month :: _ ->
                    year ++ " " ++ monthNumberToName month

                _ ->
                    ""

        extractFileTypeTerms mimeType =
            if String.startsWith "image/" mimeType then
                "image photo picture"

            else if String.startsWith "video/" mimeType then
                "video movie clip"

            else
                ""

        monthNumberToName month =
            case month of
                "01" ->
                    "January"

                "02" ->
                    "February"

                "03" ->
                    "March"

                "04" ->
                    "April"

                "05" ->
                    "May"

                "06" ->
                    "June"

                "07" ->
                    "July"

                "08" ->
                    "August"

                "09" ->
                    "September"

                "10" ->
                    "October"

                "11" ->
                    "November"

                "12" ->
                    "December"

                _ ->
                    ""

        allTerms =
            Dict.values knownAssets
                |> List.concatMap extractTerms
                |> List.concatMap (String.split " ")
                |> List.map String.trim
                |> List.filter (\term -> String.length term > 2)
                |> List.map String.toLower

        termFrequency =
            List.foldl
                (\term acc -> Dict.update term (\count -> Just (Maybe.withDefault 0 count + 1)) acc)
                Dict.empty
                allTerms
    in
    Dict.toList termFrequency
        |> List.sortBy (\( _, count ) -> -count)
        |> List.take 20
        |> List.map (\( term, _ ) -> term)


addToRecentSearches : String -> List String -> List String
addToRecentSearches newSearch recentSearches =
    if String.isEmpty (String.trim newSearch) then
        recentSearches

    else
        let
            trimmedSearch =
                String.trim newSearch

            filteredRecent =
                List.filter ((/=) trimmedSearch) recentSearches
        in
        (trimmedSearch :: filteredRecent)
            |> List.take 10
