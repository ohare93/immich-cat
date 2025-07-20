module Menus exposing
    ( AlbumConfig
    , SearchConfig
    , SearchContext(..)
    , TimelineConfig
    , defaultAlbumConfig
    , defaultSearchConfig
    , defaultTimelineConfig
    , filterByMediaType
    , filterByStatus
    , toggleCategorisation
    , toggleMediaType
    , toggleOrder
    , toggleSearchContext
    , toggleStatus
    , viewAlbumView
    , viewMainMenu
    , viewSearchView
    , viewSettings
    , viewTimelineView
    )

import Dict exposing (Dict)
import Element exposing (Element, centerX, centerY, column, el, fill, fillPortion, height, minimum, paddingXY, px, row, text, width)
import Element.Font as Font
import Element.Input as Input exposing (button)
import HelpText exposing (AlbumBrowseState(..), ViewContext(..), viewContextHelp)
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
    }


type alias AlbumConfig =
    { mediaType : MediaTypeFilter
    , order : ImageOrder
    , status : StatusFilter
    }



-- Main menu view


viewMainMenu : Maybe String -> Element msg
viewMainMenu reloadFeedback =
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
                [ case reloadFeedback of
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


viewSearchView : { a | albumKeybindings : Dict ImmichAssetId String, currentAssets : List ImmichAssetId, imagesLoadState : ImmichLoadState, knownAlbums : Dict ImmichAssetId ImmichAlbum } -> SearchConfig -> msg -> Element msg
viewSearchView model config executeSearchMsg =
    row [ width fill, height fill ]
        [ column [ width (fillPortion 3 |> minimum 220), height fill, paddingXY 15 15, Element.spacingXY 0 15 ]
            [ el [ Font.size 20, Font.bold ] (text "🔍 Search Assets")
            , viewSearchFilters config
            , el [] (text ("Search Query: " ++ config.query))
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


maskApiKey : String -> String
maskApiKey apiKey =
    let
        keyLength =
            String.length apiKey
    in
    if keyLength > 5 then
        String.left 5 apiKey ++ String.repeat (keyLength - 5) "*"

    else
        apiKey



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


mediaTypeToString : MediaTypeFilter -> String
mediaTypeToString mediaType =
    case mediaType of
        AllMedia ->
            "All"

        ImagesOnly ->
            "Images"

        VideosOnly ->
            "Videos"


categorisationToString : CategorisationFilter -> String
categorisationToString categorisation =
    case categorisation of
        All ->
            "All"

        Uncategorised ->
            "Uncategorised"


orderToString : ImageOrder -> String
orderToString order =
    case order of
        CreatedDesc ->
            "Newest Created"

        CreatedAsc ->
            "Oldest Created"

        ModifiedDesc ->
            "Newest Modified"

        ModifiedAsc ->
            "Oldest Modified"

        Random ->
            "Random"


statusToString : StatusFilter -> String
statusToString status =
    case status of
        AllStatuses ->
            "All"

        FavoritesOnly ->
            "Favorites"

        ArchivedOnly ->
            "Archived"


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


toggleMediaType : MediaTypeFilter -> MediaTypeFilter
toggleMediaType current =
    case current of
        AllMedia ->
            ImagesOnly

        ImagesOnly ->
            VideosOnly

        VideosOnly ->
            AllMedia


toggleCategorisation : CategorisationFilter -> CategorisationFilter
toggleCategorisation current =
    case current of
        All ->
            Uncategorised

        Uncategorised ->
            All


toggleOrder : ImageOrder -> ImageOrder
toggleOrder current =
    case current of
        CreatedDesc ->
            CreatedAsc

        CreatedAsc ->
            ModifiedDesc

        ModifiedDesc ->
            ModifiedAsc

        ModifiedAsc ->
            Random

        Random ->
            CreatedDesc


toggleStatus : StatusFilter -> StatusFilter
toggleStatus current =
    case current of
        AllStatuses ->
            FavoritesOnly

        FavoritesOnly ->
            ArchivedOnly

        ArchivedOnly ->
            AllStatuses


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
    }


defaultAlbumConfig : AlbumConfig
defaultAlbumConfig =
    { mediaType = AllMedia
    , order = CreatedDesc
    , status = AllStatuses
    }



-- Filter functions


filterByMediaType : MediaTypeFilter -> List ImmichAsset -> List ImmichAsset
filterByMediaType mediaFilter assets =
    case mediaFilter of
        AllMedia ->
            assets

        ImagesOnly ->
            List.filter (\asset -> String.startsWith "image/" asset.mimeType) assets

        VideosOnly ->
            List.filter (\asset -> String.startsWith "video/" asset.mimeType) assets


filterByStatus : StatusFilter -> List ImmichAsset -> List ImmichAsset
filterByStatus statusFilter assets =
    case statusFilter of
        AllStatuses ->
            assets

        FavoritesOnly ->
            List.filter (\asset -> asset.isFavourite) assets

        ArchivedOnly ->
            List.filter (\asset -> asset.isArchived) assets
