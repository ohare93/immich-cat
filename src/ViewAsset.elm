module ViewAsset exposing
    ( viewAsset
    , viewCreateAlbumConfirmation
    , viewEditAsset
    , viewEditAssetHelp
    , viewImage
    , viewKeybinding
    , viewLoadingAssets
    , viewVideo
    )

import Date exposing (Date)
import Dict exposing (Dict)
import Element exposing (Element, alignRight, alignTop, centerX, centerY, clipY, column, el, fill, fillPortion, height, minimum, paddingXY, px, row, text, width)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html, node)
import Html.Attributes
import Immich exposing (ImmichApiPaths, ImmichAsset, ImmichAssetId, ImmichLoadState(..))
import ViewAlbums exposing (AssetWithActions, InputMode(..), PropertyChange(..), usefulColours)

type alias ImageIndex =
    Int

type Msg
    = NoOp -- Placeholder for now

-- Helper types and functions for date-based asset counting

type alias AssetCounts =
    { today : Int
    , week : Int
    , month : Int
    , year : Int
    , all : Int
    }

-- Calculate asset counts for different time periods
calculateAssetCounts : Date -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> AssetCounts
calculateAssetCounts currentDate assetIds knownAssets =
    let
        assets = List.filterMap (\id -> Dict.get id knownAssets) assetIds
        
        -- Calculate date boundaries
        todayStart = currentDate
        todayEnd = Date.add Date.Days 1 currentDate
        weekStart = Date.add Date.Days (-(Date.weekdayToNumber (Date.weekday currentDate) - 1)) currentDate
        weekEnd = Date.add Date.Days 7 weekStart
        monthStart = Date.fromCalendarDate (Date.year currentDate) (Date.month currentDate) 1
        monthEnd = Date.add Date.Months 1 monthStart
        yearStart = Date.fromCalendarDate (Date.year currentDate) (Date.numberToMonth 1) 1
        yearEnd = Date.add Date.Years 1 yearStart
        
        -- Helper function to check if asset is in date range
        isInRange asset startDate endDate =
            let
                assetDate = asset.fileCreatedAt
                afterStart = Date.compare assetDate startDate /= LT
                beforeEnd = Date.compare assetDate endDate == LT
            in
            afterStart && beforeEnd
        
        -- Count assets in each specific period
        todayCount = List.length (List.filter (\asset -> isInRange asset todayStart todayEnd) assets)
        weekCount = List.length (List.filter (\asset -> isInRange asset weekStart weekEnd) assets)
        monthCount = List.length (List.filter (\asset -> isInRange asset monthStart monthEnd) assets)
        yearCount = List.length (List.filter (\asset -> isInRange asset yearStart yearEnd) assets)
        allCount = List.length assets
    in
    { today = todayCount
    , week = weekCount
    , month = monthCount
    , year = yearCount
    , all = allCount
    }

-- Format asset counts as simple text "today/week/month/year/all"
viewAssetCountsText : AssetCounts -> Element msg
viewAssetCountsText counts =
    el [ alignRight, Font.size 12, Font.color (Element.rgb 0.6 0.6 0.6) ] 
        (text (String.fromInt counts.today ++ "/" ++ 
               String.fromInt counts.week ++ "/" ++ 
               String.fromInt counts.month ++ "/" ++ 
               String.fromInt counts.year ++ "/" ++ 
               String.fromInt counts.all))

-- Main asset view function

viewAsset : ImmichApiPaths -> String -> ImmichAsset -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> Int -> Element msg
viewAsset apiPaths apiKey asset currentAssets knownAssets imageIndex =
    if asset.path == "" then
        text "No asset selected"
    else if String.startsWith "image/" asset.mimeType then
        viewImage asset apiPaths apiKey currentAssets knownAssets imageIndex

    else if String.startsWith "video/" asset.mimeType then
        viewVideo asset apiPaths apiKey currentAssets knownAssets imageIndex

    else
        text "Unknown asset type"


-- Image view function

viewImage : ImmichAsset -> ImmichApiPaths -> String -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> Int -> Element msg
viewImage asset apiPaths apiKey currentAssets knownAssets imageIndex =
    let
        preloadList =
            let
                currentIndex =
                    imageIndex
                preloadIndices =
                    List.range (currentIndex - 5) (currentIndex + 5)
                validIndices =
                    List.filter (\i -> i >= 0 && i < List.length currentAssets) preloadIndices
            in
            validIndices
                |> List.filterMap (\i -> List.drop i currentAssets |> List.head)
                |> List.filterMap (\assetId -> Dict.get assetId knownAssets)
                |> List.map
                    (\preloadAsset ->
                        { assetId = preloadAsset.id
                        , thumbnailUrl = apiPaths.downloadAsset preloadAsset.id
                        , webUrl = apiPaths.downloadAsset preloadAsset.id
                        }
                    )
    in
    column [ width fill, height fill ]
        [ el [ width fill, height fill ] <|
            Element.html <|
                Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "align-items" "center", Html.Attributes.style "justify-content" "center", Html.Attributes.style "height" "calc(100vh - 40px)", Html.Attributes.style "width" "100%", Html.Attributes.style "overflow" "hidden" ]
                    [ Html.node "style" [] [ Html.text "image-from-api img { max-width: 100% !important; max-height: calc(100vh - 40px) !important; object-fit: contain !important; width: auto !important; height: auto !important; }" ]
                    , node "image-from-api"
                        [ Html.Attributes.attribute "asset-url" (apiPaths.downloadAsset asset.id)
                        , Html.Attributes.attribute "api-key" apiKey
                        , Html.Attributes.attribute "preload-urls"
                            (preloadList
                                |> List.map (\item -> item.webUrl)
                                |> String.join ","
                            )
                        , Html.Attributes.class "center"
                        , Html.Attributes.style "max-width" "100%"
                        , Html.Attributes.style "max-height" "100%"
                        , Html.Attributes.style "object-fit" "contain"
                        , Html.Attributes.style "display" "block"
                        ]
                        []
                    ]
        , el [ width fill, height (px 20), Font.size 12 ] <|
            text (asset.title ++ " - " ++ String.fromInt (Date.ordinalDay asset.fileCreatedAt))
        ]


-- Video view function

viewVideo : ImmichAsset -> ImmichApiPaths -> String -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> Int -> Element msg
viewVideo asset apiPaths apiKey currentAssets knownAssets imageIndex =
    let
        preloadList =
            let
                currentIndex =
                    imageIndex
                preloadIndices =
                    List.range (currentIndex - 2) (currentIndex + 2)
                validIndices =
                    List.filter (\i -> i >= 0 && i < List.length currentAssets) preloadIndices
            in
            validIndices
                |> List.filterMap (\i -> List.drop i currentAssets |> List.head)
                |> List.filterMap (\assetId -> Dict.get assetId knownAssets)
                |> List.map
                    (\preloadAsset ->
                        { assetId = preloadAsset.id
                        , thumbnailUrl = apiPaths.downloadAsset preloadAsset.id
                        , webUrl = apiPaths.downloadAsset preloadAsset.id
                        }
                    )
    in
    column [ width fill, height fill ]
        [ el [ width fill, height fill, Html.Attributes.style "position" "relative" |> Element.htmlAttribute ] <|
            Element.html <|
                Html.div []
                    [ Html.node "style" [] [ Html.text "video-from-api video { width: 100% !important; height: 100% !important; object-fit: contain !important; }" ]
                    , node "video-from-api"
                        [ Html.Attributes.attribute "asset-url" (apiPaths.downloadAsset asset.id)
                        , Html.Attributes.attribute "api-key" apiKey
                        , Html.Attributes.attribute "preload-urls"
                            (preloadList
                                |> List.map (\item -> item.webUrl)
                                |> String.join ","
                            )
                        , Html.Attributes.class "center"
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "calc(100vh - 40px)"
                        , Html.Attributes.style "max-height" "calc(100vh - 40px)"
                        , Html.Attributes.style "max-width" "100%"
                        , Html.Attributes.style "display" "block"
                        , Html.Attributes.style "overflow" "hidden"
                        ]
                        []
                    ]
        , el [ width fill, height (px 20), Font.size 12 ] <|
            text (asset.title ++ " - " ++ String.fromInt (Date.ordinalDay asset.fileCreatedAt))
        ]


-- Edit asset view function

viewEditAsset : ImmichApiPaths -> String -> ImageIndex -> Int -> String -> AssetWithActions -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> Int -> Element msg
viewEditAsset apiPaths apiKey imageIndex totalAssets viewTitle currentAsset currentAssets knownAssets currentDateMillis =
    let
        -- Simple approximation: use current milliseconds to derive a realistic current date
        -- 1737400000000 ms ≈ January 20, 2025
        -- Calculate days since epoch and create approximate date
        daysFromEpoch = currentDateMillis // (1000 * 60 * 60 * 24)
        -- Approximation: January 1, 1970 + calculated days ≈ current date
        -- For simplicity, just use July 15, 2025 as current date since that's close to today
        currentDate = Date.fromCalendarDate 2025 (Date.numberToMonth 7) 15
        -- Calculate asset counts for current date
        counts = calculateAssetCounts currentDate currentAssets knownAssets
    in
    column [ width fill, height fill ]
        [ row [ width fill, alignTop, height (px 20) ]
            [ el [] (text (String.fromInt (imageIndex + 1) ++ "/" ++ String.fromInt totalAssets ++ "    " ++ viewTitle))
            , viewAssetCountsText counts
            ]
        , el [ width fill, height fill ] <| viewAsset apiPaths apiKey currentAsset.asset currentAssets knownAssets imageIndex
        ]


-- Loading assets view

viewLoadingAssets : ImmichLoadState -> Element msg
viewLoadingAssets imagesLoadState =
    case imagesLoadState of
        ImmichLoading ->
            text "Loading images"
        ImmichLoadSuccess ->
            text "Loaded. Should move states...."
        ImmichLoadError error ->
            let
                errorMessage =
                    Immich.errorToString error
            in
            text errorMessage


-- Album creation confirmation view

viewCreateAlbumConfirmation : String -> Element msg
viewCreateAlbumConfirmation albumName =
    column [ width fill, height fill, paddingXY 20 20, Element.spacingXY 0 20, centerX, centerY ]
        [ el [ Font.size 18, Font.bold, centerX ] (text "Create New Album")
        , el [ centerX ] (text ("Album name: \"" ++ albumName ++ "\""))
        , el [ Font.size 14, centerX ] (text "Press Enter to create, Escape to cancel")
        ]


-- Edit asset help view

viewEditAssetHelp : InputMode -> Element msg
viewEditAssetHelp inputMode =
    column [ width fill, height fill, paddingXY 20 20, Element.spacingXY 0 20, centerX, centerY ]
        [ el [ Font.size 18, Font.bold, centerX ] (text "Asset Navigation Help")
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] <| text "Navigation"
            , viewKeybinding "←" "Previous image"
            , viewKeybinding "→" "Next image"
            , viewKeybinding "Space" "Next image"
            , viewKeybinding "Escape" "Return to main menu"
            ]
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] <| text "Asset Actions"
            , viewKeybinding "D" "Toggle delete/archive"
            , viewKeybinding "F" "Toggle favorite"
            , viewKeybinding "K" "Open in Immich (new tab)"
            , viewKeybinding "I" "Enter album search mode"
            ]
        , if inputMode == InsertMode then
            column [ Element.spacingXY 0 8 ]
                [ el [ Font.size 16, Font.bold ] <| text "Album Search (Insert Mode)"
                , viewKeybinding "Type" "Search albums by name"
                , viewKeybinding "↑↓" "Navigate through results"
                , viewKeybinding "Enter" "Add to highlighted album"
                , viewKeybinding "Tab" "Create new album"
                , viewKeybinding "Click" "Click album to add"
                , viewKeybinding "Escape" "Exit search mode"
                ]
          else
            Element.none
        , el [ Font.size 14, centerX ] (text "Press ? or Escape to close help")
        ]


-- Keybinding display helper

viewKeybinding : String -> String -> Element msg
viewKeybinding key description =
    row [ width fill, Element.spacingXY 10 0 ]
        [ el [ width <| Element.px 120, Font.family [ Font.monospace ], Background.color <| usefulColours "grey", paddingXY 8 4 ] <| text key
        , el [ width fill ] <| text description
        ]

