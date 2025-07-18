module ViewAsset exposing
    ( TimeViewMode(..)
    , viewCreateAlbumConfirmation
    , viewEditAsset
    , viewEditAssetHelp
    , viewGridAssets
    , viewKeybinding
    , viewLoadingAssets
    )

import Date exposing (Date)
import Dict exposing (Dict)
import Element exposing (Element, alignRight, alignTop, centerX, centerY, column, el, fill, height, paddingXY, px, row, text, width)
import Element.Background as Background
import Element.Font as Font
import Html exposing (node)
import Html.Attributes
import Immich exposing (ImmichApiPaths, ImmichAsset, ImmichAssetId, ImmichLoadState(..))
import Time
import ViewAlbums exposing (AssetWithActions, InputMode(..), usefulColours)
import ViewGrid exposing (GridMsg, GridState)


type alias ImageIndex =
    Int



-- Helper types and functions for date-based asset counting


type TimeViewMode
    = Absolute
    | Relative


type alias AssetCounts =
    { today : Int
    , week : Int
    , month : Int
    , year : Int
    , rest : Int
    }



-- Calculate asset counts for different time periods with exclusive buckets


calculateAssetCounts : TimeViewMode -> Date -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> AssetCounts
calculateAssetCounts timeMode currentDate assetIds knownAssets =
    let
        assets =
            List.filterMap (\id -> Dict.get id knownAssets) assetIds

        -- Calculate date boundaries based on time mode
        dateBoundaries =
            case timeMode of
                Absolute ->
                    -- Calendar-based periods
                    { todayStart = currentDate
                    , todayEnd = Date.add Date.Days 1 currentDate
                    , weekStart = Date.add Date.Days -(Date.weekdayToNumber (Date.weekday currentDate) - 1) currentDate
                    , weekEnd = Date.add Date.Days 7 (Date.add Date.Days -(Date.weekdayToNumber (Date.weekday currentDate) - 1) currentDate)
                    , monthStart = Date.fromCalendarDate (Date.year currentDate) (Date.month currentDate) 1
                    , monthEnd = Date.add Date.Months 1 (Date.fromCalendarDate (Date.year currentDate) (Date.month currentDate) 1)
                    , yearStart = Date.fromCalendarDate (Date.year currentDate) (Date.numberToMonth 1) 1
                    , yearEnd = Date.add Date.Years 1 (Date.fromCalendarDate (Date.year currentDate) (Date.numberToMonth 1) 1)
                    }

                Relative ->
                    -- Duration-based periods from current time
                    { todayStart = currentDate
                    , todayEnd = Date.add Date.Days 1 currentDate
                    , weekStart = Date.add Date.Days -7 currentDate
                    , weekEnd = currentDate
                    , monthStart = Date.add Date.Days -30 currentDate
                    , monthEnd = currentDate
                    , yearStart = Date.add Date.Days -365 currentDate
                    , yearEnd = currentDate
                    }

        -- Helper function to check if asset is in date range
        isInRange asset startDate endDate =
            let
                assetDate =
                    asset.fileCreatedAt

                afterStart =
                    Date.compare assetDate startDate /= LT

                beforeEnd =
                    Date.compare assetDate endDate == LT
            in
            afterStart && beforeEnd

        -- Categorize assets into exclusive time buckets (priority: Today > Week > Month > Year)
        categorizedAssets =
            List.foldl
                (\asset acc ->
                    if isInRange asset dateBoundaries.todayStart dateBoundaries.todayEnd then
                        { acc | today = acc.today + 1 }

                    else if isInRange asset dateBoundaries.weekStart dateBoundaries.weekEnd then
                        { acc | week = acc.week + 1 }

                    else if isInRange asset dateBoundaries.monthStart dateBoundaries.monthEnd then
                        { acc | month = acc.month + 1 }

                    else if isInRange asset dateBoundaries.yearStart dateBoundaries.yearEnd then
                        { acc | year = acc.year + 1 }

                    else
                        { acc | rest = acc.rest + 1 }
                )
                { today = 0, week = 0, month = 0, year = 0, rest = 0 }
                assets
    in
    { today = categorizedAssets.today
    , week = categorizedAssets.week
    , month = categorizedAssets.month
    , year = categorizedAssets.year
    , rest = categorizedAssets.rest
    }



-- Format asset counts as simple text "today/week/month/year/all (A/R)"


viewAssetCountsText : AssetCounts -> TimeViewMode -> Element msg
viewAssetCountsText counts timeMode =
    let
        modeIndicator =
            case timeMode of
                Absolute ->
                    " (A)"

                Relative ->
                    " (R)"
    in
    el [ alignRight, Font.size 12, Font.color (Element.rgb 0.6 0.6 0.6) ]
        (text
            (String.fromInt counts.today
                ++ "/"
                ++ String.fromInt counts.week
                ++ "/"
                ++ String.fromInt counts.month
                ++ "/"
                ++ String.fromInt counts.year
                ++ "/"
                ++ String.fromInt counts.rest
                ++ modeIndicator
            )
        )



-- Main asset view function


viewAsset : ImmichApiPaths -> String -> AssetWithActions -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> Int -> Element msg
viewAsset apiPaths apiKey assetWithActions currentAssets knownAssets imageIndex =
    let
        asset =
            assetWithActions.asset
    in
    if asset.path == "" then
        text "No asset selected"

    else if String.startsWith "image/" asset.mimeType then
        viewImage asset apiPaths apiKey currentAssets knownAssets imageIndex

    else if String.startsWith "video/" asset.mimeType then
        if assetWithActions.isVideoLoaded then
            viewVideo asset apiPaths apiKey currentAssets knownAssets imageIndex

        else
            let
                durationSeconds =
                    case asset.duration of
                        Just durStr ->
                            Immich.parseDurationToSeconds durStr

                        Nothing ->
                            Nothing

                -- Show confirmation dialog for videos longer than 5 minutes (300 seconds)
                shouldShowConfirmation =
                    case durationSeconds of
                        Just seconds ->
                            seconds > 300

                        Nothing ->
                            False
            in
            if shouldShowConfirmation then
                viewVideoConfirm apiPaths apiKey asset durationSeconds

            else
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


viewEditAsset : ImmichApiPaths -> String -> ImageIndex -> Int -> String -> AssetWithActions -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> Int -> TimeViewMode -> Element msg
viewEditAsset apiPaths apiKey imageIndex totalAssets viewTitle currentAsset currentAssets knownAssets currentDateMillis timeMode =
    let
        -- Convert milliseconds to proper Date
        currentDate =
            Date.fromPosix Time.utc (Time.millisToPosix currentDateMillis)

        -- Calculate asset counts for current date
        counts =
            calculateAssetCounts timeMode currentDate currentAssets knownAssets
    in
    column [ width fill, height fill ]
        [ row [ width fill, alignTop, height (px 20) ]
            [ el [] (text (String.fromInt (imageIndex + 1) ++ "/" ++ String.fromInt totalAssets ++ "    " ++ viewTitle))
            , viewAssetCountsText counts timeMode
            ]
        , el [ width fill, height fill ] <| viewAsset apiPaths apiKey currentAsset currentAssets knownAssets imageIndex
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
            , viewKeybinding "T" "Toggle time view (Absolute/Relative)"
            , viewKeybinding "G" "Switch to grid view"
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



-- Grid view function


viewGridAssets : ImmichApiPaths -> String -> GridState -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> (GridMsg -> msg) -> Element msg
viewGridAssets apiPaths apiKey gridState currentAssets knownAssets toMsg =
    let
        assets =
            List.filterMap (\id -> Dict.get id knownAssets) currentAssets
    in
    ViewGrid.viewGrid apiPaths apiKey gridState assets toMsg



-- Video confirmation dialog


viewVideoConfirm : ImmichApiPaths -> String -> ImmichAsset -> Maybe Int -> Element msg
viewVideoConfirm apiPaths apiKey asset duration =
    let
        durationText =
            case duration of
                Just seconds ->
                    formatDuration seconds

                Nothing ->
                    "unknown length"

        formatDuration : Int -> String
        formatDuration seconds =
            let
                minutes =
                    seconds // 60

                remainingSeconds =
                    remainderBy 60 seconds

                hours =
                    minutes // 60

                remainingMinutes =
                    remainderBy 60 minutes
            in
            if hours > 0 then
                String.fromInt hours ++ "h " ++ String.fromInt remainingMinutes ++ "m " ++ String.fromInt remainingSeconds ++ "s"

            else if minutes > 0 then
                String.fromInt minutes ++ "m " ++ String.fromInt remainingSeconds ++ "s"

            else
                String.fromInt remainingSeconds ++ "s"
    in
    column [ width fill, height fill, paddingXY 20 20, Element.spacingXY 0 20, centerX, centerY ]
        [ el [ Font.size 18, Font.bold, centerX ] (text "Load Video?")
        , el [ centerX ] (text ("Video: " ++ asset.title))
        , el [ centerX ] (text ("Duration: " ++ durationText))
        , el [ Font.size 14, centerX, paddingXY 0 10 ] (text "This video is longer than 5 minutes and may take time to load.")
        , column [ Element.spacingXY 0 8, centerX ]
            [ viewKeybinding "L" "Load video"
            , viewKeybinding "Escape" "Cancel and return to previous view"
            , viewKeybinding "← / →" "Navigate to previous/next asset"
            ]
        , case asset.thumbhash of
            Just thumbhash ->
                el [ centerX, width (px 200), height (px 150) ] <|
                    Element.html <|
                        Html.node "thumbhash-image"
                            [ Html.Attributes.attribute "thumbhash" thumbhash
                            , Html.Attributes.attribute "asset-url" (apiPaths.downloadAsset asset.id)
                            , Html.Attributes.attribute "api-key" apiKey
                            , Html.Attributes.style "width" "200px"
                            , Html.Attributes.style "height" "150px"
                            ]
                            []

            Nothing ->
                el [ centerX, width (px 200), height (px 150), Background.color (Element.rgb 0.9 0.9 0.9) ] <|
                    el [ centerX, centerY ] (text "No preview available")
        ]
