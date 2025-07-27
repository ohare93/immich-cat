module ViewAsset exposing
    ( TimeViewMode(..)
    , viewCreateAlbumConfirmation
    , viewEditAsset
    , viewEditAssetHelp
    , viewGridAssets
    , viewLoadingAssets
    )

import Date exposing (Date)
import Dict exposing (Dict)
import Element exposing (Element, alignRight, alignTop, centerX, centerY, column, el, fill, height, paddingXY, px, row, text, width)
import Element.Background as Background
import Element.Font as Font
import HelpText exposing (ViewContext(..), viewContextHelp, viewKeybinding)
import Html exposing (node)
import Html.Attributes
import Immich exposing (ImmichApiPaths, ImmichAsset, ImmichAssetId, ImmichLoadState(..))
import Time
import ViewAlbums exposing (AssetWithActions, InputMode(..))
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



-- Format time since upload as compact string like "4d", "1m 4d", or "1y 11m 30d"


formatTimeSinceUpload : Date -> Date -> String
formatTimeSinceUpload currentDate uploadDate =
    let
        totalDays =
            Date.diff Date.Days uploadDate currentDate

        years =
            totalDays // 365

        remainingDaysAfterYears =
            remainderBy 365 totalDays

        months =
            remainingDaysAfterYears // 30

        days =
            remainderBy 30 remainingDaysAfterYears
    in
    if years > 0 then
        if months > 0 then
            if days > 0 then
                String.fromInt years ++ "y " ++ String.fromInt months ++ "m " ++ String.fromInt days ++ "d"

            else
                String.fromInt years ++ "y " ++ String.fromInt months ++ "m"

        else if days > 0 then
            String.fromInt years ++ "y " ++ String.fromInt days ++ "d"

        else
            String.fromInt years ++ "y"

    else if months > 0 then
        if days > 0 then
            String.fromInt months ++ "m " ++ String.fromInt days ++ "d"

        else
            String.fromInt months ++ "m"

    else
        String.fromInt days ++ "d"



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
        ]



-- Scroll view for very large images


viewScrollAsset : ImmichApiPaths -> String -> AssetWithActions -> { scrollX : Int, scrollY : Int } -> Element msg
viewScrollAsset apiPaths apiKey assetWithActions scrollState =
    let
        asset =
            assetWithActions.asset
    in
    if asset.path == "" then
        text "No asset selected"

    else if String.startsWith "image/" asset.mimeType then
        viewScrollImage asset apiPaths apiKey scrollState

    else if String.startsWith "video/" asset.mimeType then
        if assetWithActions.isVideoLoaded then
            viewScrollVideo asset apiPaths apiKey scrollState

        else
            text "Video not loaded for scroll view"

    else
        text "Unknown asset type for scroll view"



-- Scroll image view with full-width display and scrolling


viewScrollImage : ImmichAsset -> ImmichApiPaths -> String -> { scrollX : Int, scrollY : Int } -> Element msg
viewScrollImage asset apiPaths apiKey scrollState =
    column [ width fill, height fill ]
        [ el [ width fill, height fill ] <|
            Element.html <|
                Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "align-items" "flex-start"
                    , Html.Attributes.style "justify-content" "flex-start"
                    , Html.Attributes.style "height" "calc(100vh - 40px)"
                    , Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "overflow" "hidden"
                    , Html.Attributes.style "position" "relative"
                    ]
                    [ Html.node "style" [] [ Html.text ("image-from-api.scroll-view img { max-width: none !important; max-height: none !important; width: 100% !important; height: auto !important; transform: translate(" ++ String.fromInt scrollState.scrollX ++ "px, " ++ String.fromInt scrollState.scrollY ++ "px) !important; position: absolute !important; top: 0 !important; left: 0 !important; }") ]
                    , Html.node "image-from-api"
                        [ Html.Attributes.attribute "asset-url" (apiPaths.downloadAsset asset.id)
                        , Html.Attributes.attribute "api-key" apiKey
                        , Html.Attributes.class "scroll-view"
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "100%"
                        , Html.Attributes.style "display" "block"
                        ]
                        []
                    ]
        ]



-- Scroll video view with full-width display and scrolling


viewScrollVideo : ImmichAsset -> ImmichApiPaths -> String -> { scrollX : Int, scrollY : Int } -> Element msg
viewScrollVideo asset apiPaths apiKey scrollState =
    column [ width fill, height fill ]
        [ el [ width fill, height fill ] <|
            Element.html <|
                Html.div
                    [ Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "height" "calc(100vh - 40px)"
                    , Html.Attributes.style "position" "relative"
                    , Html.Attributes.style "overflow" "hidden"
                    ]
                    [ Html.node "style" [] [ Html.text ("video-from-api.scroll-view video { max-width: none !important; max-height: none !important; width: 100% !important; height: auto !important; transform: translate(" ++ String.fromInt scrollState.scrollX ++ "px, " ++ String.fromInt scrollState.scrollY ++ "px) !important; position: absolute !important; top: 0 !important; left: 0 !important; }") ]
                    , Html.node "video-from-api"
                        [ Html.Attributes.attribute "asset-url" (apiPaths.downloadAsset asset.id)
                        , Html.Attributes.attribute "api-key" apiKey
                        , Html.Attributes.class "scroll-view"
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "calc(100vh - 40px)"
                        , Html.Attributes.style "display" "block"
                        , Html.Attributes.style "overflow" "hidden"
                        ]
                        []
                    ]
        ]



-- Edit asset view function


viewEditAsset : ImmichApiPaths -> String -> ImageIndex -> Int -> String -> AssetWithActions -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> Int -> TimeViewMode -> InputMode -> Element msg
viewEditAsset apiPaths apiKey imageIndex totalAssets viewTitle currentAsset currentAssets knownAssets currentDateMillis timeMode inputMode =
    let
        -- Convert milliseconds to proper Date
        currentDate =
            Date.fromPosix Time.utc (Time.millisToPosix currentDateMillis)

        -- Calculate asset counts for current date
        counts =
            calculateAssetCounts timeMode currentDate currentAssets knownAssets

        -- Format time since upload for current asset
        timeSinceUpload =
            formatTimeSinceUpload currentDate currentAsset.asset.fileCreatedAt
    in
    case inputMode of
        ScrollViewMode scrollState ->
            -- Scroll view mode: show scroll view with minimal header
            column [ width fill, height fill ]
                [ row [ width fill, alignTop, height (px 20) ]
                    [ el [] (text (String.fromInt (imageIndex + 1) ++ "/" ++ String.fromInt totalAssets ++ "    " ++ viewTitle ++ " (Scroll View)"))
                    , el [ alignRight, Font.size 12, Font.color (Element.rgb 0.7 0.7 0.7) ] (text (timeSinceUpload ++ " - "))
                    , viewAssetCountsText counts timeMode
                    ]
                , el [ width fill, height fill ] <| viewScrollAsset apiPaths apiKey currentAsset scrollState
                ]

        _ ->
            -- Normal view modes
            column [ width fill, height fill ]
                [ row [ width fill, alignTop, height (px 20) ]
                    [ el [] (text (String.fromInt (imageIndex + 1) ++ "/" ++ String.fromInt totalAssets ++ "    " ++ viewTitle))
                    , el [ alignRight, Font.size 12, Font.color (Element.rgb 0.7 0.7 0.7) ] (text (timeSinceUpload ++ " - "))
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
            column [ width fill, height fill, paddingXY 20 20, Element.spacingXY 0 20, centerX, centerY ]
                [ el [ Font.size 20, Font.bold, Font.color (Element.rgb 0.8 0.2 0.2), centerX ] (text "⚠️ Connection Error")
                , el [ Font.size 16, centerX ] (text errorMessage)
                , column [ Element.spacingXY 0 10, centerX ]
                    [ el [ Font.size 14 ] (text "Possible solutions:")
                    , el [ Font.size 13 ] (text "• Check your internet connection")
                    , el [ Font.size 13 ] (text "• Verify Immich server URL in settings")
                    , el [ Font.size 13 ] (text "• Check API key configuration")
                    , el [ Font.size 13 ] (text "• Ensure Immich server is running")
                    ]
                , column [ Element.spacingXY 0 8, centerX ]
                    [ el [ Font.size 14, Font.bold ] (text "Navigation:")
                    , el [ Font.size 13 ] (text "Press Escape to return to main menu")
                    , el [ Font.size 13 ] (text "Press 'g' to go to settings")
                    ]
                ]



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
    column [ width fill, height fill, paddingXY 20 20, centerX, centerY ]
        [ viewContextHelp (AssetViewContext inputMode)
        , el [ Font.size 14, centerX, Element.paddingXY 0 10 ] (text "Press ? or Escape to close help")
        ]



-- Note: viewKeybinding is now imported from HelpText module for consistency
-- Grid view function


viewGridAssets : ImmichApiPaths -> String -> GridState -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> Bool -> Bool -> (GridMsg -> msg) -> Element msg
viewGridAssets apiPaths apiKey gridState currentAssets knownAssets hasMorePages isLoadingMore toMsg =
    let
        assets =
            List.filterMap (\id -> Dict.get id knownAssets) currentAssets
    in
    ViewGrid.viewGrid apiPaths apiKey gridState assets hasMorePages isLoadingMore toMsg



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
