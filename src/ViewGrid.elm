module ViewGrid exposing
    ( GridItem
    , GridMsg(..)
    , GridState
    , getSelectedAsset
    , initGridState
    , updateGridState
    , viewGrid
    )

import Dict exposing (Dict)
import Element exposing (Element, column, el, fill, height, html, paddingXY, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Immich exposing (ImmichApiPaths, ImmichAsset, ImmichAssetId)
import Json.Decode
import Json.Encode
import ViewAlbums exposing (usefulColours)



-- TYPES


type alias GridState =
    { selectedAssets : Dict ImmichAssetId Bool
    , focusedAssetId : Maybe ImmichAssetId
    , gridColumns : Int
    , multiSelectMode : Bool
    , screenWidth : Int
    , screenHeight : Int
    , scrollTop : Float
    , itemHeight : Float
    , bufferSize : Int
    , programmaticScroll : Bool -- True when scroll was triggered programmatically
    }


type GridMsg
    = GridItemClicked ImmichAssetId
    | GridItemFocused ImmichAssetId
    | GridKeyPressed String
    | GridResized Int Int
    | GridToggleMultiSelect
    | GridSelectAll
    | GridClearSelection
    | GridBulkFavorite Bool
    | GridBulkArchive Bool
    | GridBulkAddToAlbum
    | GridBulkRemoveFromAlbum
    | GridScrolled Float


type alias GridItem =
    { asset : ImmichAsset
    , isSelected : Bool
    , isFocused : Bool
    , index : Int
    }



-- INIT


initGridState : Int -> Int -> GridState
initGridState screenWidth screenHeight =
    let
        -- Calculate item height based on grid columns and available width
        columns =
            calculateGridColumns screenWidth

        itemWidth =
            (screenWidth - 20 - (8 * (columns - 1))) // columns

        -- Account for padding and gaps
        itemHeight =
            toFloat itemWidth

        -- Square aspect ratio
    in
    { selectedAssets = Dict.empty
    , focusedAssetId = Nothing
    , gridColumns = columns
    , multiSelectMode = False
    , screenWidth = screenWidth
    , screenHeight = screenHeight
    , scrollTop = 0.0
    , itemHeight = itemHeight
    , bufferSize = 5 -- Number of extra rows to render above/below viewport
    , programmaticScroll = False
    }



-- UPDATE


updateGridState : GridMsg -> GridState -> List ImmichAsset -> GridState
updateGridState msg state assets =
    case msg of
        GridItemClicked assetId ->
            if state.multiSelectMode then
                { state | selectedAssets = toggleAssetSelection assetId state.selectedAssets }

            else
                { state | selectedAssets = Dict.singleton assetId True, focusedAssetId = Just assetId }

        GridItemFocused assetId ->
            { state | focusedAssetId = Just assetId }

        GridKeyPressed key ->
            handleGridKeyPress key state assets

        GridResized width height ->
            let
                newColumns =
                    calculateGridColumns width

                itemWidth =
                    (width - 20 - (8 * (newColumns - 1))) // newColumns

                newItemHeight =
                    toFloat itemWidth
            in
            { state
                | screenWidth = width
                , screenHeight = height
                , gridColumns = newColumns
                , itemHeight = newItemHeight
            }

        GridToggleMultiSelect ->
            { state | multiSelectMode = not state.multiSelectMode }

        GridSelectAll ->
            { state | selectedAssets = Dict.fromList (List.map (\asset -> ( asset.id, True )) assets) }

        GridClearSelection ->
            { state | selectedAssets = Dict.empty }

        GridBulkFavorite _ ->
            state

        GridBulkArchive _ ->
            state

        GridBulkAddToAlbum ->
            state

        GridBulkRemoveFromAlbum ->
            state

        GridScrolled scrollTop ->
            { state | scrollTop = scrollTop, programmaticScroll = False }



-- HELPER FUNCTIONS


calculateGridColumns : Int -> Int
calculateGridColumns screenWidth =
    let
        minItemWidth =
            150

        maxColumns =
            8

        calculatedColumns =
            screenWidth // minItemWidth
    in
    max 2 (min maxColumns calculatedColumns)


{-| Calculate which items are visible based on scroll position and viewport size
-}
calculateVisibleRange : GridState -> Int -> { startIndex : Int, endIndex : Int, startRow : Int, endRow : Int }
calculateVisibleRange state totalItems =
    let
        -- Calculate row height (item height + gap)
        rowHeight =
            state.itemHeight + 8

        -- 8px gap between rows
        -- Calculate total rows needed
        totalRows =
            (totalItems + state.gridColumns - 1) // state.gridColumns

        -- Calculate visible rows
        viewportHeight =
            toFloat state.screenHeight

        startRow =
            max 0 (floor (state.scrollTop / rowHeight) - state.bufferSize)

        endRow =
            min totalRows (ceiling ((state.scrollTop + viewportHeight) / rowHeight) + state.bufferSize)

        -- Convert rows to item indices
        startIndex =
            startRow * state.gridColumns

        endIndex =
            min totalItems ((endRow + 1) * state.gridColumns)
    in
    { startIndex = startIndex
    , endIndex = endIndex
    , startRow = startRow
    , endRow = endRow
    }


{-| Calculate the total height needed for all items
-}
calculateTotalHeight : GridState -> Int -> Float
calculateTotalHeight state totalItems =
    let
        totalRows =
            (totalItems + state.gridColumns - 1) // state.gridColumns

        rowHeight =
            state.itemHeight + 8

        -- 8px gap between rows
    in
    toFloat totalRows * rowHeight


{-| Calculate scroll position needed to show a specific item index
-}
calculateScrollToItem : GridState -> Int -> Float
calculateScrollToItem state itemIndex =
    let
        rowIndex =
            itemIndex // state.gridColumns

        rowHeight =
            state.itemHeight + 8

        itemTop =
            toFloat rowIndex * rowHeight

        itemBottom =
            itemTop + state.itemHeight

        viewportHeight =
            toFloat state.screenHeight

        currentScrollTop =
            state.scrollTop

        currentScrollBottom =
            currentScrollTop + viewportHeight

        -- Add some padding for better visibility
        padding =
            state.itemHeight * 0.2
    in
    if itemTop < currentScrollTop + padding then
        -- Item is above viewport, scroll up
        max 0 (itemTop - padding)

    else if itemBottom > currentScrollBottom - padding then
        -- Item is below viewport, scroll down
        itemBottom - viewportHeight + padding

    else
        -- Item is already visible, no scroll needed
        state.scrollTop


{-| Decode scroll position from scroll event
-}
decodeScrollTop : String -> Json.Decode.Decoder Float
decodeScrollTop _ =
    Json.Decode.at [ "target", "scrollTop" ] Json.Decode.float


toggleAssetSelection : ImmichAssetId -> Dict ImmichAssetId Bool -> Dict ImmichAssetId Bool
toggleAssetSelection assetId selectedAssets =
    if Dict.member assetId selectedAssets then
        Dict.remove assetId selectedAssets

    else
        Dict.insert assetId True selectedAssets


getSelectedAsset : GridState -> Maybe ImmichAssetId
getSelectedAsset state =
    case Dict.keys state.selectedAssets of
        [ singleAsset ] ->
            Just singleAsset

        _ ->
            state.focusedAssetId


moveSelection : String -> GridState -> List ImmichAsset -> GridState
moveSelection direction state assets =
    let
        assetIds =
            List.map .id assets

        currentIndex =
            case state.focusedAssetId of
                Nothing ->
                    0

                Just assetId ->
                    List.indexedMap
                        (\i id ->
                            if id == assetId then
                                Just i

                            else
                                Nothing
                        )
                        assetIds
                        |> List.filterMap identity
                        |> List.head
                        |> Maybe.withDefault 0

        newIndex =
            case direction of
                "ArrowUp" ->
                    max 0 (currentIndex - state.gridColumns)

                "ArrowDown" ->
                    min (List.length assets - 1) (currentIndex + state.gridColumns)

                "ArrowLeft" ->
                    max 0 (currentIndex - 1)

                "ArrowRight" ->
                    min (List.length assets - 1) (currentIndex + 1)

                "Home" ->
                    0

                "End" ->
                    List.length assets - 1

                _ ->
                    currentIndex

        newFocusedAssetId =
            List.drop newIndex assetIds
                |> List.head

        -- Calculate new scroll position to keep focused item visible
        newScrollTop =
            if newIndex /= currentIndex then
                calculateScrollToItem state newIndex

            else
                state.scrollTop

        -- Check if scroll position actually changed
        scrollChanged =
            newScrollTop /= state.scrollTop
    in
    { state
        | focusedAssetId = newFocusedAssetId
        , scrollTop = newScrollTop
        , programmaticScroll = scrollChanged
    }


toggleSelection : GridState -> GridState
toggleSelection state =
    case state.focusedAssetId of
        Nothing ->
            state

        Just assetId ->
            { state | selectedAssets = toggleAssetSelection assetId state.selectedAssets }



-- KEYBOARD HANDLING


handleGridKeyPress : String -> GridState -> List ImmichAsset -> GridState
handleGridKeyPress key state assets =
    case key of
        "ArrowUp" ->
            moveSelection "ArrowUp" state assets

        "ArrowDown" ->
            moveSelection "ArrowDown" state assets

        "ArrowLeft" ->
            moveSelection "ArrowLeft" state assets

        "ArrowRight" ->
            moveSelection "ArrowRight" state assets

        "Home" ->
            moveSelection "Home" state assets

        "End" ->
            moveSelection "End" state assets

        " " ->
            toggleSelection state

        "Tab" ->
            { state | multiSelectMode = not state.multiSelectMode }

        "a" ->
            if state.multiSelectMode then
                { state | selectedAssets = Dict.fromList (List.map (\asset -> ( asset.id, True )) assets) }

            else
                state

        "c" ->
            if state.multiSelectMode then
                { state | selectedAssets = Dict.empty }

            else
                state

        _ ->
            state



-- VIEW


viewGrid : ImmichApiPaths -> String -> GridState -> List ImmichAsset -> (GridMsg -> msg) -> Element msg
viewGrid apiPaths apiKey state assets toMsg =
    let
        gridItems =
            List.indexedMap (createGridItem state) assets

        headerInfo =
            let
                selectedCount =
                    Dict.size state.selectedAssets

                totalCount =
                    List.length assets

                modeText =
                    if state.multiSelectMode then
                        "Multi-select"

                    else
                        "Single-select"

                bulkActionsRow =
                    if selectedCount > 0 then
                        row [ spacing 10, paddingXY 0 5 ]
                            [ viewBulkActionButton "Favorite" (toMsg (GridBulkFavorite True))
                            , viewBulkActionButton "Unfavorite" (toMsg (GridBulkFavorite False))
                            , viewBulkActionButton "Archive" (toMsg (GridBulkArchive True))
                            , viewBulkActionButton "Unarchive" (toMsg (GridBulkArchive False))
                            , viewBulkActionButton "Add to Album" (toMsg GridBulkAddToAlbum)
                            , viewBulkActionButton "Remove from Album" (toMsg GridBulkRemoveFromAlbum)
                            ]

                    else
                        Element.none
            in
            column [ width fill, spacing 5 ]
                [ row [ width fill, paddingXY 10 5, spacing 20 ]
                    [ el [ Font.size 14 ] (text (modeText ++ " mode"))
                    , el [ Font.size 14 ] (text (String.fromInt selectedCount ++ " of " ++ String.fromInt totalCount ++ " selected"))
                    , el [ Font.size 12, Font.color (usefulColours "darkgrey") ] (text "Press Space to select, Enter to view, Tab for multi-select")
                    ]
                , bulkActionsRow
                ]
    in
    column [ width fill, height fill, spacing 5 ]
        [ headerInfo
        , el [ width fill, height fill ] (viewGridItems apiPaths apiKey state gridItems toMsg)
        ]


viewBulkActionButton : String -> msg -> Element msg
viewBulkActionButton label onPress =
    Input.button
        [ Background.color (usefulColours "lightblue")
        , Font.color (usefulColours "white")
        , paddingXY 12 6
        , Border.rounded 4
        , Font.size 12
        ]
        { onPress = Just onPress
        , label = text label
        }


createGridItem : GridState -> Int -> ImmichAsset -> GridItem
createGridItem state index asset =
    { asset = asset
    , isSelected = Dict.member asset.id state.selectedAssets
    , isFocused = state.focusedAssetId == Just asset.id
    , index = index
    }


viewGridItems : ImmichApiPaths -> String -> GridState -> List GridItem -> (GridMsg -> msg) -> Element msg
viewGridItems apiPaths apiKey state gridItems toMsg =
    let
        totalItems =
            List.length gridItems

        visibleRange =
            calculateVisibleRange state totalItems

        totalHeight =
            calculateTotalHeight state totalItems

        -- Extract only visible items
        visibleItems =
            gridItems
                |> List.drop visibleRange.startIndex
                |> List.take (visibleRange.endIndex - visibleRange.startIndex)

        -- Calculate offset for visible items to maintain scroll position
        rowHeight =
            state.itemHeight + 8

        topOffset =
            toFloat visibleRange.startRow * rowHeight

        -- Container styles for virtual scrolling
        containerStyle =
            [ Html.Attributes.style "position" "relative"
            , Html.Attributes.style "height" "100%"
            , Html.Attributes.style "overflow-y" "auto"
            , Html.Events.on "scroll" (Html.Events.targetValue |> Json.Decode.andThen decodeScrollTop |> Json.Decode.map (toMsg << GridScrolled))
            ]
                ++ -- Only set scrollTop property for programmatic scrolls to avoid feedback loops
                   (if state.programmaticScroll then
                        [ Html.Attributes.property "scrollTop" (Json.Encode.float state.scrollTop) ]

                    else
                        []
                   )

        -- Inner container with total height to maintain scrollbar
        innerContainerStyle =
            [ Html.Attributes.style "height" (String.fromFloat totalHeight ++ "px")
            , Html.Attributes.style "position" "relative"
            ]

        -- Grid styles for visible items
        gridStyle =
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns" ("repeat(" ++ String.fromInt state.gridColumns ++ ", 1fr)")
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "padding" "10px"
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" (String.fromFloat topOffset ++ "px")
            , Html.Attributes.style "left" "0"
            , Html.Attributes.style "right" "0"
            ]

        visibleItemsHtml =
            List.map (viewGridItem apiPaths apiKey toMsg) visibleItems
    in
    html
        (Html.div containerStyle
            [ Html.div innerContainerStyle
                [ Html.div gridStyle visibleItemsHtml ]
            ]
        )


viewGridItem : ImmichApiPaths -> String -> (GridMsg -> msg) -> GridItem -> Html msg
viewGridItem apiPaths apiKey toMsg item =
    let
        containerStyle =
            [ Html.Attributes.style "position" "relative"
            , Html.Attributes.style "aspect-ratio" "1"
            , Html.Attributes.style "border-radius" "8px"
            , Html.Attributes.style "overflow" "hidden"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "transition" "all 0.2s ease"
            , Html.Attributes.style "border"
                (if item.isFocused then
                    "3px solid #4A90E2"

                 else if item.isSelected then
                    "2px solid #50C878"

                 else
                    "1px solid #ccc"
                )
            , Html.Attributes.style "box-shadow"
                (if item.isFocused then
                    "0 0 10px rgba(74, 144, 226, 0.5)"

                 else if item.isSelected then
                    "0 0 5px rgba(80, 200, 120, 0.5)"

                 else
                    "0 2px 4px rgba(0,0,0,0.1)"
                )
            ]

        overlayStyle =
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "0"
            , Html.Attributes.style "left" "0"
            , Html.Attributes.style "right" "0"
            , Html.Attributes.style "bottom" "0"
            , Html.Attributes.style "background"
                (if item.isSelected then
                    "rgba(80, 200, 120, 0.3)"

                 else
                    "transparent"
                )
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "align-items" "flex-end"
            , Html.Attributes.style "justify-content" "center"
            , Html.Attributes.style "padding" "5px"
            , Html.Attributes.style "pointer-events" "none"
            ]

        metadataStyle =
            [ Html.Attributes.style "background" "rgba(0,0,0,0.7)"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "padding" "2px 5px"
            , Html.Attributes.style "border-radius" "3px"
            , Html.Attributes.style "font-size" "11px"
            , Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "max-width" "90%"
            , Html.Attributes.style "overflow" "hidden"
            , Html.Attributes.style "text-overflow" "ellipsis"
            , Html.Attributes.style "white-space" "nowrap"
            ]

        selectionIndicatorStyle =
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "5px"
            , Html.Attributes.style "right" "5px"
            , Html.Attributes.style "width" "20px"
            , Html.Attributes.style "height" "20px"
            , Html.Attributes.style "border-radius" "50%"
            , Html.Attributes.style "background"
                (if item.isSelected then
                    "#50C878"

                 else
                    "rgba(255,255,255,0.8)"
                )
            , Html.Attributes.style "border" "2px solid white"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "justify-content" "center"
            , Html.Attributes.style "font-size" "12px"
            , Html.Attributes.style "color"
                (if item.isSelected then
                    "white"

                 else
                    "#666"
                )
            ]

        thumbhashComponent =
            case item.asset.thumbhash of
                Just hash ->
                    Html.node "thumbhash-image"
                        [ Html.Attributes.attribute "thumbhash" hash
                        , Html.Attributes.attribute "asset-url" (apiPaths.downloadAsset item.asset.id)
                        , Html.Attributes.attribute "api-key" apiKey
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "100%"
                        , Html.Attributes.style "object-fit" "cover"
                        ]
                        []

                Nothing ->
                    Html.div
                        [ Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "100%"
                        , Html.Attributes.style "background" "#f0f0f0"
                        , Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "align-items" "center"
                        , Html.Attributes.style "justify-content" "center"
                        , Html.Attributes.style "color" "#999"
                        ]
                        [ Html.text "No thumbnail" ]
    in
    Html.div
        (containerStyle
            ++ [ Html.Events.onClick (toMsg (GridItemClicked item.asset.id))
               , Html.Events.onFocus (toMsg (GridItemFocused item.asset.id))
               , Html.Attributes.tabindex 0
               ]
        )
        [ thumbhashComponent
        , Html.div overlayStyle
            [ Html.div metadataStyle [ Html.text item.asset.title ]
            ]
        , Html.div selectionIndicatorStyle
            [ Html.text
                (if item.isSelected then
                    "✓"

                 else
                    String.fromInt (item.index + 1)
                )
            ]
        ]
