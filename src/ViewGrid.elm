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
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Immich exposing (ImmichApiPaths, ImmichAsset, ImmichAssetId)
import ViewAlbums exposing (usefulColours)



-- TYPES


type alias GridState =
    { selectedAssets : Dict ImmichAssetId Bool
    , focusedAssetId : Maybe ImmichAssetId
    , gridColumns : Int
    , multiSelectMode : Bool
    , screenWidth : Int
    , screenHeight : Int
    }


type GridMsg
    = GridItemClicked ImmichAssetId
    | GridItemFocused ImmichAssetId
    | GridKeyPressed String
    | GridResized Int Int
    | GridToggleMultiSelect
    | GridSelectAll
    | GridClearSelection


type alias GridItem =
    { asset : ImmichAsset
    , isSelected : Bool
    , isFocused : Bool
    , index : Int
    }



-- INIT


initGridState : Int -> Int -> GridState
initGridState screenWidth screenHeight =
    { selectedAssets = Dict.empty
    , focusedAssetId = Nothing
    , gridColumns = calculateGridColumns screenWidth
    , multiSelectMode = False
    , screenWidth = screenWidth
    , screenHeight = screenHeight
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
            { state
                | screenWidth = width
                , screenHeight = height
                , gridColumns = calculateGridColumns width
            }

        GridToggleMultiSelect ->
            { state | multiSelectMode = not state.multiSelectMode }

        GridSelectAll ->
            { state | selectedAssets = Dict.fromList (List.map (\asset -> ( asset.id, True )) assets) }

        GridClearSelection ->
            { state | selectedAssets = Dict.empty }



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
    in
    { state | focusedAssetId = newFocusedAssetId }


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
            in
            row [ width fill, paddingXY 10 5, spacing 20 ]
                [ el [ Font.size 14 ] (text (modeText ++ " mode"))
                , el [ Font.size 14 ] (text (String.fromInt selectedCount ++ " of " ++ String.fromInt totalCount ++ " selected"))
                , el [ Font.size 12, Font.color (usefulColours "darkgrey") ] (text "Press Space to select, Enter to view")
                ]
    in
    column [ width fill, height fill, spacing 5 ]
        [ headerInfo
        , el [ width fill, height fill ] (viewGridItems apiPaths apiKey state gridItems toMsg)
        ]


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
        gridStyle =
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns" ("repeat(" ++ String.fromInt state.gridColumns ++ ", 1fr)")
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "padding" "10px"
            , Html.Attributes.style "height" "100%"
            , Html.Attributes.style "overflow-y" "auto"
            ]

        gridItemsHtml =
            List.map (viewGridItem apiPaths apiKey toMsg) gridItems
    in
    html (Html.div gridStyle gridItemsHtml)


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
