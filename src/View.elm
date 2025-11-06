module View exposing
    ( createDetailedViewTitle
    , isInInputMode
    , view
    , viewAssetState
    , viewInputMode
    , viewMainWindow
    , viewMenuState
    , viewPaginationStatus
    , viewWithInputBottomBar
    )

import Dict
import Element exposing (Element, alignRight, alignTop, clipY, column, el, fill, fillPortion, height, minimum, paddingXY, px, row, text, width)
import Element.Background as Background
import Element.Font as Font
import HelpText exposing (AlbumBrowseState(..), viewContextHelp)
import Html exposing (Html)
import Immich exposing (CategorisationFilter(..), ImageOrder(..), MediaTypeFilter(..))
import Menus
import Theme exposing (getBackgroundColor, getHighlightColor, getKeybindTextColor, getMutedTextColor, getSecondaryColor, getTextColor)
import Types exposing (AssetSource(..), DeviceClass(..), Model, Msg(..), PaginationState, Theme(..), UserMode(..))
import UpdateAsset exposing (AssetState(..))
import UpdateMenus exposing (MenuState(..))
import ViewAlbums exposing (InputMode(..))
import ViewAsset
import ViewGrid


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height (fill |> minimum 1)
        , Background.color (getBackgroundColor model.theme)
        , Font.color (getTextColor model.theme)
        ]
    <|
        viewWithInputBottomBar model.deviceClass model.userMode model.theme <|
            viewMainWindow model


viewWithInputBottomBar : DeviceClass -> UserMode -> Theme -> Element Msg -> Element Msg
viewWithInputBottomBar deviceClass userMode theme viewMain =
    let
        inputBarHeight =
            case deviceClass of
                Mobile ->
                    px 44

                -- Touch-friendly height for mobile
                Tablet ->
                    px 30

                Desktop ->
                    px 20
    in
    column [ width fill, height fill ]
        [ el [ width fill, height (fill |> minimum 1), clipY ] viewMain
        , el [ width fill, height inputBarHeight ] <| viewInputMode userMode theme
        ]


viewMainWindow : Model -> Element Msg
viewMainWindow model =
    let
        mainContent =
            case model.userMode of
                MainMenu menuState ->
                    viewMenuState model menuState

                ViewAssets assetState ->
                    viewAssetState model assetState

                LoadingAssets _ ->
                    ViewAsset.viewLoadingAssets model.imagesLoadState
    in
    Element.el [ width fill, height fill, Element.inFront (viewPaginationStatus model.paginationState) ] mainContent


viewMenuState : Model -> MenuState -> Element Msg
viewMenuState model menuState =
    case menuState of
        MainMenuHome ->
            let
                isConfigured =
                    not (String.isEmpty model.baseUrl) && not (String.isEmpty model.apiKey)
            in
            Menus.viewMainMenu (model.deviceClass == Mobile) model.reloadFeedback isConfigured

        TimelineView config ->
            Menus.viewTimelineView model config LoadDataAgain LoadTimelineAssets

        SearchView config ->
            Menus.viewSearchView model config ChangeSearchQuery SelectSearchSuggestion ExecuteSearch ClearSearchQuery

        AlbumBrowse search ->
            Element.row [ width fill, height fill ]
                [ Element.column [ width (fillPortion 4 |> minimum 280), height fill, paddingXY 15 15, Element.spacingXY 0 15 ]
                    [ el [ Font.size 20, Font.bold ] (text "📁 Browse Albums")
                    , if search.searchString /= "" then
                        text ("Search: \"" ++ search.searchString ++ "\"")

                      else
                        text ""
                    , if search.partialKeybinding /= "" then
                        let
                            nextChars =
                                ViewAlbums.getNextAvailableCharacters search.partialKeybinding model.albumKeybindings

                            nextCharString =
                                String.fromList nextChars
                        in
                        column []
                            [ el [ Font.color <| Element.fromRgb { red = 1, green = 0.6, blue = 0, alpha = 1 } ] <|
                                text ("Keybind: \"" ++ search.partialKeybinding ++ "\"")
                            , if List.isEmpty nextChars then
                                el [ Font.color <| Element.fromRgb { red = 1, green = 0.2, blue = 0.2, alpha = 1 }, Font.size 12 ] <| text "No matches"

                              else
                                el [ Font.color <| getMutedTextColor model.theme, Font.size 12 ] <| text ("Next: " ++ nextCharString)
                            ]

                      else
                        text ""
                    , case search.invalidInputWarning of
                        Just warning ->
                            el [ Font.color <| Element.fromRgb { red = 1, green = 0.2, blue = 0.2, alpha = 1 }, Font.size 12 ] <| text ("Invalid: \"" ++ warning ++ "\"")

                        Nothing ->
                            text ""
                    , ViewAlbums.viewSidebarAlbums search model.albumKeybindings model.knownAlbums SelectAlbum (getKeybindTextColor model.theme) (getMutedTextColor model.theme) (getHighlightColor model.theme)
                    ]
                , Element.column [ width (fillPortion 5), height fill, paddingXY 20 20 ]
                    [ el [ Font.size 16 ] (text "Select an album from the left to configure and view its contents.")
                    , el [ Font.size 14, Font.color <| getMutedTextColor model.theme ] (text "Type album name or keybinding to filter the list.")
                    ]
                , Element.column [ width (fillPortion 4 |> minimum 300), height fill, paddingXY 15 15 ]
                    [ viewContextHelp (AlbumBrowseContext SelectingAlbum)
                    ]
                ]

        AlbumView album config ->
            Menus.viewAlbumView model album config LoadAlbumAssets

        Settings ->
            Menus.viewSettings model UpdateSettingsApiUrl UpdateSettingsApiKey SaveConfig ClearConfig


viewPaginationStatus : PaginationState -> Element Msg
viewPaginationStatus paginationState =
    if paginationState.isLoadingMore then
        let
            progressText =
                "Loading assets: "
                    ++ String.fromInt paginationState.loadedAssets
                    ++ " / "
                    ++ (if paginationState.totalAssets > 0 then
                            String.fromInt (min paginationState.totalAssets paginationState.maxAssetsToFetch)

                        else
                            "?"
                       )
                    ++ " (page "
                    ++ String.fromInt paginationState.currentPage
                    ++ ")"
        in
        el
            [ alignRight
            , alignTop
            , Element.padding 10
            , Background.color (Element.rgba 0 0 0 0.8)
            , Font.color (Element.rgb 1 1 1)
            , Font.size 14
            ]
            (text progressText)

    else
        Element.none


viewAssetState : Model -> AssetState -> Element Msg
viewAssetState model assetState =
    case assetState of
        SearchAssetInput searchString ->
            column []
                [ text "Search Assets"
                , text searchString
                ]

        SelectAlbumInput search ->
            ViewAlbums.viewWithSidebar
                (ViewAlbums.viewSidebarAlbums search model.albumKeybindings model.knownAlbums SelectAlbum (getKeybindTextColor model.theme) (getMutedTextColor model.theme) (getHighlightColor model.theme))
                (column []
                    [ text "Select Album"
                    , text search.searchString
                    ]
                )

        EditAsset inputMode asset search ->
            let
                viewTitle =
                    createDetailedViewTitle model.currentAssetsSource
            in
            ViewAlbums.viewWithSidebar (ViewAlbums.viewSidebar asset search model.albumKeybindings model.knownAlbums (Just inputMode) SelectAlbum (getKeybindTextColor model.theme) (getMutedTextColor model.theme) (getHighlightColor model.theme)) (ViewAsset.viewEditAsset model.immichApiPaths model.apiKey model.imageIndex (List.length model.currentAssets) viewTitle asset model.currentAssets model.knownAssets model.currentDateMillis model.timeViewMode inputMode)

        CreateAlbumConfirmation _ asset search albumName ->
            ViewAlbums.viewWithSidebar (ViewAlbums.viewSidebar asset search model.albumKeybindings model.knownAlbums Nothing SelectAlbum (getKeybindTextColor model.theme) (getMutedTextColor model.theme) (getHighlightColor model.theme)) (ViewAsset.viewCreateAlbumConfirmation albumName)

        ShowEditAssetHelp inputMode asset search ->
            ViewAlbums.viewWithSidebar (ViewAlbums.viewSidebar asset search model.albumKeybindings model.knownAlbums (Just inputMode) SelectAlbum (getKeybindTextColor model.theme) (getMutedTextColor model.theme) (getHighlightColor model.theme)) (ViewAsset.viewEditAssetHelp inputMode)

        GridView gridState ->
            ViewAsset.viewGridAssets model.immichApiPaths model.apiKey gridState model.currentAssets model.knownAssets model.paginationState.hasMorePages model.paginationState.isLoadingMore (AssetMsg << UpdateAsset.AssetGridMsg)


isInInputMode : UserMode -> Bool
isInInputMode userMode =
    case userMode of
        MainMenu menuState ->
            case menuState of
                SearchView config ->
                    config.inputFocused

                _ ->
                    False

        ViewAssets assetState ->
            case assetState of
                SearchAssetInput _ ->
                    True

                SelectAlbumInput _ ->
                    True

                EditAsset editInputMode _ _ ->
                    editInputMode == InsertMode

                CreateAlbumConfirmation editInputMode _ _ _ ->
                    editInputMode == InsertMode

                ShowEditAssetHelp editInputMode _ _ ->
                    editInputMode == InsertMode

                _ ->
                    False

        LoadingAssets _ ->
            False


viewInputMode : UserMode -> Theme -> Element msg
viewInputMode userMode theme =
    let
        inputMode =
            case userMode of
                MainMenu menuState ->
                    case menuState of
                        MainMenuHome ->
                            NormalMode

                        TimelineView _ ->
                            NormalMode

                        SearchView config ->
                            if config.inputFocused then
                                InsertMode

                            else
                                NormalMode

                        AlbumBrowse _ ->
                            KeybindingMode

                        AlbumView _ _ ->
                            NormalMode

                        Settings ->
                            NormalMode

                ViewAssets assetState ->
                    case assetState of
                        SearchAssetInput _ ->
                            InsertMode

                        SelectAlbumInput _ ->
                            InsertMode

                        EditAsset editInputMode _ _ ->
                            editInputMode

                        CreateAlbumConfirmation editInputMode _ _ _ ->
                            editInputMode

                        ShowEditAssetHelp editInputMode _ _ ->
                            editInputMode

                        GridView _ ->
                            NormalMode

                LoadingAssets _ ->
                    NormalMode

        themeText =
            case theme of
                Light ->
                    "☀️"

                Dark ->
                    "🌙"

                System ->
                    "⚙️"
    in
    row [ width fill ]
        [ case inputMode of
            NormalMode ->
                el [ width (fillPortion 1), Background.color <| Element.fromRgb { red = 0.8, green = 0.8, blue = 0.8, alpha = 1 } ] <| text "Normal"

            InsertMode ->
                el [ width (fillPortion 1), Background.color <| Element.fromRgb { red = 0, green = 1, blue = 0, alpha = 1 } ] <| text "Input"

            KeybindingMode ->
                el [ width (fillPortion 1), Background.color <| Element.fromRgb { red = 1, green = 0.5, blue = 0, alpha = 1 } ] <| text "Keybind"

            ScrollViewMode _ ->
                el [ width (fillPortion 1), Background.color <| Element.fromRgb { red = 0.5, green = 0, blue = 1, alpha = 1 } ] <| text "Scroll"
        , el [ width (px 40), Background.color <| getSecondaryColor theme, Font.color <| getTextColor theme, Element.centerX ] <| text themeText
        ]


createDetailedViewTitle : AssetSource -> String
createDetailedViewTitle assetSource =
    case assetSource of
        ImageSearch config ->
            let
                orderText =
                    case config.order of
                        CreatedDesc ->
                            "[created desc]"

                        CreatedAsc ->
                            "[created asc]"

                        ModifiedDesc ->
                            "[modified desc]"

                        ModifiedAsc ->
                            "[modified asc]"

                        Random ->
                            "[random]"

                mediaText =
                    case config.mediaType of
                        AllMedia ->
                            ""

                        ImagesOnly ->
                            " [images]"

                        VideosOnly ->
                            " [videos]"

                statusText =
                    case config.status of
                        Immich.AllStatuses ->
                            ""

                        Immich.FavoritesOnly ->
                            " [favourites]"

                        Immich.ArchivedOnly ->
                            " [archived]"

                categText =
                    case config.categorisation of
                        All ->
                            "Timeline"

                        Uncategorised ->
                            "Timeline [uncategorised]"
            in
            categText ++ statusText ++ mediaText ++ " " ++ orderText

        TextSearch searchText ->
            "Search \"" ++ searchText ++ "\""

        Album album ->
            "Album \"" ++ album.albumName ++ "\""

        FilteredAlbum album config ->
            let
                orderText =
                    case config.order of
                        CreatedDesc ->
                            "[created desc]"

                        CreatedAsc ->
                            "[created asc]"

                        ModifiedDesc ->
                            "[modified desc]"

                        ModifiedAsc ->
                            "[modified asc]"

                        Random ->
                            "[random]"

                mediaText =
                    case config.mediaType of
                        AllMedia ->
                            ""

                        ImagesOnly ->
                            " [images]"

                        VideosOnly ->
                            " [videos]"

                statusText =
                    case config.status of
                        Immich.AllStatuses ->
                            ""

                        Immich.FavoritesOnly ->
                            " [favourites]"

                        Immich.ArchivedOnly ->
                            " [archived]"

                hasFilters =
                    config.mediaType /= AllMedia || config.status /= Immich.AllStatuses || config.order /= CreatedDesc
            in
            if hasFilters then
                "Album \"" ++ album.albumName ++ "\"" ++ statusText ++ mediaText ++ " " ++ orderText

            else
                "Album \"" ++ album.albumName ++ "\""

        NoAssets ->
            ""
