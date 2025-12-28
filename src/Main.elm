port module Main exposing (main)

import AssetNavigation
import Browser exposing (element)
import Browser.Events exposing (onKeyDown, onKeyUp, onResize, onVisibilityChange)
import Date
import Dict exposing (Dict)
import Element exposing (Element, alignRight, alignTop, clipY, column, el, fill, fillPortion, height, minimum, paddingXY, px, row, text, width)
import Element.Background as Background
import Element.Font as Font
import HelpText exposing (AlbumBrowseState(..), ViewContext(..), viewContextHelp)
import Helpers exposing (applySortingToAssets, filterByMediaType, filterByStatus, validateConfig)
import Html exposing (Html)
import Immich exposing (CategorisationFilter(..), ImageOrder(..), ImageSearchConfig, ImmichAlbum, ImmichAlbumId, ImmichApiPaths, ImmichAsset, ImmichAssetId, ImmichLoadState(..), MediaTypeFilter(..), SearchContext(..), StatusFilter(..), getAllAlbums, getImmichApiPaths)
import Json.Decode as Decode
import KeybindBranches exposing (generateAlbumKeybindings)
import LoadState
import Menus exposing (AlbumConfig, defaultAlbumConfig, defaultSearchConfig)
import Navigation
import Pagination
import Process
import Task
import Theme exposing (DeviceClass(..), Theme(..))
import TitleHelpers exposing (createDetailedViewTitle, getMoveFromInfo)
import Types exposing (AlbumPaginationContext, AssetSource(..), AssetSourceUpdate(..), ImageIndex, NavigationHistoryEntry, PaginationState, SourceLoadState, UserMode(..))
import UpdateAlbums exposing (AlbumMsg)
import UpdateAsset exposing (AssetMsg(..), AssetResult(..), AssetState(..), updateAsset)
import UpdateAssetResult
import UpdateConfig
import UpdateImmich
import UpdateMenuConfig
import UpdateMenuResult
import UpdateMenus exposing (MenuMsg(..), MenuResult(..), MenuState(..), updateMenus)
import ViewAlbums exposing (AlbumSearch, AssetWithActions, InputMode(..), PropertyChange(..))
import ViewAsset exposing (TimeViewMode(..))
import ViewGrid



-- PORTS


port openUrl : String -> Cmd msg


port saveToStorage : ( String, String ) -> Cmd msg


port loadFromStorage : String -> Cmd msg


port clearStorage : () -> Cmd msg


port storageLoaded : (( String, Maybe String ) -> msg) -> Sub msg


port yankAssetToClipboard : String -> Cmd msg


type Msg
    = KeyPress String
    | KeyRelease String
    | VisibilityChanged Browser.Events.Visibility
    | ImmichMsg Immich.Msg
    | LoadDataAgain
    | ClearReloadFeedback
    | SelectAlbum ImmichAlbum
    | WindowResize Int Int
    | ChangeTimelineMediaType MediaTypeFilter
    | ChangeTimelineCategorisation CategorisationFilter
    | ChangeTimelineOrder ImageOrder
    | ChangeTimelineStatus StatusFilter
    | ChangeSearchMediaType MediaTypeFilter
    | ChangeSearchContext SearchContext
    | ChangeSearchStatus StatusFilter
    | ChangeSearchQuery String
    | SelectSearchSuggestion String
    | ClearSearchQuery
    | ChangeAlbumMediaType MediaTypeFilter
    | ChangeAlbumOrder ImageOrder
    | ChangeAlbumStatus StatusFilter
    | LoadTimelineAssets
    | ExecuteSearch
    | LoadAlbumAssets ImmichAlbum
    | SearchInputFocused
    | SearchInputBlurred
      -- Config-related messages
    | SaveConfig String String
    | LoadConfig String
    | ConfigLoaded String (Maybe String)
    | ClearConfig
    | UpdateSettingsApiUrl String
    | UpdateSettingsApiKey String
      -- Module-specific messages
    | MenuMsg MenuMsg
    | AlbumMsg AlbumMsg
    | AssetMsg AssetMsg
      -- Theme messages
    | ToggleTheme


type alias Flags =
    { currentDateMillis : Int
    , immichApiKey : String
    , immichApiUrl : String
    }


type alias Model =
    { key : String
    , currentAssetsSource : AssetSource
    , userMode : UserMode
    , currentDateMillis : Int
    , imageIndex : ImageIndex
    , imageSearchConfig : ImageSearchConfig
    , timeViewMode : TimeViewMode
    , reloadFeedback : Maybe String
    , controlPressed : Bool
    , altPressed : Bool

    -- Navigation history (stack + current + queue model)
    , navigationBackStack : List NavigationHistoryEntry
    , currentNavigationState : Maybe NavigationHistoryEntry
    , navigationForwardQueue : List NavigationHistoryEntry

    -- Configuration fields
    , configuredApiUrl : Maybe String
    , configuredApiKey : Maybe String
    , settingsApiUrl : String
    , settingsApiKey : String
    , configValidationMessage : Maybe String

    -- Immich fields
    , currentAssets : List ImmichAssetId
    , knownAssets : Dict ImmichAssetId ImmichAsset
    , imagesLoadState : ImmichLoadState
    , knownAlbums : Dict ImmichAlbumId ImmichAlbum
    , albumKeybindings : Dict ImmichAlbumId String
    , albumsLoadState : ImmichLoadState
    , baseUrl : String
    , apiKey : String
    , envBaseUrl : String -- Original env values for defaulting back
    , envApiKey : String
    , immichApiPaths : ImmichApiPaths
    , screenHeight : Int
    , screenWidth : Int
    , deviceClass : DeviceClass
    , theme : Theme
    , pendingAlbumChanges : List ( ImmichAlbumId, Bool ) -- (albumId, isAddition)
    , paginationState : PaginationState
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { key = ""
      , userMode = MainMenu MainMenuHome
      , currentAssetsSource = NoAssets
      , currentDateMillis = flags.currentDateMillis
      , imageIndex = 0
      , imageSearchConfig = { order = CreatedDesc, categorisation = Uncategorised, mediaType = AllMedia, status = AllStatuses }
      , timeViewMode = Absolute
      , reloadFeedback = Nothing
      , controlPressed = False
      , altPressed = False

      -- Navigation history (stack + current + queue model)
      , navigationBackStack = []
      , currentNavigationState = Nothing
      , navigationForwardQueue = []

      -- Configuration fields
      , configuredApiUrl = Nothing
      , configuredApiKey = Nothing
      , settingsApiUrl = flags.immichApiUrl
      , settingsApiKey = flags.immichApiKey
      , configValidationMessage = Nothing

      -- Immich fields
      , currentAssets = []
      , knownAssets = Dict.empty
      , imagesLoadState = ImmichLoading
      , knownAlbums = Dict.empty
      , albumKeybindings = Dict.empty
      , albumsLoadState = ImmichLoading
      , baseUrl = flags.immichApiUrl
      , apiKey = flags.immichApiKey
      , envBaseUrl = flags.immichApiUrl
      , envApiKey = flags.immichApiKey
      , immichApiPaths = getImmichApiPaths flags.immichApiUrl flags.immichApiKey
      , screenHeight = 800 -- Default, will be updated by window resize
      , screenWidth = 1200 -- Default, will be updated by window resize
      , deviceClass = Theme.classifyDevice 1200 800 -- Will be updated by WindowResize
      , theme = Dark -- Default to dark theme
      , pendingAlbumChanges = []
      , paginationState =
            { currentConfig = Nothing
            , currentQuery = Nothing
            , currentSearchContext = Nothing
            , currentAlbumContext = Nothing
            , totalAssets = 0
            , currentPage = 1
            , hasMorePages = False
            , isLoadingMore = False
            , loadedAssets = 0
            , maxAssetsToFetch = 10000 -- Default limit of 10,000 assets
            }
      }
    , Cmd.batch
        [ loadFromStorage "immichApiUrl"
        , loadFromStorage "immichApiKey"

        -- Don't load albums immediately - wait for localStorage config to load first
        -- to avoid duplicate API calls with different credentials
        ]
    )



-- NAVIGATION HISTORY HELPERS --
-- Helper to extract navigation fields from Model


getNavFields : Model -> Navigation.NavigationFields
getNavFields model =
    { navigationBackStack = model.navigationBackStack
    , currentNavigationState = model.currentNavigationState
    , navigationForwardQueue = model.navigationForwardQueue
    }



-- Helper to apply NavigateResult to Model


applyNavigateResult : Navigation.NavigateResult -> Model -> ( Model, Cmd Msg )
applyNavigateResult result model =
    case result of
        Navigation.NoHistory ->
            ( model, Cmd.none )

        Navigation.RestoredState restored ->
            let
                restoredModel =
                    { model
                        | userMode = restored.userMode
                        , currentAssetsSource = restored.currentAssetsSource
                        , currentAssets = restored.currentAssets
                        , imageIndex = restored.imageIndex
                        , paginationState = restored.paginationState
                        , navigationBackStack = restored.navFields.navigationBackStack
                        , currentNavigationState = restored.navFields.currentNavigationState
                        , navigationForwardQueue = restored.navFields.navigationForwardQueue
                    }
            in
            if restored.needsAssetSwitch then
                switchToAssetWithoutHistory restoredModel restored.assetIndex

            else
                ( restoredModel, Cmd.none )


recordNavigationState : UserMode -> Model -> Model
recordNavigationState newMode model =
    let
        navFields =
            Navigation.clearForwardQueueForViewAssets newMode (getNavFields model)
    in
    { model | navigationForwardQueue = navFields.navigationForwardQueue }


setCurrentNavigationState : Model -> Model
setCurrentNavigationState model =
    let
        maybeEntry =
            Navigation.createCurrentNavigationEntry
                model.userMode
                model.currentAssetsSource
                model.currentAssets
                model.imageIndex
                model.paginationState
    in
    case maybeEntry of
        Just entry ->
            { model | currentNavigationState = Just entry }

        Nothing ->
            model


updateCurrentHistoryEntry : Model -> Model
updateCurrentHistoryEntry model =
    { model
        | currentNavigationState =
            Navigation.updateCurrentEntry
                model.currentNavigationState
                model.userMode
                model.currentAssetsSource
                model.currentAssets
                model.imageIndex
                model.paginationState
    }


navigateHistoryBack : Model -> ( Model, Cmd Msg )
navigateHistoryBack model =
    applyNavigateResult (Navigation.navigateBack (getNavFields model)) model


navigateHistoryForward : Model -> ( Model, Cmd Msg )
navigateHistoryForward model =
    applyNavigateResult (Navigation.navigateForward (getNavFields model)) model


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height (fill |> minimum 1)
        , Background.color (Theme.getBackgroundColor model.theme)
        , Font.color (Theme.getTextColor model.theme)
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
                    ViewAsset.viewLoadingAssets model.imagesLoadState (createDetailedViewTitle model.currentAssetsSource)
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
                                el [ Font.color <| Theme.getMutedTextColor model.theme, Font.size 12 ] <| text ("Next: " ++ nextCharString)
                            ]

                      else
                        text ""
                    , case search.invalidInputWarning of
                        Just warning ->
                            el [ Font.color <| Element.fromRgb { red = 1, green = 0.2, blue = 0.2, alpha = 1 }, Font.size 12 ] <| text ("Invalid: \"" ++ warning ++ "\"")

                        Nothing ->
                            text ""
                    , ViewAlbums.viewSidebarAlbums search model.albumKeybindings model.knownAlbums SelectAlbum (Theme.getKeybindTextColor model.theme) (Theme.getMutedTextColor model.theme) (Theme.getHighlightColor model.theme)
                    ]
                , Element.column [ width (fillPortion 5), height fill, paddingXY 20 20 ]
                    [ el [ Font.size 16 ] (text "Select an album from the left to configure and view its contents.")
                    , el [ Font.size 14, Font.color <| Theme.getMutedTextColor model.theme ] (text "Type album name or keybinding to filter the list.")
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
                (ViewAlbums.viewSidebarAlbums search model.albumKeybindings model.knownAlbums SelectAlbum (Theme.getKeybindTextColor model.theme) (Theme.getMutedTextColor model.theme) (Theme.getHighlightColor model.theme))
                (column []
                    [ text "Select Album"
                    , text search.searchString
                    ]
                )

        EditAsset inputMode asset search ->
            let
                viewTitle =
                    createDetailedViewTitle model.currentAssetsSource

                moveFromInfo =
                    getMoveFromInfo model.currentAssetsSource
            in
            ViewAlbums.viewWithSidebar (ViewAlbums.viewSidebar asset search model.albumKeybindings model.knownAlbums (Just inputMode) moveFromInfo SelectAlbum (Theme.getKeybindTextColor model.theme) (Theme.getMutedTextColor model.theme) (Theme.getHighlightColor model.theme)) (ViewAsset.viewEditAsset model.immichApiPaths model.apiKey model.imageIndex (List.length model.currentAssets) viewTitle asset model.currentAssets model.knownAssets model.currentDateMillis model.timeViewMode inputMode)

        CreateAlbumConfirmation _ asset search albumName ->
            let
                moveFromInfo =
                    getMoveFromInfo model.currentAssetsSource
            in
            ViewAlbums.viewWithSidebar (ViewAlbums.viewSidebar asset search model.albumKeybindings model.knownAlbums Nothing moveFromInfo SelectAlbum (Theme.getKeybindTextColor model.theme) (Theme.getMutedTextColor model.theme) (Theme.getHighlightColor model.theme)) (ViewAsset.viewCreateAlbumConfirmation albumName)

        ShowEditAssetHelp inputMode asset search ->
            let
                moveFromInfo =
                    getMoveFromInfo model.currentAssetsSource
            in
            ViewAlbums.viewWithSidebar (ViewAlbums.viewSidebar asset search model.albumKeybindings model.knownAlbums (Just inputMode) moveFromInfo SelectAlbum (Theme.getKeybindTextColor model.theme) (Theme.getMutedTextColor model.theme) (Theme.getHighlightColor model.theme)) (ViewAsset.viewEditAssetHelp inputMode)

        GridView gridState ->
            ViewAsset.viewGridAssets model.immichApiPaths model.apiKey gridState model.currentAssets model.knownAssets model.paginationState.hasMorePages model.paginationState.isLoadingMore (AssetMsg << AssetGridMsg)


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
        , el [ width (px 40), Background.color <| Theme.getSecondaryColor theme, Font.color <| Theme.getTextColor theme, Element.centerX ] <| text themeText
        ]



-- UPDATE --
-- Helper to apply MenuState updates (reduces boilerplate for config changes)


updateMenuState : (MenuState -> MenuState) -> Model -> Model
updateMenuState fn model =
    case model.userMode of
        MainMenu menuState ->
            { model | userMode = MainMenu (fn menuState) }

        _ ->
            model



-- Helper function to handle MenuResult


handleMenuResult : MenuResult msg -> Model -> ( Model, Cmd Msg )
handleMenuResult menuResult model =
    case UpdateMenuResult.processMenuResult menuResult model.paginationState (Just model.userMode) of
        UpdateMenuResult.StayInMenuAction newMenuState ->
            ( { model | userMode = MainMenu newMenuState }, Cmd.none )

        UpdateMenuResult.LoadAssetsAction config loadType ->
            let
                loadModel =
                    createLoadStateForCurrentAssetSource config.assetSource model

                modelWithPagination =
                    { loadModel | paginationState = config.paginationState }

                loadCmd =
                    generateMenuLoadCmd loadType modelWithPagination
            in
            ( modelWithPagination, loadCmd )

        UpdateMenuResult.ReloadAlbumsAction ->
            ( model, Immich.getAllAlbums model.baseUrl model.apiKey |> Cmd.map ImmichMsg )

        UpdateMenuResult.UpdateSearchFocusAction focused ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        SearchView config ->
                            ( { model | userMode = MainMenu (SearchView { config | inputFocused = focused }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


{-| Generate the load Cmd based on LoadType from menu result.
-}
generateMenuLoadCmd : UpdateMenuResult.LoadType -> Model -> Cmd Msg
generateMenuLoadCmd loadType model =
    case loadType of
        UpdateMenuResult.TimelineLoad searchConfig ->
            Immich.fetchImagesPaginated model.immichApiPaths searchConfig 1000 1 |> Cmd.map ImmichMsg

        UpdateMenuResult.TextSearchLoad query searchContext ->
            Immich.searchAssetsPaginated model.immichApiPaths searchContext query AllMedia AllStatuses 1000 1 |> Cmd.map ImmichMsg

        UpdateMenuResult.AlbumLoad album config ->
            Immich.fetchAlbumAssetsWithFilters model.immichApiPaths album.id config.order config.mediaType config.status |> Cmd.map ImmichMsg



-- Helper function to handle AssetResult


handleAssetResult : AssetResult msg -> Model -> ( Model, Cmd Msg )
handleAssetResult assetResult model =
    let
        context =
            { userMode = model.userMode
            , currentAssetsSource = model.currentAssetsSource
            , currentAssets = model.currentAssets
            , imageIndex = model.imageIndex
            , paginationState = model.paginationState
            , currentNavigationState = model.currentNavigationState
            , navigationBackStack = model.navigationBackStack
            , screenHeight = model.screenHeight
            , timeViewMode = model.timeViewMode
            , baseUrl = model.baseUrl
            }
    in
    case UpdateAssetResult.processAssetResult assetResult context of
        UpdateAssetResult.StayInAssetsAction newAssetState ->
            let
                newUserMode =
                    ViewAssets newAssetState

                updatedModel =
                    { model | userMode = newUserMode }
                        |> recordNavigationState newUserMode
                        |> setCurrentNavigationState
            in
            ( updatedModel, Cmd.none )

        UpdateAssetResult.GoToMainMenuAction data ->
            let
                updatedModel =
                    case data.currentEntry of
                        Just entry ->
                            { model
                                | navigationBackStack = data.newBackStack
                                , currentNavigationState = Just entry
                                , navigationForwardQueue = []
                            }

                        Nothing ->
                            model
            in
            ( { updatedModel | userMode = MainMenu MainMenuHome }, Cmd.none )

        UpdateAssetResult.GoToSearchViewAction query ->
            ( { model | userMode = MainMenu (SearchView { defaultSearchConfig | query = query }) }, Cmd.none )

        UpdateAssetResult.LoadTextSearchAction query ->
            let
                mainAssetSource =
                    TextSearch query ContentSearch

                loadModel =
                    createLoadStateForCurrentAssetSource mainAssetSource model

                loadCmd =
                    Immich.searchAssetsPaginated model.immichApiPaths ContentSearch query AllMedia AllStatuses 1000 1 |> Cmd.map ImmichMsg
            in
            ( loadModel, loadCmd )

        UpdateAssetResult.LoadAlbumAction album ->
            let
                mainAssetSource =
                    Album album

                loadModel =
                    createLoadStateForCurrentAssetSource mainAssetSource model

                loadCmd =
                    Immich.getAlbum model.immichApiPaths album.id |> Cmd.map ImmichMsg
            in
            ( loadModel, loadCmd )

        UpdateAssetResult.SwitchToAssetIndexAction newIndex ->
            switchToAssetWithoutHistory model newIndex

        UpdateAssetResult.ToggleFavoriteAction newAsset newIsFavorite ->
            case model.userMode of
                ViewAssets (EditAsset inputMode _ search) ->
                    ( { model | userMode = ViewAssets (EditAsset inputMode newAsset search) }
                    , Immich.updateAssetFavorite model.immichApiPaths newAsset.asset.id newIsFavorite |> Cmd.map ImmichMsg
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateAssetResult.ToggleArchivedAction newAsset newIsArchived ->
            case model.userMode of
                ViewAssets (EditAsset inputMode _ search) ->
                    ( { model | userMode = ViewAssets (EditAsset inputMode newAsset search) }
                    , Immich.updateAssetArchived model.immichApiPaths newAsset.asset.id newIsArchived |> Cmd.map ImmichMsg
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateAssetResult.ToggleAlbumMembershipAction data ->
            let
                primaryCmd =
                    Immich.albumChangeAssetMembership model.immichApiPaths data.targetAlbumId [ data.newAsset.asset.id ] data.isAddition
                        |> Cmd.map ImmichMsg

                moveFromCmd =
                    case data.moveFromSourceId of
                        Just sourceId ->
                            Immich.albumChangeAssetMembership model.immichApiPaths sourceId [ data.newAsset.asset.id ] False
                                |> Cmd.map ImmichMsg

                        Nothing ->
                            Cmd.none
            in
            ( { model | userMode = ViewAssets (EditAsset NormalMode data.newAsset data.newSearch), pendingAlbumChanges = data.pendingChanges }
            , Cmd.batch [ primaryCmd, moveFromCmd ]
            )

        UpdateAssetResult.OpenInImmichAction url ->
            ( model, openUrl url )

        UpdateAssetResult.YankToClipboardAction assetId ->
            ( model, yankAssetToClipboard assetId )

        UpdateAssetResult.ToggleMoveFromModeAction album newConfig ->
            ( { model | currentAssetsSource = FilteredAlbum album newConfig }, Cmd.none )

        UpdateAssetResult.CreateAlbumAction albumName ->
            ( { model | userMode = LoadingAssets { fetchedAssetList = Nothing, fetchedAssetMembership = Nothing } }
            , Immich.createAlbum model.immichApiPaths albumName |> Cmd.map ImmichMsg
            )

        UpdateAssetResult.ToggleTimeViewAction newTimeViewMode ->
            ( { model | timeViewMode = newTimeViewMode }, Cmd.none )

        UpdateAssetResult.SwitchToGridViewAction gridState ->
            let
                newUserMode =
                    ViewAssets (GridView gridState)

                updatedModel =
                    { model | userMode = newUserMode }
                        |> recordNavigationState newUserMode
                        |> setCurrentNavigationState
            in
            ( updatedModel, Cmd.none )

        UpdateAssetResult.SwitchToDetailViewAction maybeIndex ->
            case maybeIndex of
                Just assetIndex ->
                    switchToEditIfAssetFound model assetIndex

                Nothing ->
                    ( model, Cmd.none )

        UpdateAssetResult.GridUpdateAction gridState ->
            ( { model | userMode = ViewAssets (GridView gridState) }, Cmd.none )

        UpdateAssetResult.BulkFavoriteAction assetIds isFavorite ->
            ( model, Immich.bulkUpdateAssetsFavorite model.immichApiPaths assetIds isFavorite |> Cmd.map ImmichMsg )

        UpdateAssetResult.BulkArchiveAction assetIds isArchived ->
            ( model, Immich.bulkUpdateAssetsArchived model.immichApiPaths assetIds isArchived |> Cmd.map ImmichMsg )

        UpdateAssetResult.BulkAddToAlbumAction assetIds albumId ->
            ( model, Immich.albumChangeAssetMembership model.immichApiPaths albumId assetIds True |> Cmd.map ImmichMsg )

        UpdateAssetResult.BulkRemoveFromAlbumAction assetIds albumId ->
            ( model, Immich.albumChangeAssetMembership model.immichApiPaths albumId assetIds False |> Cmd.map ImmichMsg )

        UpdateAssetResult.RequestLoadMoreAction ->
            let
                paginationState =
                    model.paginationState

                nextPage =
                    paginationState.currentPage + 1

                loadMoreCmd =
                    case ( paginationState.currentConfig, paginationState.currentQuery, paginationState.currentSearchContext ) of
                        ( Just config, Nothing, _ ) ->
                            Immich.fetchImagesPaginated model.immichApiPaths config 1000 nextPage |> Cmd.map ImmichMsg

                        ( Nothing, Just query, Just searchContext ) ->
                            Immich.searchAssetsPaginated model.immichApiPaths searchContext query AllMedia AllStatuses 1000 nextPage |> Cmd.map ImmichMsg

                        ( Nothing, Just query, Nothing ) ->
                            Immich.searchAssetsPaginated model.immichApiPaths ContentSearch query AllMedia AllStatuses 1000 nextPage |> Cmd.map ImmichMsg

                        _ ->
                            Cmd.none

                updatedPaginationState =
                    { paginationState | isLoadingMore = True }
            in
            ( { model | paginationState = updatedPaginationState }, loadMoreCmd )

        UpdateAssetResult.ReloadAlbumsAction ->
            ( model, Immich.getAllAlbums model.baseUrl model.apiKey |> Cmd.map ImmichMsg )

        UpdateAssetResult.NoAction ->
            ( model, Cmd.none )



-- Helper to convert UpdateMenus.AssetSource to Main.AssetSource


convertMenuAssetSource : UpdateMenus.AssetSource -> AssetSource
convertMenuAssetSource menuAssetSource =
    case menuAssetSource of
        UpdateMenus.ImageSearch config ->
            ImageSearch config

        UpdateMenus.TextSearch query searchContext ->
            TextSearch query searchContext

        UpdateMenus.FilteredAlbum album config ->
            FilteredAlbum album config


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadDataAgain ->
            ( model, Immich.getAllAlbums model.baseUrl model.apiKey |> Cmd.map ImmichMsg )

        ClearReloadFeedback ->
            ( { model | reloadFeedback = Nothing }, Cmd.none )

        SaveConfig url apiKey ->
            case UpdateConfig.handleSaveConfig { url = url, apiKey = apiKey, envBaseUrl = model.envBaseUrl, envApiKey = model.envApiKey } of
                UpdateConfig.ValidationError errorMsg ->
                    ( { model | configValidationMessage = Just errorMsg }, Cmd.none )

                UpdateConfig.ConfigValid result ->
                    ( { model
                        | configValidationMessage = Just "Saving configuration..."
                        , configuredApiUrl = Just result.finalUrl
                        , configuredApiKey = Just result.finalApiKey
                        , baseUrl = result.finalUrl
                        , apiKey = result.finalApiKey
                        , immichApiPaths = result.immichApiPaths
                      }
                    , Cmd.batch
                        [ saveToStorage ( "immichApiUrl", result.finalUrl )
                        , saveToStorage ( "immichApiKey", result.finalApiKey )
                        , Process.sleep 1000
                            |> Task.perform (always (ConfigLoaded "saveSuccess" (Just "✅ Configuration saved successfully!")))
                        ]
                    )

        LoadConfig key ->
            ( model, loadFromStorage key )

        ConfigLoaded key maybeValue ->
            let
                result =
                    UpdateConfig.handleConfigLoaded
                        { key = key
                        , maybeValue = maybeValue
                        , currentConfiguredApiUrl = model.configuredApiUrl
                        , currentConfiguredApiKey = model.configuredApiKey
                        , currentSettingsApiUrl = model.settingsApiUrl
                        , currentSettingsApiKey = model.settingsApiKey
                        , currentBaseUrl = model.baseUrl
                        , currentApiKey = model.apiKey
                        , knownAlbums = model.knownAlbums
                        , albumKeybindings = model.albumKeybindings
                        , albumsLoadState = model.albumsLoadState
                        }

                finalModel =
                    { model
                        | configuredApiUrl = result.configuredApiUrl
                        , configuredApiKey = result.configuredApiKey
                        , settingsApiUrl = result.settingsApiUrl
                        , settingsApiKey = result.settingsApiKey
                        , configValidationMessage = result.configValidationMessage
                        , baseUrl = result.baseUrl
                        , apiKey = result.apiKey
                        , immichApiPaths = result.immichApiPaths
                        , knownAlbums = result.knownAlbums
                        , albumKeybindings = result.albumKeybindings
                        , albumsLoadState = result.albumsLoadState
                    }

                autoClearCmd =
                    if result.shouldAutoClear then
                        Process.sleep 3000
                            |> Task.perform (always (ConfigLoaded "clearSuccess" Nothing))

                    else
                        Cmd.none
            in
            if result.shouldInitializeImmich then
                ( finalModel, Cmd.batch [ getAllAlbums result.baseUrl result.apiKey |> Cmd.map ImmichMsg, autoClearCmd ] )

            else
                ( finalModel, autoClearCmd )

        ClearConfig ->
            ( { model
                | configuredApiUrl = Nothing
                , configuredApiKey = Nothing
                , settingsApiUrl = model.envBaseUrl
                , settingsApiKey = model.envApiKey
                , configValidationMessage = Nothing

                -- Clear albums when clearing config to avoid mixing data
                , knownAlbums = Dict.empty
                , albumKeybindings = Dict.empty
                , albumsLoadState = ImmichLoading
              }
            , clearStorage ()
            )

        UpdateSettingsApiUrl url ->
            ( { model | settingsApiUrl = url, configValidationMessage = Nothing }, Cmd.none )

        UpdateSettingsApiKey apiKey ->
            ( { model | settingsApiKey = apiKey, configValidationMessage = Nothing }, Cmd.none )

        SelectAlbum album ->
            case model.userMode of
                ViewAssets assetState ->
                    case assetState of
                        SelectAlbumInput _ ->
                            ( createLoadStateForCurrentAssetSource (Album album) model, Immich.getAlbum model.immichApiPaths album.id |> Cmd.map ImmichMsg )

                        EditAsset inputMode asset search ->
                            let
                                currentPropertyChange =
                                    Maybe.withDefault RemainFalse (Dict.get album.id asset.albumMembership)

                                currentlyInAlbum =
                                    ViewAlbums.isCurrentlyInAlbum currentPropertyChange

                                isNotInAlbum =
                                    not currentlyInAlbum

                                toggledAsset =
                                    ViewAlbums.toggleAssetAlbum asset album

                                newPropertyChange =
                                    Maybe.withDefault RemainFalse (Dict.get album.id toggledAsset.albumMembership)

                                isAddition =
                                    ViewAlbums.isAddingToAlbum newPropertyChange
                            in
                            ( { model | userMode = ViewAssets (EditAsset inputMode toggledAsset (ViewAlbums.getAlbumSearch "" model.knownAlbums)), pendingAlbumChanges = [ ( album.id, isAddition ) ] }, Immich.albumChangeAssetMembership model.immichApiPaths album.id [ asset.asset.id ] isNotInAlbum |> Cmd.map ImmichMsg )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyPress key ->
            -- Always reset modifier states on ANY Escape press (even Alt+Escape)
            if key == "Escape" then
                let
                    resetModel =
                        { model | controlPressed = False, altPressed = False }
                in
                -- Continue with normal Escape processing using reset model
                case resetModel.userMode of
                    MainMenu menuState ->
                        handleMenuResult (updateMenus (MenuKeyPress "Escape") menuState resetModel.knownAlbums resetModel.immichApiPaths resetModel.screenHeight) resetModel

                    ViewAssets assetState ->
                        handleAssetResult (updateAsset (AssetKeyPress "Escape") assetState resetModel.albumKeybindings resetModel.knownAlbums resetModel.screenHeight resetModel.currentAssets resetModel.knownAssets) resetModel

                    LoadingAssets _ ->
                        -- Save current state if transitioning from ViewAssets context
                        let
                            updatedModel =
                                case resetModel.currentAssetsSource of
                                    NoAssets ->
                                        resetModel

                                    _ ->
                                        -- We have asset context, save it
                                        let
                                            currentEntry =
                                                { userMode = resetModel.userMode
                                                , currentAssetsSource = resetModel.currentAssetsSource
                                                , currentAssets = resetModel.currentAssets
                                                , imageIndex = resetModel.imageIndex
                                                , paginationState = resetModel.paginationState
                                                }

                                            updatedBackStack =
                                                case resetModel.currentNavigationState of
                                                    Just existing ->
                                                        -- Push existing current state to back stack
                                                        List.take 19 (existing :: resetModel.navigationBackStack)

                                                    Nothing ->
                                                        resetModel.navigationBackStack
                                        in
                                        { resetModel
                                            | navigationBackStack = updatedBackStack
                                            , currentNavigationState = Just currentEntry
                                            , navigationForwardQueue = [] -- Clear forward queue
                                        }
                        in
                        ( { updatedModel | userMode = MainMenu MainMenuHome }, Cmd.none )

            else if key == "Control" then
                -- Control key pressed - just track state, don't pass to handlers
                ( { model | controlPressed = True }, Cmd.none )

            else if key == "Alt" then
                -- Alt key pressed - just track state, don't pass to handlers
                ( { model | altPressed = True }, Cmd.none )

            else
                let
                    -- If Control or Alt is pressed, prefix the key appropriately
                    effectiveKey =
                        if model.controlPressed then
                            "Control+" ++ key

                        else if model.altPressed then
                            "Alt+" ++ key

                        else
                            key
                in
                -- Handle navigation keys at top level (work in all views)
                case effectiveKey of
                    "Alt+o" ->
                        navigateHistoryBack model

                    "Alt+i" ->
                        navigateHistoryForward model

                    _ ->
                        case model.userMode of
                            MainMenu menuState ->
                                if effectiveKey == "T" && not (isInInputMode model.userMode) then
                                    ( { model | theme = Theme.nextTheme model.theme }, Cmd.none )

                                else
                                    handleMenuResult (updateMenus (MenuKeyPress effectiveKey) menuState model.knownAlbums model.immichApiPaths model.screenHeight) model

                            ViewAssets assetState ->
                                handleAssetResult (updateAsset (AssetKeyPress effectiveKey) assetState model.albumKeybindings model.knownAlbums model.screenHeight model.currentAssets model.knownAssets) model

                            LoadingAssets _ ->
                                -- Escape is already handled at the top of KeyPress
                                case effectiveKey of
                                    "g" ->
                                        ( { model | userMode = MainMenu Settings }, Cmd.none )

                                    "T" ->
                                        ( { model | theme = Theme.nextTheme model.theme }, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

        KeyRelease key ->
            if key == "Control" then
                -- Control key released - clear state
                ( { model | controlPressed = False }, Cmd.none )

            else if key == "Alt" then
                -- Alt key released - clear state
                ( { model | altPressed = False }, Cmd.none )

            else
                -- Ignore other key releases
                ( model, Cmd.none )

        VisibilityChanged visibility ->
            -- Reset modifier states when window visibility changes (handles Alt+Tab, etc.)
            ( { model | controlPressed = False, altPressed = False }, Cmd.none )

        WindowResize width height ->
            let
                newModel =
                    { model
                        | screenHeight = height
                        , screenWidth = width
                        , deviceClass = Theme.classifyDevice width height
                    }

                updatedModel =
                    case model.userMode of
                        ViewAssets assetState ->
                            case assetState of
                                SelectAlbumInput search ->
                                    { newModel | userMode = ViewAssets (SelectAlbumInput { search | pagination = ViewAlbums.updatePagination height search.pagination }) }

                                EditAsset inputMode asset search ->
                                    { newModel | userMode = ViewAssets (EditAsset inputMode asset { search | pagination = ViewAlbums.updatePagination height search.pagination }) }

                                CreateAlbumConfirmation inputMode asset search albumName ->
                                    { newModel | userMode = ViewAssets (CreateAlbumConfirmation inputMode asset { search | pagination = ViewAlbums.updatePagination height search.pagination } albumName) }

                                ShowEditAssetHelp inputMode asset search ->
                                    { newModel | userMode = ViewAssets (ShowEditAssetHelp inputMode asset { search | pagination = ViewAlbums.updatePagination height search.pagination }) }

                                GridView gridState ->
                                    let
                                        screenWidth =
                                            height * 16 // 9

                                        -- Assume 16:9 ratio
                                        updatedGridState =
                                            ViewGrid.updateGridState (ViewGrid.GridResized screenWidth height) gridState []
                                    in
                                    { newModel | userMode = ViewAssets (GridView updatedGridState) }

                                _ ->
                                    newModel

                        _ ->
                            newModel
            in
            ( updatedModel, Cmd.none )

        ChangeTimelineMediaType newMediaType ->
            ( updateMenuState (UpdateMenuConfig.updateTimelineMediaType newMediaType) model, Cmd.none )

        ChangeTimelineCategorisation newCategorisation ->
            ( updateMenuState (UpdateMenuConfig.updateTimelineCategorisation newCategorisation) model, Cmd.none )

        ChangeTimelineOrder newOrder ->
            ( updateMenuState (UpdateMenuConfig.updateTimelineOrder newOrder) model, Cmd.none )

        ChangeTimelineStatus newStatus ->
            ( updateMenuState (UpdateMenuConfig.updateTimelineStatus newStatus) model, Cmd.none )

        ChangeSearchMediaType newMediaType ->
            ( updateMenuState (UpdateMenuConfig.updateSearchMediaType newMediaType) model, Cmd.none )

        ChangeSearchContext newContext ->
            ( updateMenuState (UpdateMenuConfig.updateSearchContext newContext) model, Cmd.none )

        ChangeSearchStatus newStatus ->
            ( updateMenuState (UpdateMenuConfig.updateSearchStatus newStatus) model, Cmd.none )

        ChangeSearchQuery newQuery ->
            ( updateMenuState (UpdateMenuConfig.updateSearchQuery model.knownAssets newQuery) model, Cmd.none )

        SelectSearchSuggestion suggestion ->
            ( updateMenuState (UpdateMenuConfig.updateSearchSelectSuggestion suggestion) model, Cmd.none )

        ClearSearchQuery ->
            ( updateMenuState UpdateMenuConfig.updateSearchClear model, Cmd.none )

        ChangeAlbumMediaType newMediaType ->
            ( updateMenuState (UpdateMenuConfig.updateAlbumMediaType newMediaType) model, Cmd.none )

        ChangeAlbumOrder newOrder ->
            ( updateMenuState (UpdateMenuConfig.updateAlbumOrder newOrder) model, Cmd.none )

        ChangeAlbumStatus newStatus ->
            ( updateMenuState (UpdateMenuConfig.updateAlbumStatus newStatus) model, Cmd.none )

        LoadTimelineAssets ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        TimelineView config ->
                            let
                                searchConfig =
                                    { order = config.order, categorisation = config.categorisation, mediaType = config.mediaType, status = config.status }
                            in
                            let
                                updatedModel =
                                    createLoadStateForCurrentAssetSource (ImageSearch searchConfig) model

                                modelWithPagination =
                                    { updatedModel | paginationState = { currentConfig = Just searchConfig, currentQuery = Nothing, currentSearchContext = Nothing, currentAlbumContext = Nothing, totalAssets = 0, currentPage = 1, hasMorePages = False, isLoadingMore = False, loadedAssets = 0, maxAssetsToFetch = updatedModel.paginationState.maxAssetsToFetch } }
                            in
                            ( modelWithPagination, Immich.fetchImagesPaginated modelWithPagination.immichApiPaths searchConfig 1000 1 |> Cmd.map ImmichMsg )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ExecuteSearch ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        SearchView config ->
                            if String.isEmpty config.query then
                                ( model, Cmd.none )

                            else
                                let
                                    updatedRecentSearches =
                                        Menus.addToRecentSearches config.query config.recentSearches

                                    updatedConfig =
                                        { config | recentSearches = updatedRecentSearches }

                                    updatedModel =
                                        createLoadStateForCurrentAssetSource (TextSearch config.query config.searchContext) { model | userMode = MainMenu (SearchView updatedConfig) }

                                    modelWithPagination =
                                        { updatedModel | paginationState = { currentConfig = Nothing, currentQuery = Just config.query, currentSearchContext = Just config.searchContext, currentAlbumContext = Nothing, totalAssets = 0, currentPage = 1, hasMorePages = False, isLoadingMore = False, loadedAssets = 0, maxAssetsToFetch = updatedModel.paginationState.maxAssetsToFetch } }
                                in
                                ( modelWithPagination, Immich.searchAssetsPaginated modelWithPagination.immichApiPaths config.searchContext config.query config.mediaType config.status 1000 1 |> Cmd.map ImmichMsg )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoadAlbumAssets album ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        AlbumView _ config ->
                            ( createLoadStateForCurrentAssetSource (FilteredAlbum album config) model, Immich.fetchAlbumAssetsWithFilters model.immichApiPaths album.id config.order config.mediaType config.status |> Cmd.map ImmichMsg )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SearchInputFocused ->
            ( updateMenuState UpdateMenuConfig.updateSearchInputFocused model, Cmd.none )

        SearchInputBlurred ->
            ( updateMenuState UpdateMenuConfig.updateSearchInputBlurred model, Cmd.none )

        -- Module-specific message handlers (now removed - handled in KeyPress above)
        MenuMsg menuMsg ->
            -- This should no longer be called due to new architecture
            ( model, Cmd.none )

        AlbumMsg albumMsg ->
            -- This should no longer be called due to new architecture
            ( model, Cmd.none )

        AssetMsg assetMsg ->
            case model.userMode of
                ViewAssets assetState ->
                    handleAssetResult (updateAsset assetMsg assetState model.albumKeybindings model.knownAlbums model.screenHeight model.currentAssets model.knownAssets) model

                _ ->
                    ( model, Cmd.none )

        ToggleTheme ->
            ( { model | theme = Theme.nextTheme model.theme }, Cmd.none )

        ImmichMsg imsg ->
            let
                newModel =
                    case imsg of
                        Immich.SingleAlbumFetched (Ok album) ->
                            model
                                |> handleFetchAlbums False [ album ]
                                |> handleFetchAssets album.assets
                                -- |> handleProgressLoadingState FetchedAlbums
                                |> handleUpdateLoadingState FetchedAssetList

                        Immich.AlbumsFetched (Ok albums) ->
                            let
                                updatedModel =
                                    model |> handleFetchAlbums True albums
                            in
                            updatedModel

                        Immich.AlbumCreated (Ok album) ->
                            let
                                updatedModel =
                                    model
                                        |> handleFetchAlbums False [ album ]
                            in
                            updatedModel

                        -- |> handleProgressLoadingState FetchedAlbums
                        Immich.ImagesFetched (Ok assets) ->
                            let
                                updatedModel =
                                    model
                                        |> handleFetchAssets assets
                                        |> handleUpdateLoadingState FetchedAssetList
                            in
                            updatedModel

                        Immich.PaginatedImagesFetched (Ok paginatedResponse) ->
                            let
                                afterFetch =
                                    model |> handleFetchAssets paginatedResponse.assets

                                afterUpdate =
                                    afterFetch |> handleUpdateLoadingState FetchedAssetList

                                finalModel =
                                    afterUpdate |> updatePaginationState paginatedResponse 1
                            in
                            finalModel

                        Immich.MoreImagesFetched page (Ok paginatedResponse) ->
                            model
                                |> appendFetchedAssets paginatedResponse.assets
                                |> updatePaginationState paginatedResponse page

                        Immich.AssetMembershipFetched (Ok assetWithMembership) ->
                            model
                                |> handleFetchAssetMembership assetWithMembership

                        Immich.AssetMembershipFetched (Err httpError) ->
                            -- Asset membership fetch failed - log error and continue
                            { model | reloadFeedback = Just ("Album membership fetch failed: " ++ Immich.errorToString httpError) }

                        Immich.AlbumFetchedWithClientSideFiltering _ _ _ (Ok album) ->
                            -- Album fetched without assets - store album info and trigger paginated asset fetch
                            model
                                |> handleFetchAlbums False [ album ]

                        Immich.AlbumFetchedWithClientSideFiltering _ _ _ (Err error) ->
                            { model | imagesLoadState = ImmichLoadError error }

                        Immich.AssetUpdated (Ok updatedAsset) ->
                            { model | knownAssets = Dict.insert updatedAsset.id updatedAsset model.knownAssets }

                        Immich.BulkAssetsUpdated (Ok updatedAssets) ->
                            -- Update all bulk updated assets in knownAssets
                            let
                                updatedKnownAssets =
                                    List.foldl
                                        (\asset acc -> Dict.insert asset.id asset acc)
                                        model.knownAssets
                                        updatedAssets
                            in
                            { model | knownAssets = updatedKnownAssets }

                        Immich.AlbumsFetched (Err error) ->
                            { model | albumsLoadState = ImmichLoadError error }

                        Immich.AlbumCreated (Err error) ->
                            case model.userMode of
                                LoadingAssets _ ->
                                    getCurrentAssetWithActions model
                                        |> Maybe.map (\( assetWithActions, search ) -> { model | userMode = ViewAssets (EditAsset NormalMode assetWithActions search) })
                                        |> Maybe.withDefault model

                                _ ->
                                    model

                        Immich.ImagesFetched (Err error) ->
                            { model | imagesLoadState = ImmichLoadError error }

                        Immich.PaginatedImagesFetched (Err error) ->
                            { model | imagesLoadState = ImmichLoadError error }

                        Immich.MoreImagesFetched _ (Err error) ->
                            { model | imagesLoadState = ImmichLoadError error }

                        _ ->
                            model
            in
            case imsg of
                Immich.AlbumAssetsChanged (Ok _) ->
                    -- Album membership change succeeded, update album asset count and refresh membership data
                    let
                        -- Pop one pending change and update its album count
                        ( updatedModel, remainingChanges ) =
                            case newModel.pendingAlbumChanges of
                                ( albumId, isAddition ) :: rest ->
                                    let
                                        countChange =
                                            if isAddition then
                                                1

                                            else
                                                -1

                                        modelWithUpdatedCount =
                                            updateAlbumAssetCount albumId countChange newModel
                                    in
                                    ( { modelWithUpdatedCount | pendingAlbumChanges = rest }, rest )

                                [] ->
                                    ( newModel, [] )

                        -- Only fetch membership when ALL pending changes are processed
                        membershipCmd =
                            if List.isEmpty remainingChanges then
                                case updatedModel.userMode of
                                    ViewAssets assetState ->
                                        case assetState of
                                            EditAsset _ asset _ ->
                                                Immich.fetchMembershipForAsset updatedModel.immichApiPaths asset.asset.id |> Cmd.map ImmichMsg

                                            _ ->
                                                Cmd.none

                                    _ ->
                                        Cmd.none

                            else
                                Cmd.none
                    in
                    ( updatedModel, membershipCmd )

                Immich.AlbumAssetsChanged (Err _) ->
                    -- Album membership change failed, clear all pending changes and re-fetch to get correct state
                    switchToEditIfAssetFound { model | pendingAlbumChanges = [] } model.imageIndex

                Immich.AlbumsFetched (Ok albums) ->
                    let
                        clearFeedbackCmd =
                            if newModel.reloadFeedback /= Nothing then
                                Process.sleep 3000 |> Task.perform (always ClearReloadFeedback)

                            else
                                Cmd.none
                    in
                    ( newModel, clearFeedbackCmd )

                Immich.AlbumCreated (Ok album) ->
                    case model.userMode of
                        LoadingAssets _ ->
                            getCurrentAssetWithActions newModel
                                |> Maybe.map
                                    (\( assetWithActions, _ ) ->
                                        let
                                            updatedAsset =
                                                ViewAlbums.toggleAssetAlbum assetWithActions album

                                            updatedModel =
                                                { newModel | userMode = ViewAssets (EditAsset NormalMode updatedAsset (ViewAlbums.getAlbumSearch "" newModel.knownAlbums)) }
                                        in
                                        ( { updatedModel | pendingAlbumChanges = [ ( album.id, True ) ] }, Immich.albumChangeAssetMembership newModel.immichApiPaths album.id [ assetWithActions.asset.id ] True |> Cmd.map ImmichMsg )
                                    )
                                |> Maybe.withDefault ( newModel, Cmd.none )

                        _ ->
                            ( newModel, Cmd.none )

                Immich.PaginatedImagesFetched (Ok paginatedResponse) ->
                    processPaginatedResponse paginatedResponse 2 newModel

                Immich.MoreImagesFetched page (Ok paginatedResponse) ->
                    processPaginatedResponse paginatedResponse (page + 1) newModel

                Immich.PaginatedImagesFetched (Err _) ->
                    checkIfLoadingComplete newModel

                Immich.MoreImagesFetched _ (Err _) ->
                    checkIfLoadingComplete newModel

                Immich.AlbumFetchedWithClientSideFiltering order mediaType status (Ok album) ->
                    -- Trigger paginated fetch for album assets
                    let
                        fetchCmd =
                            Immich.fetchAlbumAssetsWithFilters newModel.immichApiPaths album.id order mediaType status
                                |> Cmd.map ImmichMsg
                    in
                    ( newModel, fetchCmd )

                _ ->
                    checkIfLoadingComplete newModel


handleFetchAssetMembership : Immich.AssetWithMembership -> Model -> Model
handleFetchAssetMembership assetWithMembership model =
    case Dict.get assetWithMembership.assetId model.knownAssets of
        Nothing ->
            model

        Just asset ->
            let
                newAsset =
                    { asset | albumMembership = assetWithMembership.albumIds }

                updatedModel =
                    { model | knownAssets = Dict.insert assetWithMembership.assetId newAsset model.knownAssets }

                -- Check if we're currently viewing this asset and need to update the view state
                currentAssetId =
                    updatedModel.currentAssets
                        |> List.drop updatedModel.imageIndex
                        |> List.head

                isCurrentlyViewingThisAsset =
                    currentAssetId == Just assetWithMembership.assetId

                finalModel =
                    if isCurrentlyViewingThisAsset then
                        case updatedModel.userMode of
                            ViewAssets (EditAsset inputMode currentAsset search) ->
                                let
                                    updatedAsset =
                                        ViewAlbums.getAssetWithActions newAsset
                                            |> (\a -> { a | isVideoLoaded = currentAsset.isVideoLoaded })
                                in
                                { updatedModel | userMode = ViewAssets (EditAsset inputMode updatedAsset search) }

                            ViewAssets (CreateAlbumConfirmation inputMode currentAsset search albumName) ->
                                let
                                    updatedAsset =
                                        ViewAlbums.getAssetWithActions newAsset
                                            |> (\a -> { a | isVideoLoaded = currentAsset.isVideoLoaded })
                                in
                                { updatedModel | userMode = ViewAssets (CreateAlbumConfirmation inputMode updatedAsset search albumName) }

                            ViewAssets (ShowEditAssetHelp inputMode currentAsset search) ->
                                let
                                    updatedAsset =
                                        ViewAlbums.getAssetWithActions newAsset
                                            |> (\a -> { a | isVideoLoaded = currentAsset.isVideoLoaded })
                                in
                                { updatedModel | userMode = ViewAssets (ShowEditAssetHelp inputMode updatedAsset search) }

                            _ ->
                                updatedModel

                    else
                        updatedModel
            in
            finalModel


handleFetchAssets : List ImmichAsset -> Model -> Model
handleFetchAssets assets model =
    let
        result =
            UpdateImmich.handleFetchAssetsResult
                { assets = assets
                , currentConfig = model.paginationState.currentConfig
                , currentKnownAssets = model.knownAssets
                , currentImageIndex = model.imageIndex
                }

        updatedModel =
            { model
                | knownAssets = result.knownAssets
                , currentAssets = result.currentAssets
                , imagesLoadState = result.imagesLoadState
                , imageIndex = result.imageIndex
            }
    in
    if result.isTimelineView then
        case model.userMode of
            ViewAssets _ ->
                Tuple.first (switchToEditIfAssetFound updatedModel 0)

            _ ->
                updatedModel

    else
        updatedModel



-- KEYBINDING GENERATION --
-- All keybinding functions are now imported from KeybindingGenerator module


handleFetchAlbums : Bool -> List ImmichAlbum -> Model -> Model
handleFetchAlbums showReloadFeedback albums model =
    let
        result =
            UpdateImmich.handleFetchAlbumsResult
                { showReloadFeedback = showReloadFeedback
                , albums = albums
                , currentKnownAlbums = model.knownAlbums
                , currentReloadFeedback = model.reloadFeedback
                }
    in
    { model
        | knownAlbums = result.knownAlbums
        , albumsLoadState = result.albumsLoadState
        , albumKeybindings = result.albumKeybindings
        , reloadFeedback = result.reloadFeedback
    }


handleUpdateLoadingState : AssetSourceUpdate -> Model -> Model
handleUpdateLoadingState updateType model =
    case model.userMode of
        LoadingAssets loadState ->
            { model | userMode = LoadingAssets (LoadState.updateLoadStateForFetch updateType loadState) }

        _ ->
            model


checkIfLoadingComplete : Model -> ( Model, Cmd Msg )
checkIfLoadingComplete model =
    case model.userMode of
        LoadingAssets loadState ->
            if LoadState.isLoadStateCompleted loadState then
                switchToEditIfAssetFound model 0

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


createLoadStateForCurrentAssetSource : AssetSource -> Model -> Model
createLoadStateForCurrentAssetSource assetSource model =
    case LoadState.createInitialLoadState assetSource of
        Nothing ->
            model

        Just loadState ->
            { model | currentAssetsSource = assetSource, userMode = LoadingAssets loadState }



-- Helper to convert UpdateMenus.AssetSource to Main.AssetSource


updateAlbumAssetCount : ImmichAlbumId -> Int -> Model -> Model
updateAlbumAssetCount albumId countChange model =
    { model | knownAlbums = UpdateImmich.updateAlbumAssetCount albumId countChange model.knownAlbums }



-- Helper to process paginated response and auto-fetch next page if needed


processPaginatedResponse : Immich.PaginatedAssetResponse -> Int -> Model -> ( Model, Cmd Msg )
processPaginatedResponse paginatedResponse nextPage model =
    let
        -- Clear loading state since we just received a response
        ps =
            model.paginationState

        clearedPaginationState =
            { ps | isLoadingMore = False }

        modelWithClearedLoading =
            { model | paginationState = clearedPaginationState }

        -- Use pure function to determine if we should fetch more
        shouldFetchMore =
            Pagination.shouldFetchNextPage paginatedResponse clearedPaginationState

        modelWithLoadingState =
            if shouldFetchMore then
                { modelWithClearedLoading | paginationState = { clearedPaginationState | isLoadingMore = True } }

            else
                modelWithClearedLoading

        -- Use pure function to compute next page request, then generate Cmd
        nextPageCmd =
            if shouldFetchMore then
                Pagination.computeNextPageRequest nextPage clearedPaginationState
                    |> Maybe.map (generateNextPageCmd model.immichApiPaths)
                    |> Maybe.withDefault Cmd.none

            else
                Cmd.none
    in
    if shouldFetchMore then
        ( modelWithLoadingState, nextPageCmd )

    else
        -- No more pages to fetch, check if loading is complete and transition if needed
        checkIfLoadingComplete modelWithLoadingState


{-| Generate a Cmd for fetching the next page based on NextPageRequest.
This is the impure wrapper that converts pure pagination decisions to Cmds.
-}
generateNextPageCmd : ImmichApiPaths -> Pagination.NextPageRequest -> Cmd Msg
generateNextPageCmd apiPaths request =
    case request.requestType of
        Pagination.TimelineRequest config ->
            Immich.fetchImagesPaginated apiPaths config request.pageSize request.nextPage
                |> Cmd.map ImmichMsg

        Pagination.TextSearchRequest query searchContext ->
            Immich.searchAssetsPaginated apiPaths searchContext query AllMedia AllStatuses request.pageSize request.nextPage
                |> Cmd.map ImmichMsg

        Pagination.AlbumRequest albumCtx ->
            Immich.fetchAlbumAssetsPaginated apiPaths albumCtx.albumId albumCtx.order albumCtx.mediaType albumCtx.status request.pageSize request.nextPage
                |> Cmd.map ImmichMsg



-- Normalize asset PropertyChange states after successful API call


switchToEditIfAssetFound : Model -> ImageIndex -> ( Model, Cmd Msg )
switchToEditIfAssetFound model index =
    -- Use pure function to compute asset view state
    case AssetNavigation.buildAssetViewState model.currentAssets index model.knownAssets model.knownAlbums model.screenHeight of
        AssetNavigation.AssetFound result ->
            let
                -- Preserve video loaded state if viewing same asset (pure function)
                assetWithActions =
                    AssetNavigation.preserveVideoLoadedState model.userMode result.asset result.assetWithActions

                newViewAssetsMode =
                    ViewAssets (EditAsset NormalMode assetWithActions result.albumSearch)

                -- Record navigation state when transitioning to ViewAssets
                updatedModel =
                    { model | imageIndex = index, userMode = newViewAssetsMode }
                        |> recordNavigationState newViewAssetsMode
                        |> setCurrentNavigationState

                -- Generate Cmd for fetching membership (impure part stays here)
                cmdToSend =
                    Immich.fetchMembershipForAsset model.immichApiPaths result.asset.id |> Cmd.map ImmichMsg
            in
            ( updatedModel, cmdToSend )

        AssetNavigation.AssetNotFound ->
            ( createLoadStateForCurrentAssetSource model.currentAssetsSource model, Cmd.none )


switchToAssetWithoutHistory : Model -> ImageIndex -> ( Model, Cmd Msg )
switchToAssetWithoutHistory model index =
    -- Use pure function to compute asset view state
    case AssetNavigation.buildAssetViewState model.currentAssets index model.knownAssets model.knownAlbums model.screenHeight of
        AssetNavigation.AssetFound result ->
            let
                newViewAssetsMode =
                    ViewAssets (EditAsset NormalMode result.assetWithActions result.albumSearch)

                -- Update current history entry with new index, don't create new entry
                updatedModel =
                    updateCurrentHistoryEntry { model | imageIndex = index, userMode = newViewAssetsMode }

                -- Generate Cmd for fetching membership (impure part stays here)
                cmdToSend =
                    Immich.fetchMembershipForAsset model.immichApiPaths result.asset.id |> Cmd.map ImmichMsg
            in
            ( updatedModel, cmdToSend )

        AssetNavigation.AssetNotFound ->
            ( model, Cmd.none )


getCurrentAssetWithActions : Model -> Maybe ( AssetWithActions, AlbumSearch )
getCurrentAssetWithActions model =
    Navigation.getCurrentAssetWithActions
        model.currentAssets
        model.imageIndex
        model.knownAssets
        model.knownAlbums
        model.screenHeight



-- PAGINATION HELPERS --


updatePaginationState : Immich.PaginatedAssetResponse -> Int -> Model -> Model
updatePaginationState paginatedResponse page model =
    { model
        | paginationState =
            Pagination.updatePaginationStateFromResponse paginatedResponse page model.paginationState
    }


appendFetchedAssets : List ImmichAsset -> Model -> Model
appendFetchedAssets newAssets model =
    let
        result =
            Pagination.appendAssetsResult
                newAssets
                model.knownAssets
                model.currentAssets
                model.imageIndex
                model.paginationState.currentConfig

        updatedModel =
            { model
                | knownAssets = result.knownAssets
                , currentAssets = result.currentAssets
                , imageIndex = result.imageIndex
            }
    in
    -- For timeline views, sync ViewAsset state if currently viewing an asset
    if result.isTimelineView then
        case model.userMode of
            ViewAssets _ ->
                Tuple.first (switchToEditIfAssetFound updatedModel 0)

            _ ->
                updatedModel

    else
        updatedModel



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.map KeyPress (Decode.field "key" Decode.string))
        , onKeyUp (Decode.map KeyRelease (Decode.field "key" Decode.string))
        , onResize WindowResize
        , onVisibilityChange VisibilityChanged
        , storageLoaded (\( key, value ) -> ConfigLoaded key value)
        ]


main : Program Flags Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
