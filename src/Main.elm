port module Main exposing (main)

import Browser exposing (element)
import Browser.Events exposing (onKeyDown, onKeyUp, onResize)
import Date
import Dict exposing (Dict)
import Element exposing (Element, alignRight, alignTop, clipY, column, el, fill, fillPortion, height, minimum, paddingXY, px, row, text, width)
import Element.Background as Background
import Element.Font as Font
import HelpText exposing (AlbumBrowseState(..), ViewContext(..), viewContextHelp)
import Helpers
import Html exposing (Html)
import Immich exposing (CategorisationFilter(..), ImageOrder(..), ImageSearchConfig, ImmichAlbum, ImmichAlbumId, ImmichApiPaths, ImmichAsset, ImmichAssetId, ImmichLoadState(..), MediaTypeFilter(..), StatusFilter(..), getAllAlbums, getImmichApiPaths)
import Json.Decode as Decode
import KeybindBranches exposing (generateAlbumKeybindings)
import Menus exposing (AlbumConfig, SearchContext, defaultAlbumConfig, defaultSearchConfig, filterByMediaType, filterByStatus)
import Process
import Task
import UpdateAlbums exposing (AlbumMsg)
import UpdateAsset exposing (AssetMsg(..), AssetResult(..), AssetState(..), updateAsset)
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


type DeviceClass
    = Mobile
    | Tablet
    | Desktop


type Theme
    = Light
    | Dark
    | System


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
    , immichApiPaths : ImmichApiPaths
    , screenHeight : Int
    , screenWidth : Int
    , deviceClass : DeviceClass
    , theme : Theme
    , pendingAlbumChange : Maybe ( ImmichAlbumId, Bool ) -- (albumId, isAddition)
    , paginationState : PaginationState
    }


type alias PaginationState =
    { currentConfig : Maybe ImageSearchConfig
    , currentQuery : Maybe String
    , totalAssets : Int
    , currentPage : Int
    , hasMorePages : Bool
    , isLoadingMore : Bool
    , loadedAssets : Int
    , maxAssetsToFetch : Int -- Configurable limit
    }


type AssetSource
    = NoAssets
    | ImageSearch ImageSearchConfig
    | TextSearch String
    | Album ImmichAlbum
    | FilteredAlbum ImmichAlbum AlbumConfig


type alias SourceLoadState =
    { fetchedAssetList : Maybe Bool
    , fetchedAssetMembership : Maybe Bool
    }


type AssetSourceUpdate
    = FetchedAssetList



-- | FetchedAlbums


type UserMode
    = MainMenu MenuState
    | ViewAssets AssetState
    | LoadingAssets SourceLoadState


type alias ImageIndex =
    Int


classifyDevice : Int -> Int -> DeviceClass
classifyDevice width height =
    if width < 768 then
        Mobile

    else if width < 1024 then
        Tablet

    else
        Desktop


nextTheme : Theme -> Theme
nextTheme currentTheme =
    case currentTheme of
        Light ->
            Dark

        Dark ->
            System

        System ->
            Light


getBackgroundColor : Theme -> Element.Color
getBackgroundColor theme =
    case theme of
        Light ->
            Element.rgb 0.98 0.98 0.98

        Dark ->
            Element.rgb 0.1 0.1 0.1

        System ->
            ViewAlbums.usefulColours "darkgrey"



-- Default


getTextColor : Theme -> Element.Color
getTextColor theme =
    case theme of
        Light ->
            Element.rgb 0.1 0.1 0.1

        Dark ->
            Element.rgb 0.9 0.9 0.9

        System ->
            Element.rgb 0.1 0.1 0.1



-- Default


getSecondaryColor : Theme -> Element.Color
getSecondaryColor theme =
    case theme of
        Light ->
            Element.rgb 0.6 0.6 0.6

        Dark ->
            Element.rgb 0.5 0.5 0.5

        System ->
            Element.rgb 0.6 0.6 0.6



-- Default


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
      , immichApiPaths = getImmichApiPaths flags.immichApiUrl flags.immichApiKey
      , screenHeight = 800 -- Default, will be updated by window resize
      , screenWidth = 1200 -- Default, will be updated by window resize
      , deviceClass = classifyDevice 1200 800 -- Will be updated by WindowResize
      , theme = System -- Default to system theme
      , pendingAlbumChange = Nothing
      , paginationState =
            { currentConfig = Nothing
            , currentQuery = Nothing
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
        ]
    )


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
                        AllStatuses ->
                            ""

                        FavoritesOnly ->
                            " [favourites]"

                        ArchivedOnly ->
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
                        AllStatuses ->
                            ""

                        FavoritesOnly ->
                            " [favourites]"

                        ArchivedOnly ->
                            " [archived]"

                hasFilters =
                    config.mediaType /= AllMedia || config.status /= AllStatuses || config.order /= CreatedDesc
            in
            if hasFilters then
                "Album \"" ++ album.albumName ++ "\"" ++ statusText ++ mediaText ++ " " ++ orderText

            else
                "Album \"" ++ album.albumName ++ "\""

        NoAssets ->
            ""


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
                                el [ Font.color <| Element.fromRgb { red = 0.5, green = 0.5, blue = 0.5, alpha = 1 }, Font.size 12 ] <| text ("Next: " ++ nextCharString)
                            ]

                      else
                        text ""
                    , case search.invalidInputWarning of
                        Just warning ->
                            el [ Font.color <| Element.fromRgb { red = 1, green = 0.2, blue = 0.2, alpha = 1 }, Font.size 12 ] <| text ("Invalid: \"" ++ warning ++ "\"")

                        Nothing ->
                            text ""
                    , ViewAlbums.viewSidebarAlbums search model.albumKeybindings model.knownAlbums SelectAlbum
                    ]
                , Element.column [ width (fillPortion 5), height fill, paddingXY 20 20 ]
                    [ el [ Font.size 16 ] (text "Select an album from the left to configure and view its contents.")
                    , el [ Font.size 14, Font.color <| Element.fromRgb { red = 0.6, green = 0.6, blue = 0.6, alpha = 1 } ] (text "Type album name or keybinding to filter the list.")
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
                (ViewAlbums.viewSidebarAlbums search model.albumKeybindings model.knownAlbums SelectAlbum)
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
            ViewAlbums.viewWithSidebar (ViewAlbums.viewSidebar asset search model.albumKeybindings model.knownAlbums (Just inputMode) SelectAlbum) (ViewAsset.viewEditAsset model.immichApiPaths model.apiKey model.imageIndex (List.length model.currentAssets) viewTitle asset model.currentAssets model.knownAssets model.currentDateMillis model.timeViewMode inputMode)

        CreateAlbumConfirmation _ asset search albumName ->
            ViewAlbums.viewWithSidebar (ViewAlbums.viewSidebar asset search model.albumKeybindings model.knownAlbums Nothing SelectAlbum) (ViewAsset.viewCreateAlbumConfirmation albumName)

        ShowEditAssetHelp inputMode asset search ->
            ViewAlbums.viewWithSidebar (ViewAlbums.viewSidebar asset search model.albumKeybindings model.knownAlbums (Just inputMode) SelectAlbum) (ViewAsset.viewEditAssetHelp inputMode)

        GridView gridState ->
            ViewAsset.viewGridAssets model.immichApiPaths model.apiKey gridState model.currentAssets model.knownAssets model.paginationState.hasMorePages model.paginationState.isLoadingMore (AssetMsg << AssetGridMsg)


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



-- UPDATE --
-- Helper function to handle MenuResult


handleMenuResult : MenuResult msg -> Model -> ( Model, Cmd Msg )
handleMenuResult menuResult model =
    case menuResult of
        StayInMenu newMenuState ->
            ( { model | userMode = MainMenu newMenuState }, Cmd.none )

        MenuLoadAssets assetSource ->
            -- Convert to Main's AssetSource and load assets
            let
                mainAssetSource =
                    convertMenuAssetSource assetSource

                loadModel =
                    createLoadStateForCurrentAssetSource mainAssetSource model

                -- Set pagination state based on asset source
                ( modelWithPagination, loadCmd ) =
                    case assetSource of
                        UpdateMenus.ImageSearch searchConfig ->
                            let
                                paginatedModel =
                                    { loadModel | paginationState = { currentConfig = Just searchConfig, currentQuery = Nothing, totalAssets = 0, currentPage = 1, hasMorePages = False, isLoadingMore = False, loadedAssets = 0, maxAssetsToFetch = loadModel.paginationState.maxAssetsToFetch } }
                            in
                            ( paginatedModel, Immich.fetchImagesPaginated paginatedModel.immichApiPaths searchConfig 1000 1 |> Cmd.map ImmichMsg )

                        UpdateMenus.TextSearch query ->
                            let
                                paginatedModel =
                                    { loadModel | paginationState = { currentConfig = Nothing, currentQuery = Just query, totalAssets = 0, currentPage = 1, hasMorePages = False, isLoadingMore = False, loadedAssets = 0, maxAssetsToFetch = loadModel.paginationState.maxAssetsToFetch } }
                            in
                            ( paginatedModel, Immich.searchAssetsPaginated paginatedModel.immichApiPaths query AllMedia AllStatuses 1000 1 |> Cmd.map ImmichMsg )

                        UpdateMenus.FilteredAlbum album config ->
                            ( loadModel, Immich.fetchAlbumAssetsWithFilters loadModel.immichApiPaths album.id config.order config.mediaType config.status |> Cmd.map ImmichMsg )
            in
            ( modelWithPagination, loadCmd )

        MenuReloadAlbums ->
            ( model, Immich.getAllAlbums model.baseUrl model.apiKey |> Cmd.map ImmichMsg )

        MenuUpdateSearchInput focused ->
            -- Handle search input focus change
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        SearchView config ->
                            ( { model | userMode = MainMenu (SearchView { config | inputFocused = focused }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- Helper function to handle AssetResult


handleAssetResult : AssetResult msg -> Model -> ( Model, Cmd Msg )
handleAssetResult assetResult model =
    case assetResult of
        StayInAssets newAssetState ->
            ( { model | userMode = ViewAssets newAssetState }, Cmd.none )

        GoToMainMenu ->
            ( { model | userMode = MainMenu MainMenuHome }, Cmd.none )

        GoToSearchView query ->
            ( { model | userMode = MainMenu (SearchView { defaultSearchConfig | query = query }) }, Cmd.none )

        AssetLoadTextSearch query ->
            let
                mainAssetSource =
                    TextSearch query

                loadModel =
                    createLoadStateForCurrentAssetSource mainAssetSource model

                loadCmd =
                    Immich.searchAssetsPaginated model.immichApiPaths query AllMedia AllStatuses 1000 1 |> Cmd.map ImmichMsg
            in
            ( loadModel, loadCmd )

        AssetLoadAlbum album ->
            let
                mainAssetSource =
                    Album album

                loadModel =
                    createLoadStateForCurrentAssetSource mainAssetSource model

                loadCmd =
                    Immich.getAlbum model.immichApiPaths album.id |> Cmd.map ImmichMsg
            in
            ( loadModel, loadCmd )

        AssetSwitchToAssetIndex newIndex ->
            switchToEditIfAssetFound model newIndex

        AssetToggleFavorite ->
            -- Handle favorite toggle
            case model.userMode of
                ViewAssets assetState ->
                    case assetState of
                        EditAsset inputMode asset search ->
                            let
                                newAsset =
                                    { asset | isFavourite = ViewAlbums.flipPropertyChange asset.isFavourite }

                                newIsFavorite =
                                    case newAsset.isFavourite of
                                        ChangeToTrue ->
                                            True

                                        RemainTrue ->
                                            True

                                        ChangeToFalse ->
                                            False

                                        RemainFalse ->
                                            False
                            in
                            ( { model | userMode = ViewAssets (EditAsset inputMode newAsset search) }, Immich.updateAssetFavorite model.immichApiPaths asset.asset.id newIsFavorite |> Cmd.map ImmichMsg )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AssetToggleArchived ->
            -- Handle archived toggle
            case model.userMode of
                ViewAssets assetState ->
                    case assetState of
                        EditAsset inputMode asset search ->
                            let
                                newAsset =
                                    { asset | isArchived = ViewAlbums.flipPropertyChange asset.isArchived }

                                newIsArchived =
                                    case newAsset.isArchived of
                                        ChangeToTrue ->
                                            True

                                        RemainTrue ->
                                            True

                                        ChangeToFalse ->
                                            False

                                        RemainFalse ->
                                            False
                            in
                            ( { model | userMode = ViewAssets (EditAsset inputMode newAsset search) }, Immich.updateAssetArchived model.immichApiPaths asset.asset.id newIsArchived |> Cmd.map ImmichMsg )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AssetToggleAlbumMembership album ->
            -- Handle album membership toggle
            case model.userMode of
                ViewAssets assetState ->
                    case assetState of
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

                                newSearch =
                                    { search | partialKeybinding = "", pagination = ViewAlbums.resetPagination search.pagination, invalidInputWarning = Nothing }
                            in
                            ( { model | userMode = ViewAssets (EditAsset NormalMode toggledAsset newSearch), pendingAlbumChange = Just ( album.id, isAddition ) }, Immich.albumChangeAssetMembership model.immichApiPaths album.id [ asset.asset.id ] isNotInAlbum |> Cmd.map ImmichMsg )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AssetOpenInImmich ->
            -- Handle opening in Immich
            case model.userMode of
                ViewAssets assetState ->
                    case assetState of
                        EditAsset _ asset _ ->
                            let
                                immichUrl =
                                    model.baseUrl ++ "/photos/" ++ asset.asset.id
                            in
                            ( model, openUrl immichUrl )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AssetYankToClipboard ->
            -- Handle yanking asset to clipboard
            case model.userMode of
                ViewAssets assetState ->
                    case assetState of
                        EditAsset _ asset _ ->
                            ( model, yankAssetToClipboard asset.asset.id )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AssetCreateAlbum albumName ->
            -- Handle album creation
            ( { model | userMode = LoadingAssets { fetchedAssetList = Nothing, fetchedAssetMembership = Nothing } }, Immich.createAlbum model.immichApiPaths albumName |> Cmd.map ImmichMsg )

        AssetToggleTimeView ->
            -- Handle time view toggle
            let
                newTimeViewMode =
                    case model.timeViewMode of
                        Absolute ->
                            Relative

                        Relative ->
                            Absolute
            in
            ( { model | timeViewMode = newTimeViewMode }, Cmd.none )

        AssetSwitchToGridView ->
            -- Switch to grid view
            let
                -- Assume screen width is roughly 16:9 ratio of height for now
                screenWidth =
                    model.screenHeight * 16 // 9

                gridState =
                    ViewGrid.initGridState screenWidth model.screenHeight
            in
            ( { model | userMode = ViewAssets (GridView gridState) }, Cmd.none )

        AssetSwitchToDetailView assetId ->
            -- Switch to detail view for specific asset
            case
                List.indexedMap
                    (\index id ->
                        if id == assetId then
                            Just index

                        else
                            Nothing
                    )
                    model.currentAssets
                    |> List.filterMap identity
                    |> List.head
            of
                Just assetIndex ->
                    switchToEditIfAssetFound model assetIndex

                Nothing ->
                    ( model, Cmd.none )

        AssetGridUpdate gridState ->
            -- Update grid state
            ( { model | userMode = ViewAssets (GridView gridState) }, Cmd.none )

        AssetBulkFavorite assetIds isFavorite ->
            -- Handle bulk favorite toggle
            let
                bulkCmd =
                    Immich.bulkUpdateAssetsFavorite model.immichApiPaths assetIds isFavorite |> Cmd.map ImmichMsg
            in
            ( model, bulkCmd )

        AssetBulkArchive assetIds isArchived ->
            -- Handle bulk archive toggle
            let
                bulkCmd =
                    Immich.bulkUpdateAssetsArchived model.immichApiPaths assetIds isArchived |> Cmd.map ImmichMsg
            in
            ( model, bulkCmd )

        AssetBulkAddToAlbum assetIds albumId ->
            -- Handle bulk add to album
            let
                bulkCmd =
                    Immich.albumChangeAssetMembership model.immichApiPaths albumId assetIds True |> Cmd.map ImmichMsg
            in
            ( model, bulkCmd )

        AssetBulkRemoveFromAlbum assetIds albumId ->
            -- Handle bulk remove from album
            let
                bulkCmd =
                    Immich.albumChangeAssetMembership model.immichApiPaths albumId assetIds False |> Cmd.map ImmichMsg
            in
            ( model, bulkCmd )

        AssetRequestLoadMore ->
            -- Handle infinite scroll load more request
            let
                paginationState =
                    model.paginationState

                nextPage =
                    paginationState.currentPage + 1

                loadMoreCmd =
                    case ( paginationState.currentConfig, paginationState.currentQuery ) of
                        ( Just config, Nothing ) ->
                            -- Load more for timeline view (preserves ordering)
                            Immich.fetchImagesPaginated model.immichApiPaths config 1000 nextPage |> Cmd.map ImmichMsg

                        ( Nothing, Just query ) ->
                            -- Load more for text search
                            Immich.searchAssetsPaginated model.immichApiPaths query AllMedia AllStatuses 1000 nextPage |> Cmd.map ImmichMsg

                        _ ->
                            Cmd.none

                updatedPaginationState =
                    { paginationState | isLoadingMore = True }
            in
            ( { model | paginationState = updatedPaginationState }, loadMoreCmd )

        AssetReloadAlbums ->
            -- Reload albums while staying in ViewAsset mode
            ( model, Immich.getAllAlbums model.baseUrl model.apiKey |> Cmd.map ImmichMsg )



-- Helper to convert UpdateMenus.AssetSource to Main.AssetSource


convertMenuAssetSource : UpdateMenus.AssetSource -> AssetSource
convertMenuAssetSource menuAssetSource =
    case menuAssetSource of
        UpdateMenus.ImageSearch config ->
            ImageSearch config

        UpdateMenus.TextSearch query ->
            TextSearch query

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
            let
                validation =
                    validateConfig url apiKey
            in
            case validation of
                Just errorMsg ->
                    ( { model | configValidationMessage = Just errorMsg }, Cmd.none )

                Nothing ->
                    ( { model
                        | configValidationMessage = Just "Saving configuration..."
                        , configuredApiUrl = Just url
                        , configuredApiKey = Just apiKey
                        , baseUrl = url
                        , apiKey = apiKey
                        , immichApiPaths = getImmichApiPaths url apiKey
                      }
                    , Cmd.batch
                        [ saveToStorage ( "immichApiUrl", url )
                        , saveToStorage ( "immichApiKey", apiKey )
                        , Process.sleep 1000
                            |> Task.perform (always (ConfigLoaded "saveSuccess" (Just "✅ Configuration saved successfully!")))
                        ]
                    )

        LoadConfig key ->
            ( model, loadFromStorage key )

        ConfigLoaded key maybeValue ->
            let
                updatedModel =
                    case key of
                        "immichApiUrl" ->
                            { model
                                | configuredApiUrl = maybeValue
                                , settingsApiUrl = maybeValue |> Maybe.withDefault model.settingsApiUrl
                                , configValidationMessage =
                                    if maybeValue /= Nothing then
                                        Just "✅ Configuration saved successfully!"

                                    else
                                        Nothing
                            }

                        "immichApiKey" ->
                            { model
                                | configuredApiKey = maybeValue
                                , settingsApiKey = maybeValue |> Maybe.withDefault model.settingsApiKey
                                , configValidationMessage =
                                    if maybeValue /= Nothing then
                                        Just "✅ Configuration loaded from storage"

                                    else
                                        Nothing
                            }

                        "saveSuccess" ->
                            { model
                                | configValidationMessage = maybeValue
                            }

                        "clearSuccess" ->
                            { model
                                | configValidationMessage = Nothing
                            }

                        _ ->
                            model

                -- If we have both URL and API key configured, initialize Immich
                shouldInitializeImmich =
                    case ( updatedModel.configuredApiUrl, updatedModel.configuredApiKey ) of
                        ( Just url, Just apiKey ) ->
                            True

                        _ ->
                            False

                -- Use configured values if available, otherwise fall back to flags
                finalUrl =
                    updatedModel.configuredApiUrl
                        |> Maybe.withDefault model.baseUrl

                finalApiKey =
                    updatedModel.configuredApiKey
                        |> Maybe.withDefault model.apiKey

                finalModel =
                    { updatedModel
                        | baseUrl = finalUrl
                        , apiKey = finalApiKey
                        , immichApiPaths = getImmichApiPaths finalUrl finalApiKey
                    }

                autoClearCmd =
                    if key == "saveSuccess" && maybeValue /= Nothing then
                        Process.sleep 3000
                            |> Task.perform (always (ConfigLoaded "clearSuccess" Nothing))

                    else
                        Cmd.none
            in
            if shouldInitializeImmich && model.albumsLoadState == ImmichLoading then
                ( finalModel, Cmd.batch [ getAllAlbums finalUrl finalApiKey |> Cmd.map ImmichMsg, autoClearCmd ] )

            else
                ( finalModel, autoClearCmd )

        ClearConfig ->
            ( { model 
                | configuredApiUrl = Nothing
                , configuredApiKey = Nothing
                , settingsApiUrl = ""
                , settingsApiKey = ""
                , configValidationMessage = Nothing
              }, clearStorage () )

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
                            ( { model | userMode = ViewAssets (EditAsset inputMode toggledAsset (ViewAlbums.getAlbumSearch "" model.knownAlbums)), pendingAlbumChange = Just ( album.id, isAddition ) }, Immich.albumChangeAssetMembership model.immichApiPaths album.id [ asset.asset.id ] isNotInAlbum |> Cmd.map ImmichMsg )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyPress key ->
            if key == "Control" then
                -- Control key pressed - just track state, don't pass to handlers
                ( { model | controlPressed = True }, Cmd.none )

            else
                let
                    -- If Control is pressed, prefix the key with "Control+"
                    effectiveKey =
                        if model.controlPressed then
                            "Control+" ++ key

                        else
                            key
                in
                case model.userMode of
                    MainMenu menuState ->
                        handleMenuResult (updateMenus (MenuKeyPress effectiveKey) menuState model.knownAlbums model.immichApiPaths model.screenHeight) model

                    ViewAssets assetState ->
                        handleAssetResult (updateAsset (AssetKeyPress effectiveKey) assetState model.albumKeybindings model.knownAlbums model.screenHeight model.currentAssets model.knownAssets) model

                    LoadingAssets _ ->
                        case effectiveKey of
                            "Escape" ->
                                ( { model | userMode = MainMenu MainMenuHome }, Cmd.none )

                            "g" ->
                                ( { model | userMode = MainMenu Settings }, Cmd.none )

                            "T" ->
                                ( { model | theme = nextTheme model.theme }, Cmd.none )

                            _ ->
                                ( model, Cmd.none )

        KeyRelease key ->
            if key == "Control" then
                -- Control key released - clear state
                ( { model | controlPressed = False }, Cmd.none )

            else
                -- Ignore other key releases
                ( model, Cmd.none )

        WindowResize width height ->
            let
                newModel =
                    { model
                        | screenHeight = height
                        , screenWidth = width
                        , deviceClass = classifyDevice width height
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
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        TimelineView config ->
                            ( { model | userMode = MainMenu (TimelineView { config | mediaType = newMediaType }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeTimelineCategorisation newCategorisation ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        TimelineView config ->
                            ( { model | userMode = MainMenu (TimelineView { config | categorisation = newCategorisation }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeTimelineOrder newOrder ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        TimelineView config ->
                            ( { model | userMode = MainMenu (TimelineView { config | order = newOrder }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeTimelineStatus newStatus ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        TimelineView config ->
                            ( { model | userMode = MainMenu (TimelineView { config | status = newStatus }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeSearchMediaType newMediaType ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        SearchView config ->
                            ( { model | userMode = MainMenu (SearchView { config | mediaType = newMediaType }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeSearchContext newContext ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        SearchView config ->
                            ( { model | userMode = MainMenu (SearchView { config | searchContext = newContext }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeSearchStatus newStatus ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        SearchView config ->
                            ( { model | userMode = MainMenu (SearchView { config | status = newStatus }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeSearchQuery newQuery ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        SearchView config ->
                            let
                                suggestions =
                                    if String.length newQuery > 1 then
                                        Menus.generateSearchSuggestions model.knownAssets

                                    else
                                        []

                                updatedConfig =
                                    { config
                                        | query = newQuery
                                        , suggestions = suggestions
                                        , showSuggestions = String.length newQuery > 1
                                    }
                            in
                            ( { model | userMode = MainMenu (SearchView updatedConfig) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectSearchSuggestion suggestion ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        SearchView config ->
                            let
                                updatedRecentSearches =
                                    Menus.addToRecentSearches suggestion config.recentSearches

                                updatedConfig =
                                    { config
                                        | query = suggestion
                                        , recentSearches = updatedRecentSearches
                                        , showSuggestions = False
                                    }
                            in
                            ( { model | userMode = MainMenu (SearchView updatedConfig) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClearSearchQuery ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        SearchView config ->
                            let
                                updatedConfig =
                                    { config
                                        | query = ""
                                        , showSuggestions = False
                                    }
                            in
                            ( { model | userMode = MainMenu (SearchView updatedConfig) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeAlbumMediaType newMediaType ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        AlbumView album config ->
                            ( { model | userMode = MainMenu (AlbumView album { config | mediaType = newMediaType }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeAlbumOrder newOrder ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        AlbumView album config ->
                            ( { model | userMode = MainMenu (AlbumView album { config | order = newOrder }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeAlbumStatus newStatus ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        AlbumView album config ->
                            ( { model | userMode = MainMenu (AlbumView album { config | status = newStatus }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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
                                    { updatedModel | paginationState = { currentConfig = Just searchConfig, currentQuery = Nothing, totalAssets = 0, currentPage = 1, hasMorePages = False, isLoadingMore = False, loadedAssets = 0, maxAssetsToFetch = updatedModel.paginationState.maxAssetsToFetch } }
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
                                        createLoadStateForCurrentAssetSource (TextSearch config.query) { model | userMode = MainMenu (SearchView updatedConfig) }

                                    modelWithPagination =
                                        { updatedModel | paginationState = { currentConfig = Nothing, currentQuery = Just config.query, totalAssets = 0, currentPage = 1, hasMorePages = False, isLoadingMore = False, loadedAssets = 0, maxAssetsToFetch = updatedModel.paginationState.maxAssetsToFetch } }
                                in
                                ( modelWithPagination, Immich.searchAssetsPaginated modelWithPagination.immichApiPaths config.query config.mediaType config.status 1000 1 |> Cmd.map ImmichMsg )

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
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        SearchView config ->
                            ( { model | userMode = MainMenu (SearchView { config | inputFocused = True }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SearchInputBlurred ->
            case model.userMode of
                MainMenu menuState ->
                    case menuState of
                        SearchView config ->
                            ( { model | userMode = MainMenu (SearchView { config | inputFocused = False }) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        -- Module-specific message handlers (now removed - handled in KeyPress above)
        MenuMsg menuMsg ->
            -- This should no longer be called due to new architecture
            ( model, Cmd.none )

        AlbumMsg albumMsg ->
            -- This should no longer be called due to new architecture
            ( model, Cmd.none )

        AssetMsg assetMsg ->
            -- This should no longer be called due to new architecture
            ( model, Cmd.none )

        ToggleTheme ->
            ( { model | theme = nextTheme model.theme }, Cmd.none )

        ImmichMsg imsg ->
            let
                newModel =
                    case imsg of
                        Immich.SingleAlbumFetched (Ok album) ->
                            model
                                |> handleFetchAlbums [ album ]
                                |> handleFetchAssets album.assets
                                -- |> handleProgressLoadingState FetchedAlbums
                                |> handleUpdateLoadingState FetchedAssetList

                        Immich.AlbumsFetched (Ok albums) ->
                            let
                                updatedModel =
                                    model |> handleFetchAlbums albums

                                clearFeedbackCmd =
                                    if updatedModel.reloadFeedback /= Nothing then
                                        Process.sleep 3000 |> Task.perform (always ClearReloadFeedback)

                                    else
                                        Cmd.none
                            in
                            updatedModel

                        Immich.AlbumCreated (Ok album) ->
                            let
                                updatedModel =
                                    model
                                        |> handleFetchAlbums [ album ]
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

                        Immich.AlbumFetchedWithClientSideFiltering order mediaType status (Ok album) ->
                            let
                                -- Filter and sort album assets client-side since API doesn't respect orderBy with albumIds
                                filteredAssets =
                                    album.assets
                                        |> Menus.filterByMediaType mediaType
                                        |> Menus.filterByStatus status
                                        |> (case order of
                                                CreatedAsc ->
                                                    List.sortBy (.fileCreatedAt >> Date.toRataDie)

                                                CreatedDesc ->
                                                    List.sortBy (.fileCreatedAt >> Date.toRataDie) >> List.reverse

                                                ModifiedAsc ->
                                                    List.sortBy (.fileModifiedAt >> Date.toRataDie)

                                                ModifiedDesc ->
                                                    List.sortBy (.fileModifiedAt >> Date.toRataDie) >> List.reverse

                                                Random ->
                                                    identity
                                           )

                                updatedModel =
                                    if List.isEmpty filteredAssets then
                                        -- Handle empty results - show a message instead of going to EditAssets
                                        { model
                                            | userMode = MainMenu (AlbumView album defaultAlbumConfig)
                                            , imagesLoadState = ImmichLoadSuccess
                                        }

                                    else
                                        model
                                            |> handleFetchAlbums [ album ]
                                            |> handleFetchAssets filteredAssets
                                            |> handleUpdateLoadingState FetchedAssetList
                            in
                            updatedModel

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
                        updatedModel =
                            case newModel.pendingAlbumChange of
                                Just ( albumId, isAddition ) ->
                                    let
                                        countChange =
                                            if isAddition then
                                                1

                                            else
                                                -1

                                        modelWithUpdatedCount =
                                            updateAlbumAssetCount albumId countChange { newModel | pendingAlbumChange = Nothing }
                                    in
                                    modelWithUpdatedCount

                                Nothing ->
                                    { newModel | pendingAlbumChange = Nothing }

                        -- Fetch fresh membership data to ensure all album memberships are current
                        membershipCmd =
                            case updatedModel.userMode of
                                ViewAssets assetState ->
                                    case assetState of
                                        EditAsset _ asset _ ->
                                            Immich.fetchMembershipForAsset updatedModel.immichApiPaths asset.asset.id |> Cmd.map ImmichMsg

                                        _ ->
                                            Cmd.none

                                _ ->
                                    Cmd.none
                    in
                    ( updatedModel, membershipCmd )

                Immich.AlbumAssetsChanged (Err _) ->
                    -- Album membership change failed, clear pending change and re-fetch to get correct state
                    switchToEditIfAssetFound { model | pendingAlbumChange = Nothing } model.imageIndex

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
                                        ( { updatedModel | pendingAlbumChange = Just ( album.id, True ) }, Immich.albumChangeAssetMembership newModel.immichApiPaths album.id [ assetWithActions.asset.id ] True |> Cmd.map ImmichMsg )
                                    )
                                |> Maybe.withDefault ( newModel, Cmd.none )

                        _ ->
                            ( newModel, Cmd.none )

                Immich.PaginatedImagesFetched (Ok paginatedResponse) ->
                    -- Auto-fetch next page if there are more assets
                    let
                        -- Clear loading state since we just received a response
                        currentPaginationState1 =
                            newModel.paginationState

                        clearedPaginationState =
                            { currentPaginationState1 | isLoadingMore = False }

                        modelWithClearedLoading =
                            { newModel | paginationState = clearedPaginationState }

                        -- Use the paginatedResponse data directly, not the processed model
                        newLoadedAssets =
                            modelWithClearedLoading.paginationState.loadedAssets

                        reachedLimit =
                            newLoadedAssets >= modelWithClearedLoading.paginationState.maxAssetsToFetch

                        shouldFetchMore =
                            paginatedResponse.hasNextPage && not reachedLimit

                        modelWithLoadingState =
                            if shouldFetchMore then
                                let
                                    ps =
                                        modelWithClearedLoading.paginationState
                                in
                                { modelWithClearedLoading | paginationState = { ps | isLoadingMore = True } }

                            else
                                modelWithClearedLoading

                        nextPageCmd =
                            if shouldFetchMore then
                                case ( modelWithClearedLoading.paginationState.currentConfig, modelWithClearedLoading.paginationState.currentQuery ) of
                                    ( Just config, Nothing ) ->
                                        Immich.fetchImagesPaginated modelWithClearedLoading.immichApiPaths config 1000 2 |> Cmd.map ImmichMsg

                                    ( Nothing, Just query ) ->
                                        Immich.searchAssetsPaginated modelWithClearedLoading.immichApiPaths query AllMedia AllStatuses 1000 2 |> Cmd.map ImmichMsg

                                    _ ->
                                        Cmd.none

                            else
                                Cmd.none
                    in
                    ( modelWithLoadingState, nextPageCmd )

                Immich.MoreImagesFetched page (Ok paginatedResponse) ->
                    -- Auto-fetch next page if there are more assets
                    let
                        -- Clear loading state since we just received a response
                        currentPaginationState1 =
                            newModel.paginationState

                        clearedPaginationState =
                            { currentPaginationState1 | isLoadingMore = False }

                        modelWithClearedLoading =
                            { newModel | paginationState = clearedPaginationState }

                        -- Use the paginatedResponse data directly, not the processed model
                        newLoadedAssets =
                            modelWithClearedLoading.paginationState.loadedAssets

                        reachedLimit =
                            newLoadedAssets >= modelWithClearedLoading.paginationState.maxAssetsToFetch

                        shouldFetchMore =
                            paginatedResponse.hasNextPage && not reachedLimit

                        modelWithLoadingState =
                            if shouldFetchMore then
                                let
                                    ps =
                                        modelWithClearedLoading.paginationState
                                in
                                { modelWithClearedLoading | paginationState = { ps | isLoadingMore = True } }

                            else
                                modelWithClearedLoading

                        nextPageCmd =
                            if shouldFetchMore then
                                case ( modelWithClearedLoading.paginationState.currentConfig, modelWithClearedLoading.paginationState.currentQuery ) of
                                    ( Just config, Nothing ) ->
                                        Immich.fetchImagesPaginated modelWithClearedLoading.immichApiPaths config 1000 (page + 1) |> Cmd.map ImmichMsg

                                    ( Nothing, Just query ) ->
                                        Immich.searchAssetsPaginated modelWithClearedLoading.immichApiPaths query AllMedia AllStatuses 1000 (page + 1) |> Cmd.map ImmichMsg

                                    _ ->
                                        Cmd.none

                            else
                                Cmd.none
                    in
                    ( modelWithLoadingState, nextPageCmd )

                Immich.PaginatedImagesFetched (Err _) ->
                    checkIfLoadingComplete newModel

                Immich.MoreImagesFetched _ (Err _) ->
                    checkIfLoadingComplete newModel

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
            in
            Tuple.first <| switchToEditIfAssetFound { model | knownAssets = Dict.insert assetWithMembership.assetId newAsset model.knownAssets } model.imageIndex


handleFetchAssets : List ImmichAsset -> Model -> Model
handleFetchAssets assets model =
    let
        -- Apply client-side sorting since Immich API doesn't respect orderBy properly
        sortedAssets =
            case model.paginationState.currentConfig of
                Just config ->
                    -- Apply sorting for timeline views
                    applySortingToAssets config.order assets

                Nothing ->
                    -- Keep original order for other views (album/search views handle sorting separately)
                    assets
    in
    case model.paginationState.currentConfig of
        Just _ ->
            let
                updatedModel =
                    { model
                        | knownAssets = Helpers.listOverrideDict sortedAssets (\a -> ( a.id, a )) model.knownAssets
                        , currentAssets = List.map .id sortedAssets
                        , imagesLoadState = ImmichLoadSuccess
                        , imageIndex = 0

                        -- Preserve paginationState - don't overwrite it
                    }
            in
            -- Timeline view with sorting - jump to first asset and sync ViewAsset state
            case model.userMode of
                ViewAssets _ ->
                    -- Currently viewing an asset, sync the view to show asset at index 0
                    Tuple.first (switchToEditIfAssetFound updatedModel 0)

                _ ->
                    -- Not currently viewing an asset, just return updated model
                    updatedModel

        Nothing ->
            -- No timeline sorting, preserve current index
            { model
                | knownAssets = Helpers.listOverrideDict sortedAssets (\a -> ( a.id, a )) model.knownAssets
                , currentAssets = List.map .id sortedAssets
                , imagesLoadState = ImmichLoadSuccess

                -- Preserve paginationState - don't overwrite it
            }


applySortingToAssets : ImageOrder -> List ImmichAsset -> List ImmichAsset
applySortingToAssets order assets =
    let
        sortedAssets =
            case order of
                CreatedAsc ->
                    List.sortWith
                        (\a b ->
                            case compare a.fileCreatedAtString b.fileCreatedAtString of
                                EQ ->
                                    compare a.id b.id

                                -- Secondary sort by ID for predictable ordering
                                other ->
                                    other
                        )
                        assets

                CreatedDesc ->
                    List.sortWith
                        (\a b ->
                            case compare b.fileCreatedAtString a.fileCreatedAtString of
                                -- b first for descending
                                EQ ->
                                    compare a.id b.id

                                -- Secondary sort by ID for predictable ordering
                                other ->
                                    other
                        )
                        assets

                ModifiedAsc ->
                    List.sortWith
                        (\a b ->
                            case compare a.fileModifiedAtString b.fileModifiedAtString of
                                EQ ->
                                    compare a.id b.id

                                -- Secondary sort by ID for predictable ordering
                                other ->
                                    other
                        )
                        assets

                ModifiedDesc ->
                    List.sortWith
                        (\a b ->
                            let
                                stringComparison =
                                    compare b.fileModifiedAtString a.fileModifiedAtString

                                -- b first for descending
                            in
                            case stringComparison of
                                EQ ->
                                    compare a.id b.id

                                -- Secondary sort by ID for predictable ordering
                                other ->
                                    other
                        )
                        assets

                Random ->
                    assets
    in
    sortedAssets



-- Keep original order for random
-- KEYBINDING GENERATION --
-- All keybinding functions are now imported from KeybindingGenerator module


handleFetchAlbums : List ImmichAlbum -> Model -> Model
handleFetchAlbums albums model =
    let
        updatedKnownAlbums =
            Helpers.listOverrideDict albums (\a -> ( a.id, a )) model.knownAlbums

        allAlbums =
            Dict.values updatedKnownAlbums

        albumKeybindings =
            generateAlbumKeybindings allAlbums

        albumCount =
            List.length albums

        isFirstLoad =
            Dict.isEmpty model.knownAlbums

        feedbackMessage =
            if albumCount > 0 then
                let
                    actionText =
                        if isFirstLoad then
                            "Loaded"

                        else
                            "Reloaded"
                in
                Just (actionText ++ " " ++ String.fromInt albumCount ++ " albums")

            else
                Just "No albums found"
    in
    { model
        | knownAlbums = updatedKnownAlbums
        , albumsLoadState = ImmichLoadSuccess
        , albumKeybindings = albumKeybindings
        , reloadFeedback = feedbackMessage
    }


handleUpdateLoadingState : AssetSourceUpdate -> Model -> Model
handleUpdateLoadingState updateType model =
    -- Use the event to update the Loading AssetLoadState
    -- Check if all the flags are now good, if so call progressToEditMode
    case model.userMode of
        LoadingAssets loadState ->
            let
                updatedLoadState =
                    case updateType of
                        FetchedAssetList ->
                            { loadState | fetchedAssetList = Just True }

                -- FetchedAlbums ->
                --     { loadState | fetchedAssetList = Just True }
                updatedModel =
                    { model | userMode = LoadingAssets updatedLoadState }

                -- Check if loading is complete and transition to ViewAssets
                ( finalModel, _ ) =
                    checkIfLoadingComplete updatedModel
            in
            finalModel

        _ ->
            model


checkIfLoadingComplete : Model -> ( Model, Cmd Msg )
checkIfLoadingComplete model =
    case model.userMode of
        LoadingAssets loadState ->
            let
                isCompleted =
                    isLoadStateCompleted loadState
            in
            if isCompleted then
                switchToEditIfAssetFound model 0

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


isLoadStateCompleted : SourceLoadState -> Bool
isLoadStateCompleted loadState =
    isLoadCompletedForProp loadState.fetchedAssetMembership
        && isLoadCompletedForProp loadState.fetchedAssetList


isLoadCompletedForProp : Maybe Bool -> Bool
isLoadCompletedForProp maybeBool =
    maybeBool == Nothing || maybeBool == Just True


createLoadStateForCurrentAssetSource : AssetSource -> Model -> Model
createLoadStateForCurrentAssetSource assetSource model =
    case assetSource of
        NoAssets ->
            model

        ImageSearch _ ->
            { model | currentAssetsSource = assetSource, userMode = LoadingAssets { fetchedAssetList = Just False, fetchedAssetMembership = Nothing } }

        Album _ ->
            { model | currentAssetsSource = assetSource, userMode = LoadingAssets { fetchedAssetList = Just False, fetchedAssetMembership = Nothing } }

        FilteredAlbum _ _ ->
            { model | currentAssetsSource = assetSource, userMode = LoadingAssets { fetchedAssetList = Just False, fetchedAssetMembership = Nothing } }

        TextSearch _ ->
            { model | currentAssetsSource = assetSource, userMode = LoadingAssets { fetchedAssetList = Just False, fetchedAssetMembership = Nothing } }



-- Helper to convert UpdateMenus.AssetSource to Main.AssetSource


updateAlbumAssetCount : ImmichAlbumId -> Int -> Model -> Model
updateAlbumAssetCount albumId countChange model =
    let
        updatedAlbums =
            Dict.update albumId
                (\maybeAlbum ->
                    case maybeAlbum of
                        Just album ->
                            Just { album | assetCount = max 0 (album.assetCount + countChange) }

                        Nothing ->
                            Nothing
                )
                model.knownAlbums
    in
    { model | knownAlbums = updatedAlbums }



-- Normalize asset PropertyChange states after successful API call


normalizeAssetMembershipStates : Model -> ImmichAlbumId -> Bool -> Model
normalizeAssetMembershipStates model albumId isAddition =
    let
        newStableState =
            if isAddition then
                RemainTrue

            else
                RemainFalse

        -- Update the current asset in userMode if it's ViewAssets EditAsset
        updatedUserMode =
            case model.userMode of
                ViewAssets assetState ->
                    case assetState of
                        EditAsset inputMode asset search ->
                            let
                                updatedAsset =
                                    { asset
                                        | albumMembership =
                                            Dict.update albumId
                                                (\_ -> Just newStableState)
                                                asset.albumMembership
                                    }
                            in
                            ViewAssets (EditAsset inputMode updatedAsset search)

                        _ ->
                            model.userMode

                _ ->
                    model.userMode
    in
    { model | userMode = updatedUserMode }


switchToEditIfAssetFound : Model -> ImageIndex -> ( Model, Cmd Msg )
switchToEditIfAssetFound model index =
    model.currentAssets
        |> List.drop index
        |> List.head
        |> Maybe.andThen (\id -> Dict.get id model.knownAssets)
        |> Maybe.map
            (\asset ->
                let
                    cmdToSend =
                        -- if List.isEmpty asset.albumMembership then
                        Immich.fetchMembershipForAsset model.immichApiPaths asset.id |> Cmd.map ImmichMsg

                    -- else
                    --     Cmd.none
                in
                ( { model | imageIndex = index, userMode = ViewAssets (EditAsset NormalMode (ViewAlbums.getAssetWithActions asset) (ViewAlbums.getAlbumSearchWithHeight "" model.knownAlbums model.screenHeight)) }, cmdToSend )
            )
        |> Maybe.withDefault ( createLoadStateForCurrentAssetSource model.currentAssetsSource model, Cmd.none )


getCurrentAssetWithActions : Model -> Maybe ( AssetWithActions, AlbumSearch )
getCurrentAssetWithActions model =
    model.currentAssets
        |> List.drop model.imageIndex
        |> List.head
        |> Maybe.andThen (\id -> Dict.get id model.knownAssets)
        |> Maybe.map (\asset -> ( ViewAlbums.getAssetWithActions asset, ViewAlbums.getAlbumSearchWithHeight "" model.knownAlbums model.screenHeight ))



-- PAGINATION HELPERS --


updatePaginationState : Immich.PaginatedAssetResponse -> Int -> Model -> Model
updatePaginationState paginatedResponse page model =
    let
        newLoadedAssets =
            model.paginationState.loadedAssets + paginatedResponse.count

        reachedLimit =
            newLoadedAssets >= model.paginationState.maxAssetsToFetch

        hasMoreToLoad =
            paginatedResponse.hasNextPage && not reachedLimit
    in
    { model
        | paginationState =
            { currentConfig = model.paginationState.currentConfig
            , currentQuery = model.paginationState.currentQuery
            , totalAssets = paginatedResponse.total
            , currentPage = page
            , hasMorePages = hasMoreToLoad
            , isLoadingMore = model.paginationState.isLoadingMore -- Keep existing loading state
            , loadedAssets = newLoadedAssets
            , maxAssetsToFetch = model.paginationState.maxAssetsToFetch
            }
    }


appendFetchedAssets : List ImmichAsset -> Model -> Model
appendFetchedAssets newAssets model =
    let
        updatedKnownAssets =
            Helpers.listOverrideDict newAssets (\a -> ( a.id, a )) model.knownAssets

        existingAssetIds =
            model.currentAssets

        newAssetIds =
            List.map .id newAssets

        combinedAssetIds =
            existingAssetIds ++ newAssetIds

        -- Re-sort all assets if we're in timeline view (since Immich API doesn't sort properly)
        finalAssetIds =
            case model.paginationState.currentConfig of
                Just config ->
                    -- Get all assets and re-sort them
                    let
                        allAssets =
                            combinedAssetIds
                                |> List.filterMap (\id -> Dict.get id updatedKnownAssets)

                        sortedAssets =
                            applySortingToAssets config.order allAssets
                    in
                    List.map .id sortedAssets

                Nothing ->
                    combinedAssetIds
    in
    case model.paginationState.currentConfig of
        Just _ ->
            let
                updatedModel =
                    { model
                        | knownAssets = updatedKnownAssets
                        , currentAssets = finalAssetIds
                        , imageIndex = 0
                    }
            in
            -- Timeline view with re-sorting - jump to first asset and sync ViewAsset state
            case model.userMode of
                ViewAssets _ ->
                    -- Currently viewing an asset, sync the view to show asset at index 0
                    Tuple.first (switchToEditIfAssetFound updatedModel 0)

                _ ->
                    -- Not currently viewing an asset, just return updated model
                    updatedModel

        Nothing ->
            -- No timeline re-sorting, preserve current index
            { model
                | knownAssets = updatedKnownAssets
                , currentAssets = finalAssetIds
            }



-- VALIDATION --


validateConfig : String -> String -> Maybe String
validateConfig url apiKey =
    if String.isEmpty (String.trim url) then
        Just "URL cannot be empty"

    else if String.isEmpty (String.trim apiKey) then
        Just "API key cannot be empty"

    else if not (String.startsWith "http" url) then
        Just "URL must start with http:// or https://"

    else if String.length apiKey < 10 then
        Just "API key appears to be too short"

    else
        Nothing



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.map KeyPress (Decode.field "key" Decode.string))
        , onKeyUp (Decode.map KeyRelease (Decode.field "key" Decode.string))
        , onResize WindowResize
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
