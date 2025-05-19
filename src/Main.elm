module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Browser.Events exposing (onKeyDown)
import Date
import Dict exposing (Dict)
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, centerY, column, el, fill, fillPortion, height, paddingXY, row, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Helpers exposing (regexFromString, send)
import Html exposing (Html, node)
import Html.Attributes
import Http
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichApiPaths, ImmichAsset, ImmichAssetId, ImmichLoadState(..), errorToString, getAllAlbums, getImmichApiPaths)
import Json.Decode as Decode
import Regex


type Msg
    = KeyPress String
    | ImmichMsg Immich.Msg
    | LoadDataAgain

type alias Flags =
    { test : Int
    , imagePrepend : String
    , immichApiKey : String
    , immichApiUrl : String
    }

type alias Model =
    { key : String
    , imagePrepend : String
    , currentAssetsSource : AssetSource
    , userMode : UserMode
    , test : Int
    , imageIndex : ImageIndex
    , debugging : Bool
    -- Immich fields
    , currentAssets : List ImmichAssetId
    , knownAssets : Dict ImmichAssetId ImmichAsset
    , imagesLoadState : ImmichLoadState
    , knownAlbums : Dict ImmichAlbumId ImmichAlbum
    , albumsLoadState : ImmichLoadState
    , apiUrl : String
    , apiKey : String
    , immichApiPaths : ImmichApiPaths
    }

type AssetSource
    = NoAssets
    | Uncategorised
    | Search String
    | Album ImmichAlbum

type alias SourceLoadState =
    { fetchedAssetList : Maybe Bool
    , fetchedAssetMembership : Maybe Bool
    }

type AssetSourceUpdate
    = FetchedAssetList



-- | FetchedAlbums

type UserMode
    = MainMenu
    | SearchAssetInput SearchString
    | SelectAlbumInput AlbumSearch
    | LoadingAssets SourceLoadState
    | EditAsset InputMode AssetWithActions AlbumSearch

type alias SearchString =
    String

type alias ImageIndex =
    Int

type alias AssetWithActions =
    { asset : ImmichAsset
    , isFavourite : PropertyChange
    , isArchived : PropertyChange
    , albumMembership : Dict ImmichAlbumId PropertyChange
    }

type alias AlbumSearch =
    { searchString : String
    , albumScores : Dict ImmichAlbumId Int
    }

type InputMode
    = NormalMode
    | InsertMode

type AssetChange
    = ToggleAlbum ImmichAlbum
    | ToggleDelete
    | ToggleFavourite

type TextInputUpdate
    = TextInputAddition String
    | TextInputBackspace

type UserActionGeneral
    = ChangeUserModeToEditAsset
    | ChangeUserModeToLoading AssetSource
    | ChangeUserModeToMainMenu
    | ChangeUserModeToSearchAsset
    | ChangeUserModeToSelectAlbum
    | ReloadData
    | UnknownAction

type UserActionSearchMode
    = TextInputUpdate TextInputUpdate
    | UserActionGeneralSearch UserActionGeneral

type UserActionAlbumSelectMode
    = TextSelectInputUpdate TextInputUpdate
    | SelectAlbumIfMatching
    | UserActionGeneralAlbumSelect UserActionGeneral

type UserActionEditMode
    = ChangeInputMode InputMode
    | ChangeImageIndex Int
    | TextEditModeInputUpdate TextInputUpdate
    | ApplyAlbumIfMatching
    | AssetChange AssetChange
    | UserActionGeneralEdit UserActionGeneral

type PropertyChange
    = RemainTrue
    | RemainFalse
    | ChangeToTrue
    | ChangeToFalse

flipPropertyChange : PropertyChange -> PropertyChange
flipPropertyChange propertyChange =
    case propertyChange of
        RemainTrue ->
            ChangeToFalse

        RemainFalse ->
            ChangeToTrue

        ChangeToTrue ->
            RemainFalse

        ChangeToFalse ->
            RemainTrue

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { key = ""
      , imagePrepend = flags.imagePrepend
      , userMode = MainMenu
      , currentAssetsSource = NoAssets
      , test = flags.test
      , imageIndex = 0
      , debugging = False
      -- Immich fields
      , currentAssets = []
      , knownAssets = Dict.empty
      , imagesLoadState = ImmichLoading
      , knownAlbums = Dict.empty
      , albumsLoadState = ImmichLoading
      , apiUrl = flags.immichApiUrl
      , apiKey = flags.immichApiKey
      , immichApiPaths = getImmichApiPaths flags.immichApiUrl flags.immichApiKey
      }
      -- , Cmd.none
    , getAllAlbums flags.immichApiUrl flags.immichApiKey |> Cmd.map ImmichMsg
    )

getDebugAssets : ( List ImmichAsset, List ImmichAlbum )
getDebugAssets =
    ( [ ImmichAsset "0001" "images/imafight.jpg" "Imafight" "image/jpg" False False []
      , ImmichAsset "0002" "images/dcemployees.jpg" "dcemployees" "image/jpg" False False []
      , ImmichAsset "0003" "images/jordan.jpg" "Jordan" "image/jpg" False False []
      , ImmichAsset "0004" "images/router-password.mp4" "router password" "video/mp4" False False []
      ]
    , [ ImmichAlbum "a" "J" 200 [] (Date.fromOrdinalDate 2025 1)
      , ImmichAlbum "b" "ToBeSorted" 3000 [] (Date.fromOrdinalDate 2025 1)
      , ImmichAlbum "c" "The World" 50 [] (Date.fromOrdinalDate 2025 1)
      , ImmichAlbum "d" "The Other One" 75 [] (Date.fromOrdinalDate 2025 1)
      , ImmichAlbum "e" "Comics" 50 [] (Date.fromOrdinalDate 2025 1)
      ]
    )


usefulColours : String -> Element.Color
usefulColours name =
    case name of
        "red" ->
            Element.fromRgb { red = 1, green = 0, blue = 0, alpha = 1 }
        "green" ->
            Element.fromRgb { red = 0, green = 1, blue = 0, alpha = 1 }
        "blue" ->
            Element.fromRgb { red = 0, green = 0, blue = 1, alpha = 1 }
        "grey" ->
            Element.fromRgb { red = 0.8, green = 0.8, blue = 0.8, alpha = 0.6 }
        "darkgrey" ->
            Element.fromRgb { red = 0.2, green = 0.2, blue = 0.2, alpha = 0.4 }
        "black" ->
            Element.fromRgb { red = 0, green = 0, blue = 0, alpha = 1 }
        _ ->
            Element.fromRgb { red = 0, green = 0, blue = 0, alpha = 1 }


-- VIEW --
-- showAlbumsForImage : List ImmichAlbum -> ImageWithMetadata -> Html msg
-- showAlbumsForImage albums image =
--     let
--         inAlbumns =
--             List.filter (\album -> List.member album.id image.inAlbumns) albums
--
--         notInAlbumns =
--             List.filter (\album -> not (List.member album.id image.inAlbumns)) albums
--     in
--     div []
--         [ div []
--             [ text ("In Albumns: " ++ String.join ", " (List.map (\album -> album.name) inAlbumns)) ]
--         , div []
--             [ text ("Not In Albumns: " ++ String.join ", " (List.map (\album -> album.name) notInAlbumns)) ]
--         ]


viewAsset : ImmichApiPaths -> ImmichAsset -> Element msg
viewAsset apiPaths asset =
    if asset.path == "" then
        el [] (text "No Asset")

    else if assetIsImage asset then
        viewImage asset apiPaths
    else if assetIsVideo asset then
        viewVideo asset apiPaths

    else
        text (String.join " " [ "Error with", asset.title, "Unknown mimetype:", asset.mimeType ])

assetIsImage : ImmichAsset -> Bool
assetIsImage asset =
    List.member asset.mimeType [ "image/jpg", "image/jpeg", "image/png", "image/gif" ]

viewImage : ImmichAsset -> ImmichApiPaths -> Element msg
viewImage asset apiPaths =
    column [ width fill, height fill ]
        [ -- Element.image [ centerY, centerX ] { src = path, description = "" }
          el [ centerX ] <|
            Element.html
                (node "image-from-api"
                    [ Html.Attributes.attribute "asset-url" (apiPaths.downloadAsset asset.id)
                    , Html.Attributes.attribute "api-key" apiPaths.apiKey
                    , Html.Attributes.class "center"
                    ]
                    []
                )

        --(text asset.title)
        ]

assetIsVideo : ImmichAsset -> Bool
assetIsVideo asset =
    List.member asset.mimeType [ "video/mp4", "video/quicktime" ]

viewVideo : ImmichAsset -> ImmichApiPaths -> Element msg
viewVideo asset apiPaths =
    column [ width fill, height fill ]
        [ -- Element.image [ centerY, centerX ] { src = path, description = "" }
          el [ centerX ] <|
            Element.html
                (node "video-from-api"
                    [ Html.Attributes.attribute "asset-url" (apiPaths.downloadAsset asset.id)
                    , Html.Attributes.attribute "api-key" apiPaths.apiKey
                    , Html.Attributes.class "center"
                    ]
                    []
                )

        --(text asset.title)
        ]


-- viewVideo : List (Element.Attribute msg) -> { poster : String, source : String } -> Element msg
-- viewVideo attrs { poster, source } =
--     el attrs <|
--         Element.html <|
--             Html.video
--                 [ Html.Attributes.attribute "controls" "controls"
--                 , Html.Attributes.preload "none"
--                 -- , Html.Attributes.poster poster
--                 , Html.Attributes.autoplay True
--                 , Html.Attributes.loop True
--                 ]
--                 [ Html.source
--                     [ Html.Attributes.id "mp4"
--                     , Html.Attributes.src source
--                     , Html.Attributes.type_ "video/mp4"
--                     ]
--                     []
--                 ]

viewEditAsset : ImmichApiPaths -> AssetWithActions -> Element Msg
viewEditAsset apiPaths currentAsset =
    column [ width fill, height fill ]
        [ el [ alignTop ] (text "Top Bar")
        , viewAsset apiPaths currentAsset.asset
        , column [ alignBottom ] []
        -- [ text (String.fromInt index ++ "  ")
        -- ]
        ]

viewLoadingAssets : ImmichLoadState -> Element Msg
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


view : Model -> Html Msg
view model =
    Element.layout [ width fill, height fill, Background.color <| usefulColours "darkgrey" ] <|
        viewWithInputBottomBar model.userMode <|
            viewMainWindow model


viewWithInputBottomBar : UserMode -> Element Msg -> Element Msg
viewWithInputBottomBar userMode viewMain =
    column [ width fill, height fill ]
        [ el [ width fill, height <| fillPortion 15 ] viewMain
        , el [ width fill ] <| viewInputMode userMode
        ]


viewMainWindow : Model -> Element Msg
viewMainWindow model =
    case model.userMode of
        MainMenu ->
            column []
                [ text "Select Asset Source"
                , button [] { onPress = Just LoadDataAgain, label = text "Load albums" }
                ]
        SearchAssetInput searchString ->
            column []
                [ text "Input Search String"
                , text searchString
                ]
        SelectAlbumInput search ->
            viewWithSidebar
                (viewSidebarAlbums search model.knownAlbums)
                (column []
                    [ text "Select Album"
                    , text search.searchString
                    ]
                )
        LoadingAssets _ ->
            viewLoadingAssets model.imagesLoadState
        EditAsset _ asset search ->
            viewWithSidebar (viewSidebar asset search model.knownAlbums) (viewEditAsset model.immichApiPaths asset)

viewInputMode : UserMode -> Element msg
viewInputMode userMode =
    let
        inputMode =
            case userMode of
                MainMenu ->
                    NormalMode
                SearchAssetInput _ ->
                    InsertMode
                SelectAlbumInput _ ->
                    InsertMode
                LoadingAssets _ ->
                    NormalMode
                EditAsset editInputMode _ _ ->
                    editInputMode
    in
    case inputMode of
        NormalMode ->
            el [ width fill, Background.color <| Element.fromRgb { red = 0.8, green = 0.8, blue = 0.8, alpha = 1 } ] <| text "Normal"
        InsertMode ->
            el [ width fill, Background.color <| Element.fromRgb { red = 0, green = 1, blue = 0, alpha = 1 } ] <| text "Input"

viewWithSidebar : Element Msg -> Element Msg -> Element Msg
viewWithSidebar sidebarView viewToBeNextToSidebar =
    row [ width fill, height fill ]
        [ el [ width <| fillPortion 4, height fill ] <| viewToBeNextToSidebar
        , el [ width <| fillPortion 1, height fill, alignRight ] <| sidebarView
        ]

viewSidebar : AssetWithActions -> AlbumSearch -> Dict ImmichAssetId ImmichAlbum -> Element Msg
viewSidebar asset search albums =
    column [ alignTop ]
        [ el [ alignTop, height <| fillPortion 1 ] <| text "Asset Changes"
        , row [ alignTop, height <| fillPortion 2 ]
            [ case asset.isFavourite of
                ChangeToTrue ->
                    el [ Font.color <| usefulColours "green" ] <| text "Fav"
                ChangeToFalse ->
                    el [ Font.color <| usefulColours "red" ] <| text "!Fav"
                RemainTrue ->
                    el [ Font.color <| usefulColours "grey" ] <| text "Fav"
                RemainFalse ->
                    el [ Font.color <| usefulColours "grey" ] <| text "!Fav"
            , case asset.isArchived of
                ChangeToTrue ->
                    el [ Font.color <| usefulColours "red" ] <| text "Delete"
                ChangeToFalse ->
                    el [ Font.color <| usefulColours "green" ] <| text "Undelete"
                RemainTrue ->
                    el [ Font.color <| usefulColours "grey" ] <| text ""
                RemainFalse ->
                    el [ Font.color <| usefulColours "grey" ] <| text ""
            ]
        , viewSidebarAlbumsForCurrentAsset asset search albums
        ]


viewSidebarAlbums : AlbumSearch -> Dict ImmichAssetId ImmichAlbum -> Element Msg
viewSidebarAlbums search albums =
    column [ height fill ]
        (List.map
            (\album ->
                row []
                    [ el [ paddingXY 5 0 ] <| text (String.fromInt album.assetCount)
                    , el [] <| text album.albumName
                    ]
            )
         <|
            List.take 40 <|
                Dict.values <|
                    filterToOnlySearchedForAlbums search albums
        )

viewSidebarAlbumsForCurrentAsset : AssetWithActions -> AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> Element Msg
viewSidebarAlbumsForCurrentAsset asset search albums =
    column [ height fill ]
        (List.map
            (\album ->
                let
                    assetInAlbum =
                        Maybe.withDefault RemainFalse <| Dict.get album.id asset.albumMembership
                    attrs =
                        case assetInAlbum of
                            RemainTrue ->
                                [ Background.color <| usefulColours "green" ]
                            RemainFalse ->
                                [ Background.color <| usefulColours "grey" ]
                            ChangeToTrue ->
                                [ Background.color <| usefulColours "blue" ]
                            ChangeToFalse ->
                                [ Background.color <| usefulColours "red" ]
                in
                row []
                    [ el [ paddingXY 5 0 ] <| text (String.fromInt album.assetCount)
                    , el attrs <| text album.albumName
                    ]
            )
         <|
            List.take 40 <|
                Dict.values <|
                    filterToOnlySearchedForAlbums search albums
        )

filterToOnlySearchedForAlbums : AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> Dict ImmichAlbumId ImmichAlbum
filterToOnlySearchedForAlbums search albums =
    if search.searchString == "" then
        albums
    else
        Dict.filter (\id _ -> shouldFilterAlbum search.albumScores id) albums

shouldFilterAlbum : Dict ImmichAlbumId Int -> ImmichAlbumId -> Bool
shouldFilterAlbum albumScores albumId =
    case Dict.get albumId albumScores of
        Just score ->
            0 < score

        Nothing ->
            False


-- UPDATE --


loopImageIndexOverArray : ImageIndex -> Int -> Int -> ImageIndex
loopImageIndexOverArray index step length =
    modBy length (index + step)


isSupportedSearchLetter : String -> Bool
isSupportedSearchLetter testString =
    let
        regex =
            Regex.fromString "^[a-zA-Z0-9 ]$" |> Maybe.withDefault Regex.never
    in
    Regex.contains regex testString


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadDataAgain ->
            ( model, Immich.getAllAlbums model.apiUrl model.apiKey |> Cmd.map ImmichMsg )
        KeyPress key ->
            handleUserInput model key
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
                            model
                                |> handleFetchAlbums albums

                        -- |> handleProgressLoadingState FetchedAlbums
                        Immich.RandomImagesFetched (Ok assets) ->
                            model
                                |> handleFetchAssets assets
                                |> handleUpdateLoadingState FetchedAssetList

                        Immich.AllImagesFetched (Ok assets) ->
                            model
                                |> handleFetchAssets assets
                                |> handleUpdateLoadingState FetchedAssetList

                        Immich.AssetMembershipFetched (Ok assetWithMembership) ->
                            model
                                |> handleFetchAssetMembership assetWithMembership

                        Immich.SingleAlbumFetched (Err error) ->
                            model
                        Immich.AlbumsFetched (Err error) ->
                            { model | albumsLoadState = ImmichLoadError error }
                        Immich.RandomImagesFetched (Err error) ->
                            { model | imagesLoadState = ImmichLoadError error }
                        Immich.AllImagesFetched (Err error) ->
                            { model | imagesLoadState = ImmichLoadError error }
                        Immich.AssetMembershipFetched (Err error) ->
                            model
            in
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
            Tuple.first <| switchToEditIfAssetFound { model | knownAssets = Dict.insert assetWithMembership.assetId newAsset model.knownAssets } 0


handleFetchAssets : List ImmichAsset -> Model -> Model
handleFetchAssets assets model =
    { model | knownAssets = Helpers.listOverrideDict assets (\a -> ( a.id, a )) model.knownAssets, currentAssets = List.map .id assets, imagesLoadState = ImmichLoadSuccess }

handleFetchAlbums : List ImmichAlbum -> Model -> Model
handleFetchAlbums albums model =
    { model | knownAlbums = Helpers.listOverrideDict albums (\a -> ( a.id, a )) model.knownAlbums, albumsLoadState = ImmichLoadSuccess }

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
            in
            updatedModel
        _ ->
            model

checkIfLoadingComplete : Model -> ( Model, Cmd Msg )
checkIfLoadingComplete model =
    case model.userMode of
        LoadingAssets loadState ->
            if isLoadStateCompleted loadState then
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
        Uncategorised ->
            { model | currentAssetsSource = assetSource, userMode = LoadingAssets { fetchedAssetList = Just False, fetchedAssetMembership = Nothing } }
        Album _ ->
            { model | currentAssetsSource = assetSource, userMode = LoadingAssets { fetchedAssetList = Just False, fetchedAssetMembership = Nothing } }
        Search _ ->
            { model | currentAssetsSource = assetSource, userMode = LoadingAssets { fetchedAssetList = Just False, fetchedAssetMembership = Nothing } }


handleUserInput : Model -> String -> ( Model, Cmd Msg )
handleUserInput model key =
    case model.userMode of
        MainMenu ->
            case key of
                "u" ->
                    ( applyGeneralAction model (ChangeUserModeToLoading Uncategorised), Immich.fetchAllImages model.immichApiPaths |> Cmd.map ImmichMsg )
                "a" ->
                    ( applyGeneralAction model ChangeUserModeToSelectAlbum, Cmd.none )
                "s" ->
                    ( applyGeneralAction model ChangeUserModeToSearchAsset, Cmd.none )
                _ ->
                    ( model, Cmd.none )
        SearchAssetInput searchString ->
            let
                userAction =
                    if isSupportedSearchLetter key then
                        TextInputUpdate (TextInputAddition key)
                    else
                        case key of
                            "Backspace" ->
                                TextInputUpdate TextInputBackspace
                            "Escape" ->
                                UserActionGeneralSearch <| ChangeUserModeToMainMenu
                            "Enter" ->
                                UserActionGeneralSearch <| ChangeUserModeToLoading (Search searchString)
                            _ ->
                                UserActionGeneralSearch UnknownAction
            in
            case userAction of
                TextInputUpdate (TextInputAddition newKey) ->
                    let
                        newSearchString =
                            searchString ++ newKey
                    in
                    ( { model | userMode = SearchAssetInput newSearchString }, Cmd.none )
                TextInputUpdate TextInputBackspace ->
                    let
                        newSearchString =
                            String.slice 0 (String.length searchString - 1) searchString
                    in
                    ( { model | userMode = SearchAssetInput newSearchString }, Cmd.none )
                UserActionGeneralSearch action ->
                    ( applyGeneralAction model action, Cmd.none )
        SelectAlbumInput searchResults ->
            let
                userAction =
                    if isSupportedSearchLetter key then
                        TextSelectInputUpdate (TextInputAddition key)
                    else
                        case key of
                            "Backspace" ->
                                TextSelectInputUpdate TextInputBackspace
                            "Escape" ->
                                UserActionGeneralAlbumSelect ChangeUserModeToMainMenu
                            "Enter" ->
                                SelectAlbumIfMatching
                            _ ->
                                UserActionGeneralAlbumSelect UnknownAction
            in
            case userAction of
                TextSelectInputUpdate (TextInputAddition newKey) ->
                    let
                        newSearchString =
                            searchResults.searchString ++ newKey
                    in
                    ( { model | userMode = SelectAlbumInput <| getAlbumSearch newSearchString model.knownAlbums }, Cmd.none )
                TextSelectInputUpdate TextInputBackspace ->
                    let
                        newSearchString =
                            String.slice 0 (String.length searchResults.searchString - 1) searchResults.searchString
                    in
                    ( { model | userMode = SelectAlbumInput <| getAlbumSearch newSearchString model.knownAlbums }, Cmd.none )
                SelectAlbumIfMatching ->
                    let
                        maybeMatch =
                            getTopMatchToSearch searchResults model.knownAlbums
                    in
                    case maybeMatch of
                        Just album ->
                            ( createLoadStateForCurrentAssetSource (Album album) model, Immich.getAlbum model.immichApiPaths album.id |> Cmd.map ImmichMsg )
                        Nothing ->
                            ( model, Cmd.none )
                UserActionGeneralAlbumSelect action ->
                    ( applyGeneralAction model action, Cmd.none )
        LoadingAssets _ ->
            ( model, Cmd.none )
        EditAsset inputMode asset search ->
            let
                userAction =
                    if inputMode == InsertMode then
                        if isSupportedSearchLetter key then
                            TextEditModeInputUpdate (TextInputAddition key)
                        else
                            case key of
                                "Escape" ->
                                    ChangeInputMode NormalMode
                                "Backspace" ->
                                    TextEditModeInputUpdate TextInputBackspace
                                "Enter" ->
                                    ApplyAlbumIfMatching
                                _ ->
                                    UserActionGeneralEdit UnknownAction
                    else
                        case key of
                            "ArrowLeft" ->
                                ChangeImageIndex -1
                            "ArrowRight" ->
                                ChangeImageIndex 1
                            "Escape" ->
                                UserActionGeneralEdit <| ChangeUserModeToMainMenu
                            -- "Backspace" ->
                            --     RemoveFromAssetChangeList
                            "i" ->
                                ChangeInputMode InsertMode
                            "d" ->
                                AssetChange ToggleDelete
                            "f" ->
                                AssetChange ToggleFavourite
                            _ ->
                                UserActionGeneralEdit UnknownAction
            in
            case userAction of
                AssetChange ToggleFavourite ->
                    let
                        newAsset =
                            { asset | isFavourite = flipPropertyChange asset.isFavourite }
                    in
                    ( { model | userMode = EditAsset inputMode newAsset search }, Cmd.none )
                AssetChange ToggleDelete ->
                    let
                        newAsset =
                            { asset | isArchived = flipPropertyChange asset.isArchived }
                    in
                    ( { model | userMode = EditAsset inputMode newAsset search }, Cmd.none )
                AssetChange (ToggleAlbum album) ->
                    ( { model | userMode = EditAsset inputMode (toggleAssetAlbum asset album) search }, Cmd.none )
                ApplyAlbumIfMatching ->
                    let
                        maybeMatch =
                            getTopMatchToSearch search model.knownAlbums
                    in
                    case maybeMatch of
                        Just album ->
                            ( { model | userMode = EditAsset inputMode (toggleAssetAlbum asset album) (getAlbumSearch "" model.knownAlbums) }, Cmd.none )
                        Nothing ->
                            ( model, Cmd.none )
                ChangeInputMode newInputMode ->
                    ( { model | userMode = EditAsset newInputMode asset <| getAlbumSearch "" model.knownAlbums }, Cmd.none )
                ChangeImageIndex indexChange ->
                    let
                        newIndex =
                            loopImageIndexOverArray model.imageIndex indexChange (List.length model.currentAssets)
                    in
                    switchToEditIfAssetFound model newIndex
                TextEditModeInputUpdate (TextInputAddition newKey) ->
                    let
                        newSearchString =
                            search.searchString ++ newKey
                    in
                    ( { model | userMode = EditAsset inputMode asset <| getAlbumSearch newSearchString model.knownAlbums }, Cmd.none )
                TextEditModeInputUpdate TextInputBackspace ->
                    let
                        newSearchString =
                            String.slice 0 (String.length search.searchString - 1) search.searchString
                    in
                    ( { model | userMode = EditAsset inputMode asset <| getAlbumSearch newSearchString model.knownAlbums }, Cmd.none )

                UserActionGeneralEdit generalAction ->
                    ( applyGeneralAction model generalAction, Cmd.none )

getTopMatchToSearch : AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> Maybe ImmichAlbum
getTopMatchToSearch search albums =
    let
        matchesDict =
            filterToOnlySearchedForAlbums search albums
    in
    if Dict.isEmpty matchesDict then
        Nothing
    else if Dict.size matchesDict == 1 then
        case Dict.values matchesDict of
            [ album ] ->
                Just album
            _ ->
                Nothing

    else
        Nothing

toggleAssetAlbum : AssetWithActions -> ImmichAlbum -> AssetWithActions
toggleAssetAlbum asset album =
    { asset | albumMembership = Dict.insert album.id (flipPropertyChange <| Maybe.withDefault RemainFalse <| Dict.get album.id asset.albumMembership) asset.albumMembership }


applyGeneralAction : Model -> UserActionGeneral -> Model
applyGeneralAction model action =
    case action of
        ChangeUserModeToMainMenu ->
            { model | userMode = MainMenu }
        ChangeUserModeToSearchAsset ->
            { model | userMode = SearchAssetInput "" }
        ChangeUserModeToSelectAlbum ->
            { model | userMode = SelectAlbumInput <| getAlbumSearch "" model.knownAlbums }
        ChangeUserModeToEditAsset ->
            Tuple.first <| switchToEditIfAssetFound model 0
        ChangeUserModeToLoading assetSource ->
            createLoadStateForCurrentAssetSource assetSource model

        ReloadData ->
            { model | imagesLoadState = ImmichLoading, albumsLoadState = ImmichLoading }

        UnknownAction ->
            model

switchToEditIfAssetFound : Model -> ImageIndex -> ( Model, Cmd Msg )
switchToEditIfAssetFound model index =
    let
        maybeAssetId =
            model.currentAssets |> List.drop index |> List.head
        maybeAsset =
            case maybeAssetId of
                Nothing ->
                    Nothing
                Just id ->
                    Dict.get id model.knownAssets
    in
    case maybeAsset of
        --TODO: Should not be necessary?
        Nothing ->
            ( createLoadStateForCurrentAssetSource model.currentAssetsSource model, Cmd.none )

        Just asset ->
            let
                cmdToSend =
                    if List.isEmpty asset.albumMembership then
                        Immich.fetchMembershipForAsset model.immichApiPaths asset.id |> Cmd.map ImmichMsg
                    else
                        Cmd.none
            in
            ( { model | imageIndex = index, userMode = EditAsset NormalMode (getAssetWithActions asset) (getAlbumSearch "" model.knownAlbums) }, cmdToSend )


getAssetWithActions : ImmichAsset -> AssetWithActions
getAssetWithActions asset =
    { asset = asset
    , isFavourite =
        if asset.isFavourite then
            RemainTrue

        else
            RemainFalse
    , isArchived =
        if asset.isArchived then
            RemainTrue

        else
            RemainFalse
    , albumMembership = Dict.fromList <| List.map (\a -> ( a, RemainTrue )) asset.albumMembership
    }

getAlbumSearch : String -> Dict ImmichAssetId ImmichAlbum -> AlbumSearch
getAlbumSearch searchString albums =
    { searchString = searchString
    , albumScores =
        Dict.map (\id album -> shittyFuzzyAlgorithmTest searchString album.albumName) albums
    }



-- { searchString = searchString
-- , albumsWithScore =
--     albumsWithScores
--         |> List.filter (\a -> a.score > 0 || searchString == "")
--         |> List.sortBy (\a -> ( a.score, a.album.assetCount ))
--         |> List.reverse
-- }

shittyFuzzyAlgorithmTest : String -> String -> Int
shittyFuzzyAlgorithmTest searchString textToBeSearched =
    let
        patternFzfFuzzy =
            List.foldr (++) ".*" <| List.map (\a -> String.fromChar a ++ ".*") <| String.toList searchString
        regexes =
            [ { score = 10, regex = regexFromString ("$" ++ searchString ++ ".*") }
            , { score = 7, regex = regexFromString ("[^a-z]" ++ searchString ++ ".*") }
            , { score = 5, regex = regexFromString patternFzfFuzzy }
            ]
    in
    if searchString == "" then
        0
    else
        List.filter (\a -> Regex.contains a.regex textToBeSearched) regexes |> List.map .score |> List.foldr (+) 0



-- SUBSCRIPTIONS --

subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown (Decode.map KeyPress (Decode.field "key" Decode.string))


main : Program Flags Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
