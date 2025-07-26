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
import Helpers exposing (regexFromString)
import Html exposing (Html)
import Html.Attributes
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichAsset, ImmichAssetId, ImmichLoadState(..), getAllAlbums)
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
    , assetSelectMode : AssetSource
    , userMode : UserMode
    , test : Int
    , imageIndex : ImageIndex
    -- Immich fields
    , images : List ImmichAsset
    , imagesLoadState : ImmichLoadState
    , albums : List ImmichAlbum
    , albumsLoadState : ImmichLoadState
    , apiUrl : String
    , apiKey : String
    }

type AssetSource
    = NoAssets
    | Uncategorised
    | Search String
    | Album ImmichAlbum


type UserMode
    = MainMenu
    | SearchAssetInput SearchString
    | SelectAlbumInput AlbumSearch
    | LoadingAssets
    | EditAsset InputMode AssetWithActions AlbumSearch

type alias SearchString =
    String

type alias ImageIndex =
    Int

type alias AssetWithActions =
    { asset : ImmichAsset
    , isFavourite : PropertyChange
    , isArchived : PropertyChange
    , albumMemership : Dict ImmichAssetId PropertyChange
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
    let
        testImages =
            [ ImmichAsset "0001" "images/imafight.jpg" "Imafight" "jpg" False False
            , ImmichAsset "0002" "images/dcemployees.jpg" "dcemployees" "jpg" False False
            , ImmichAsset "0003" "images/jordan.jpg" "Jordan" "jpg" False False
            , ImmichAsset "0004" "images/router-password.mp4" "router password" "mp4" False False
            ]

        testAlbums =
            [ ImmichAlbum "a" "J" 200 [] (Date.fromOrdinalDate 2025 1)
            , ImmichAlbum "b" "ToBeSorted" 3000 [] (Date.fromOrdinalDate 2025 1)
            , ImmichAlbum "c" "The World" 50 [] (Date.fromOrdinalDate 2025 1)
            , ImmichAlbum "d" "The Other One" 75 [] (Date.fromOrdinalDate 2025 1)
            , ImmichAlbum "e" "Comics" 50 [] (Date.fromOrdinalDate 2025 1)
            ]
    in
    ( { key = ""
      , imagePrepend = flags.imagePrepend
      , userMode = MainMenu
      , assetSelectMode = NoAssets
      , test = flags.test
      , imageIndex = 0
      -- Immich fields
      , images = testImages
      , imagesLoadState = ImmichLoading
      , albums = testAlbums
      , albumsLoadState = ImmichLoading
      , apiUrl = flags.immichApiUrl
      , apiKey = flags.immichApiKey
      }
      -- , Cmd.none
    , getAllAlbums flags.immichApiUrl flags.immichApiKey |> Cmd.map ImmichMsg
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


viewAsset : String -> ImmichAsset -> Element msg
viewAsset assetPrepend asset =
    if asset.path == "" then
        el [] (text "No Asset")

    else
        let
            fullAssetPath =
                assetPrepend ++ asset.path
        in
        if assetIsImage asset then
            viewImage asset fullAssetPath

        else if assetIsVideo asset then
            viewVideo
                [ Background.color (Element.rgb 0 0 0) ]
                { poster = ""
                , source = fullAssetPath
                }

        else
            text (String.join " " [ "Error with", asset.title, "Unknown mimetype:", asset.mimeType ])

assetIsImage : ImmichAsset -> Bool
assetIsImage asset =
    List.member asset.mimeType [ "image/jpg", "image/png", "image/gif" ]

viewImage : ImmichAsset -> String -> Element msg
viewImage asset path =
    column [ width fill, height fill ]
        [ Element.image [ centerY, centerX ] { src = path, description = "" }
        , el [ centerX ] (text asset.title)
        ]

assetIsVideo : ImmichAsset -> Bool
assetIsVideo asset =
    List.member asset.mimeType [ "video/mp4" ]

viewVideo : List (Element.Attribute msg) -> { poster : String, source : String } -> Element msg
viewVideo attrs { poster, source } =
    el attrs <|
        Element.html <|
            Html.video
                [ Html.Attributes.attribute "controls" "controls"
                , Html.Attributes.preload "none"
                -- , Html.Attributes.poster poster
                , Html.Attributes.autoplay True
                , Html.Attributes.loop True
                ]
                [ Html.source
                    [ Html.Attributes.id "mp4"
                    , Html.Attributes.src source
                    , Html.Attributes.type_ "video/mp4"
                    ]
                    []
                ]

viewEditAsset : String -> AssetWithActions -> Element Msg
viewEditAsset assetPathPrepend currentAsset =
    column [ width fill, height fill ]
        [ el [ alignTop ] (text "Top Bar")
        , viewAsset assetPathPrepend currentAsset.asset
        , column [ alignBottom ] []
        -- [ text (String.fromInt index ++ "  ")
        -- ]
        ]


view : Model -> Html Msg
view model =
    Element.layout [ width fill, height fill ] <|
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
                (viewSidebarAlbums search model.albums)
                (column []
                    [ text "Select Album"
                    , text search.searchString
                    ]
                )
        LoadingAssets ->
            text "Loading Assets"
        EditAsset _ asset search ->
            viewWithSidebar (viewSidebar asset search model.albums) (viewEditAsset model.imagePrepend asset)

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
                LoadingAssets ->
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

viewSidebar : AssetWithActions -> AlbumSearch -> List ImmichAlbum -> Element Msg
viewSidebar asset search albums =
    column [ height fill ]
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


viewSidebarAlbums : AlbumSearch -> List ImmichAlbum -> Element Msg
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
            filterToOnlySearchedForAlbums search albums
        )

viewSidebarAlbumsForCurrentAsset : AssetWithActions -> AlbumSearch -> List ImmichAlbum -> Element Msg
viewSidebarAlbumsForCurrentAsset asset search albums =
    column [ height fill ]
        (List.map
            (\album ->
                let
                    assetInAlbum =
                        Maybe.withDefault RemainFalse <| Dict.get album.id asset.albumMemership
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
            filterToOnlySearchedForAlbums search albums
        )

filterToOnlySearchedForAlbums : AlbumSearch -> List ImmichAlbum -> List ImmichAlbum
filterToOnlySearchedForAlbums search albums =
    if search.searchString == "" then
        albums
    else
        List.filter (\album -> 0 < (Maybe.withDefault 0 <| Dict.get album.id search.albumScores)) albums


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
        ImmichMsg Immich.StartLoading ->
            ( { model | albumsLoadState = ImmichLoading, imagesLoadState = ImmichLoading }, getAllAlbums model.apiUrl model.apiKey |> Cmd.map ImmichMsg )

        ImmichMsg (Immich.AlbumsFetched (Ok albums)) ->
            ( { model | albums = albums, albumsLoadState = ImmichLoadSuccess }, Cmd.none )

        ImmichMsg (Immich.AlbumsFetched (Err error)) ->
            ( { model | albums = [], albumsLoadState = ImmichLoadError error }, Cmd.none )

        ImmichMsg (Immich.RandomImagesFetched (Ok assets)) ->
            let
                firstAsset =
                    List.head assets
                newUserMode =
                    if model.userMode == LoadingAssets then
                        case firstAsset of
                            Just asset ->
                                EditAsset NormalMode (getAssetWithActions asset) <| getAlbumSearch "" model.albums
                            Nothing ->
                                model.userMode
                        --TODO: Show error
                    else
                        model.userMode
            in
            ( { model | userMode = newUserMode, images = assets, imagesLoadState = ImmichLoadSuccess }, Cmd.none )

        ImmichMsg (Immich.RandomImagesFetched (Err error)) ->
            ( { model | images = [], imagesLoadState = ImmichLoadError error }, Cmd.none )

handleUserInput : Model -> String -> ( Model, Cmd Msg )
handleUserInput model key =
    case model.userMode of
        MainMenu ->
            let
                generalAction =
                    case key of
                        "u" ->
                            ChangeUserModeToEditAsset
                        "a" ->
                            ChangeUserModeToSelectAlbum
                        "s" ->
                            ChangeUserModeToSearchAsset
                        _ ->
                            UnknownAction
            in
            if generalAction == ChangeUserModeToEditAsset then
                ( { model | userMode = LoadingAssets }, Immich.fetchRandomImages model.apiUrl model.apiKey |> Cmd.map ImmichMsg )
            else
                handleGeneralActions model generalAction
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
                                UserActionGeneralSearch <| ChangeUserModeToEditAsset
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
                    handleGeneralActions model action
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
                    ( { model | userMode = SelectAlbumInput <| getAlbumSearch newSearchString model.albums }, Cmd.none )
                TextSelectInputUpdate TextInputBackspace ->
                    let
                        newSearchString =
                            String.slice 0 (String.length searchResults.searchString - 1) searchResults.searchString
                    in
                    ( { model | userMode = SelectAlbumInput <| getAlbumSearch newSearchString model.albums }, Cmd.none )
                SelectAlbumIfMatching ->
                    let
                        matches =
                            filterToOnlySearchedForAlbums searchResults model.albums
                    in
                    case matches of
                        [] ->
                            ( model, Cmd.none )
                        [ album ] ->
                            -- TODO: Call fetch on album
                            switchToEditIfAssetFound model 0
                        _ ->
                            ( model, Cmd.none )
                UserActionGeneralAlbumSelect action ->
                    handleGeneralActions model action
        LoadingAssets ->
            ( { model | userMode = LoadingAssets }, Cmd.none )
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
                -- RemoveFromAssetChangeList ->
                --     ( { model | userMode = EditAsset inputMode assetSource index (List.drop 1 pendingChanges) searchResults }, Cmd.none )
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
                        matches =
                            filterToOnlySearchedForAlbums search model.albums
                    in
                    case matches of
                        [] ->
                            ( model, Cmd.none )
                        [ album ] ->
                            ( { model | userMode = EditAsset inputMode (toggleAssetAlbum asset album) (getAlbumSearch "" model.albums) }, Cmd.none )
                        _ ->
                            ( model, Cmd.none )
                ChangeInputMode newInputMode ->
                    ( { model | userMode = EditAsset newInputMode asset <| getAlbumSearch "" model.albums }, Cmd.none )
                ChangeImageIndex indexChange ->
                    let
                        newIndex =
                            loopImageIndexOverArray model.imageIndex indexChange (List.length model.images)
                    in
                    switchToEditIfAssetFound model newIndex
                TextEditModeInputUpdate (TextInputAddition newKey) ->
                    let
                        newSearchString =
                            search.searchString ++ newKey
                    in
                    ( { model | userMode = EditAsset inputMode asset <| getAlbumSearch newSearchString model.albums }, Cmd.none )
                TextEditModeInputUpdate TextInputBackspace ->
                    let
                        newSearchString =
                            String.slice 0 (String.length search.searchString - 1) search.searchString
                    in
                    ( { model | userMode = EditAsset inputMode asset <| getAlbumSearch newSearchString model.albums }, Cmd.none )

                UserActionGeneralEdit generalAction ->
                    handleGeneralActions model generalAction


toggleAssetAlbum : AssetWithActions -> ImmichAlbum -> AssetWithActions
toggleAssetAlbum asset album =
    { asset | albumMemership = Dict.insert album.id (flipPropertyChange <| Maybe.withDefault RemainFalse <| Dict.get album.id asset.albumMemership) asset.albumMemership }


handleGeneralActions : Model -> UserActionGeneral -> ( Model, Cmd Msg )
handleGeneralActions model action =
    case action of
        ChangeUserModeToMainMenu ->
            ( { model | userMode = MainMenu }, Cmd.none )
        ChangeUserModeToSearchAsset ->
            ( { model | userMode = SearchAssetInput "" }, Cmd.none )
        ChangeUserModeToSelectAlbum ->
            ( { model | userMode = SelectAlbumInput <| getAlbumSearch "" model.albums }, Cmd.none )
        ChangeUserModeToEditAsset ->
            switchToEditIfAssetFound model 0

        ReloadData ->
            ( { model | imagesLoadState = ImmichLoading, albumsLoadState = ImmichLoading }, Cmd.none )

        UnknownAction ->
            ( model, Cmd.none )

switchToEditIfAssetFound : Model -> ImageIndex -> ( Model, Cmd Msg )
switchToEditIfAssetFound model index =
    let
        maybeAsset =
            model.images |> List.drop index |> List.head
    in
    case maybeAsset of
        Nothing ->
            ( { model | userMode = LoadingAssets }, Cmd.none )

        Just asset ->
            ( { model | imageIndex = index, userMode = EditAsset NormalMode (getAssetWithActions asset) (getAlbumSearch "" model.albums) }, Cmd.none )


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
    , albumMemership = Dict.empty
    }

getAlbumSearch : String -> List ImmichAlbum -> AlbumSearch
getAlbumSearch searchString albums =
    { searchString = searchString
    , albumScores =
        Dict.fromList <| List.map (\album -> ( album.id, shittyFuzzyAlgorithmTest searchString album.albumName )) albums
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
