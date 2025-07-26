module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Browser.Events exposing (onKeyDown)
import Date
import Debug exposing (toString)
import Dict exposing (Dict)
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, centerY, column, el, fill, fillPortion, height, padding, paddingXY, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Helpers exposing (send)
import Html exposing (Html, button, div)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Immich exposing (ImmichAlbum, ImmichAsset, ImmichLoadState(..), getAllAlbums)
import Json.Decode as Decode
import Regex exposing (Regex)


type Msg
    = KeyPress String
    | ImmichMsg Immich.Msg

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
    -- Immich fields
    , images : Array ImmichAsset
    , imagesLoadState : ImmichLoadState
    , albums : Array ImmichAlbum
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
    | SelectAlbumInput AlbumSearchResults
    | EditAsset InputMode AssetSource ImageIndex (List AssetChange) AlbumSearchResults

type alias SearchString =
    String

type alias ImageIndex =
    Int

type alias AlbumSearchResults =
    { searchString : SearchString
    , albumsWithScore : List AlbumWithScore
    }

type alias AlbumWithScore =
    { album : ImmichAlbum
    , score : Int
    }

type InputMode
    = NormalMode
    | InsertMode

type AssetChange
    = AddToAlbum ImmichAlbum
    | RemoveFromAlbum ImmichAlbum
    | Delete
    | SetFavourite Bool

type TextInputUpdate
    = TextInputAddition String
    | TextInputBackspace

type UserActionGeneral
    = ChangeUserMode UserMode
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
    | AddToAssetChangeList AssetChange
    | RemoveFromAssetChangeList
    | ChangeImageIndex Int
    | TextEditModeInputUpdate TextInputUpdate
    | ApplyAlbumIfMatching
    | UserActionGeneralEdit UserActionGeneral

type PropertyChange
    = RemainTrue
    | RemainFalse
    | ChangeToTrue
    | ChangeToFalse

init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        testImages =
            Array.fromList
                [ ImmichAsset "0001" "images/imafight.jpg" "Imafight" "jpg" False False
                , ImmichAsset "0002" "images/dcemployees.jpg" "dcemployees" "jpg" False False
                , ImmichAsset "0003" "images/jordan.jpg" "Jordan" "jpg" False False
                , ImmichAsset "0004" "images/router-password.mp4" "router password" "mp4" False False
                ]

        testAlbums =
            Array.fromList
                [ ImmichAlbum "a" "J" 200 Array.empty "000001" (Date.fromOrdinalDate 2025 1)
                , ImmichAlbum "b" "ToBeSorted" 3000 Array.empty "000002" (Date.fromOrdinalDate 2025 1)
                , ImmichAlbum "c" "The World" 50 Array.empty "000003" (Date.fromOrdinalDate 2025 1)
                , ImmichAlbum "c" "The Other One" 75 Array.empty "000034" (Date.fromOrdinalDate 2025 1)
                , ImmichAlbum "d" "Comics" 50 Array.empty "000004" (Date.fromOrdinalDate 2025 1)
                ]
    in
    ( { key = ""
      , imagePrepend = flags.imagePrepend
      , userMode = MainMenu
      , assetSelectMode = NoAssets
      , test = flags.test
      -- Immich fields
      , images = testImages
      , imagesLoadState = ImmichLoading
      , albums = testAlbums
      , albumsLoadState = ImmichLoading
      , apiUrl = flags.immichApiUrl
      , apiKey = flags.immichApiKey
      }
    , Cmd.none
      -- getAllAlbums flags.immichApiUrl flags.immichApiKey |> Cmd.map ImmichMsg
    )



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
    List.member asset.mimeType [ "jpg", "png", "gif" ]

viewImage : ImmichAsset -> String -> Element msg
viewImage asset path =
    column [ width fill, height fill ]
        [ Element.image [ centerY, centerX ] { src = path, description = "" }
        , el [ centerX ] (text asset.title)
        ]

assetIsVideo : ImmichAsset -> Bool
assetIsVideo asset =
    List.member asset.mimeType [ "mp4" ]

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

viewEditAsset : String -> ImmichAsset -> Element Msg
viewEditAsset assetPathPrepend currentAsset =
    column [ width fill, height fill ]
        [ el [ alignTop ] (text "Top Bar")
        , viewAsset assetPathPrepend currentAsset
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
            text "Select Asset Source"
        SearchAssetInput searchString ->
            column []
                [ text "Input Search String"
                , text searchString
                ]
        SelectAlbumInput searchResults ->
            viewWithSidebar
                (viewSidebarAlbums searchResults)
                (column []
                    [ text "Select Album"
                    , text searchResults.searchString
                    ]
                )
        EditAsset _ _ index pendingChanges searchResults ->
            let
                currentAsset =
                    Array.get index model.images
            in
            case currentAsset of
                Just asset ->
                    viewWithSidebar (viewSidebar searchResults pendingChanges asset) (viewEditAsset model.imagePrepend asset)
                Nothing ->
                    text ("Error. Cannot find image of index " ++ String.fromInt index)

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
                EditAsset editInputMode _ _ _ _ ->
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

viewSidebar : AlbumSearchResults -> List AssetChange -> ImmichAsset -> Element Msg
viewSidebar searchResults pendingChanges currentAsset =
    let
        setFavourite =
            case currentAsset.isFavourite of
                True ->
                    if List.member (SetFavourite False) pendingChanges then
                        ChangeToFalse
                    else
                        RemainTrue
                False ->
                    if List.member (SetFavourite True) pendingChanges then
                        ChangeToTrue
                    else
                        RemainFalse
        shouldDelete =
            case currentAsset.isArchived of
                True ->
                    if List.member Delete pendingChanges then
                        ChangeToTrue
                    else
                        RemainTrue
                False ->
                    if List.member Delete pendingChanges then
                        ChangeToTrue
                    else
                        RemainFalse
    in
    column [ height fill ]
        [ el [ alignTop, height <| fillPortion 1 ] <| text "Asset Changes"
        , row [ alignTop, height <| fillPortion 2 ]
            [ case setFavourite of
                ChangeToTrue ->
                    el [ Font.color <| Element.fromRgb { red = 0, green = 1, blue = 0, alpha = 1 } ] <| text "Fav"
                ChangeToFalse ->
                    el [ Font.color <| Element.fromRgb { red = 1, green = 0, blue = 0, alpha = 1 } ] <| text "!Fav"
                RemainTrue ->
                    el [ Font.color <| Element.fromRgb { red = 0.8, green = 0.8, blue = 0.8, alpha = 0.6 } ] <| text "Fav"
                RemainFalse ->
                    el [ Font.color <| Element.fromRgb { red = 0.8, green = 0.8, blue = 0.8, alpha = 0.6 } ] <| text "!Fav"
            , case shouldDelete of
                ChangeToTrue ->
                    el [ Font.color <| Element.fromRgb { red = 1, green = 0, blue = 0, alpha = 1 } ] <| text "Delete"
                ChangeToFalse ->
                    el [ Font.color <| Element.fromRgb { red = 0, green = 1, blue = 0, alpha = 1 } ] <| text "Undelete"
                RemainTrue ->
                    el [ Font.color <| Element.fromRgb { red = 0.8, green = 0.8, blue = 0.8, alpha = 0.6 } ] <| text ""
                RemainFalse ->
                    el [ Font.color <| Element.fromRgb { red = 0.8, green = 0.8, blue = 0.8, alpha = 0.6 } ] <| text ""
            ]
        , viewSidebarAlbumsForCurrentAsset searchResults pendingChanges currentAsset
        ]


viewSidebarAlbums : AlbumSearchResults -> Element Msg
viewSidebarAlbums searchResults =
    column [ height fill ]
        (List.map
            (\albumWithScore ->
                row []
                    [ el [ paddingXY 5 0 ] <| text (String.fromInt albumWithScore.album.assetCount)
                    , el [] <| text albumWithScore.album.albumName
                    ]
            )
            searchResults.albumsWithScore
        )

viewSidebarAlbumsForCurrentAsset : AlbumSearchResults -> List AssetChange -> ImmichAsset -> Element Msg
viewSidebarAlbumsForCurrentAsset searchResults pendingChanges currentAsset =
    column [ height fill ]
        (List.map
            (\albumWithScore ->
                let
                    attrs =
                        if List.member (AddToAlbum albumWithScore.album) pendingChanges then
                            [ Background.color <| Element.fromRgb { red = 0, green = 1, blue = 0, alpha = 1 } ]
                        else if List.member (RemoveFromAlbum albumWithScore.album) pendingChanges then
                            [ Background.color <| Element.fromRgb { red = 1, green = 0, blue = 0, alpha = 1 } ]
                        else
                            []
                in
                row []
                    [ el [ paddingXY 5 0 ] <| text (String.fromInt albumWithScore.album.assetCount)
                    , el attrs <| text albumWithScore.album.albumName
                    ]
            )
            searchResults.albumsWithScore
        )


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
        ImmichMsg immichMsg ->
            Immich.update immichMsg model |> Tuple.mapBoth (\a -> a) (Cmd.map ImmichMsg)
        KeyPress key ->
            handleUserInput model key

handleUserInput : Model -> String -> ( Model, Cmd Msg )
handleUserInput model key =
    case model.userMode of
        MainMenu ->
            let
                generalAction =
                    case key of
                        "u" ->
                            ChangeUserMode (EditAsset NormalMode Uncategorised 0 [] <| getMatchingAlbumsOrdered "" model.albums)
                        "a" ->
                            ChangeUserMode (SelectAlbumInput <| getMatchingAlbumsOrdered "" model.albums)
                        "s" ->
                            ChangeUserMode (SearchAssetInput "")
                        _ ->
                            UnknownAction
            in
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
                                UserActionGeneralSearch <| ChangeUserMode MainMenu
                            "Enter" ->
                                UserActionGeneralSearch <| ChangeUserMode <| EditAsset NormalMode (Search searchString) 0 [] <| getMatchingAlbumsOrdered "" model.albums
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
                                UserActionGeneralAlbumSelect (ChangeUserMode MainMenu)
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
                    ( { model | userMode = SelectAlbumInput <| getMatchingAlbumsOrdered newSearchString model.albums }, Cmd.none )
                TextSelectInputUpdate TextInputBackspace ->
                    let
                        newSearchString =
                            String.slice 0 (String.length searchResults.searchString - 1) searchResults.searchString
                    in
                    ( { model | userMode = SelectAlbumInput <| getMatchingAlbumsOrdered newSearchString model.albums }, Cmd.none )
                SelectAlbumIfMatching ->
                    case searchResults.albumsWithScore of
                        [] ->
                            ( model, Cmd.none )
                        [ albumWithScore ] ->
                            ( { model | userMode = EditAsset NormalMode (Album albumWithScore.album) 0 [] { searchString = "", albumsWithScore = [] } }, Cmd.none )
                        _ ->
                            ( model, Cmd.none )
                UserActionGeneralAlbumSelect action ->
                    handleGeneralActions model action
        EditAsset inputMode assetSource index pendingChanges searchResults ->
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
                                UserActionGeneralEdit <| ChangeUserMode MainMenu
                            "Backspace" ->
                                RemoveFromAssetChangeList
                            "i" ->
                                ChangeInputMode InsertMode
                            "d" ->
                                AddToAssetChangeList Delete
                            "f" ->
                                -- TODO: Base on the assets favourite state
                                AddToAssetChangeList <| SetFavourite True
                            _ ->
                                UserActionGeneralEdit UnknownAction
            in
            case userAction of
                AddToAssetChangeList change ->
                    let
                        newPendingChanges =
                            if List.member change pendingChanges then
                                pendingChanges
                            else
                                change :: pendingChanges
                    in
                    ( { model | userMode = EditAsset inputMode assetSource index newPendingChanges searchResults }, Cmd.none )
                RemoveFromAssetChangeList ->
                    ( { model | userMode = EditAsset inputMode assetSource index (List.drop 1 pendingChanges) searchResults }, Cmd.none )
                ApplyAlbumIfMatching ->
                    case searchResults.albumsWithScore of
                        [] ->
                            ( model, Cmd.none )
                        [ albumWithScore ] ->
                            let
                                change =
                                    AddToAlbum albumWithScore.album
                                newPendingChanges =
                                    if List.member change pendingChanges then
                                        pendingChanges
                                    else
                                        change :: pendingChanges
                            in
                            ( { model | userMode = EditAsset inputMode assetSource index newPendingChanges (getMatchingAlbumsOrdered "" model.albums) }, Cmd.none )
                        _ ->
                            ( model, Cmd.none )
                ChangeInputMode newInputMode ->
                    ( { model | userMode = EditAsset newInputMode assetSource index pendingChanges <| getMatchingAlbumsOrdered "" model.albums }, Cmd.none )
                ChangeImageIndex indexChange ->
                    let
                        newIndex =
                            loopImageIndexOverArray index indexChange (Array.length model.images)
                    in
                    ( { model | userMode = EditAsset inputMode assetSource newIndex pendingChanges searchResults }, Cmd.none )
                TextEditModeInputUpdate (TextInputAddition newKey) ->
                    let
                        newSearchString =
                            searchResults.searchString ++ newKey
                    in
                    ( { model | userMode = EditAsset inputMode assetSource index pendingChanges <| getMatchingAlbumsOrdered newSearchString model.albums }, Cmd.none )
                TextEditModeInputUpdate TextInputBackspace ->
                    let
                        newSearchString =
                            String.slice 0 (String.length searchResults.searchString - 1) searchResults.searchString
                    in
                    ( { model | userMode = EditAsset inputMode assetSource index pendingChanges <| getMatchingAlbumsOrdered newSearchString model.albums }, Cmd.none )

                UserActionGeneralEdit generalAction ->
                    handleGeneralActions model generalAction

handleGeneralActions : Model -> UserActionGeneral -> ( Model, Cmd Msg )
handleGeneralActions model action =
    case action of
        ChangeUserMode newMode ->
            ( { model | userMode = newMode }, Cmd.none )

        ReloadData ->
            ( { model | imagesLoadState = ImmichLoading, albumsLoadState = ImmichLoading }, Cmd.none )

        UnknownAction ->
            ( model, Cmd.none )


getMatchingAlbumsOrdered : String -> Array ImmichAlbum -> AlbumSearchResults
getMatchingAlbumsOrdered searchString albums =
    let
        albumsList =
            Array.toList albums
        albumsWithScores =
            List.map (\a -> { album = a, score = shittyFuzzyAlgorithmTest searchString a.albumName }) albumsList
    in
    { searchString = searchString
    , albumsWithScore =
        albumsWithScores
            |> List.filter (\a -> a.score > 0 || searchString == "")
            |> List.sortBy (\a -> ( a.score, a.album.assetCount ))
            |> List.reverse
    }

regexFromString : String -> Regex
regexFromString searchString =
    Regex.fromStringWith { caseInsensitive = True, multiline = False } searchString |> Maybe.withDefault Regex.never

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
