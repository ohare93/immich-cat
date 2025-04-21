module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Browser.Events exposing (onKeyDown)
import Date
import Dict exposing (Dict)
import Helpers exposing (send)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Immich exposing (ImageWithMetadata, ImmichAlbum, ImmichLoadState(..), getAllAlbums)
import Json.Decode as Decode
import Regex exposing (Regex)


type Msg
    = Increment
    | KeyPress String
    | ImmichMsg Immich.Msg

type alias Flags =
    { test : Int
    , imagePrepend : String
    , immichApiKey : String
    , immichApiUrl : String
    }


type alias Model =
    { count : Int
    , key : String
    , imagePrepend : String
    , assetSelectMode : AssetSource
    , userMode : UserMode
    , test : Int
    -- Immich fields
    , images : Array ImageWithMetadata
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
    | SearchAssetInput String
    | SelectAlbumInput String (List ImmichAlbum)
    | EditAsset InputMode AssetSource Int (List AssetChange)

type InputMode
    = NormalMode
    | InsertMode

type AssetChange
    = AddToAlbum ImmichAlbum
    | RemoveFromAlbum ImmichAlbum
    | Delete
    | Favourite

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
    | ChangeImageIndex Int
    | UserActionGeneralEdit UserActionGeneral

init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        testImages =
            Array.fromList
                [ ImageWithMetadata "0001"
                    "images/imafight.jpg"
                    "Imafight"
                    "jpg"
                , ImageWithMetadata
                    "0002"
                    "images/dcemployees.jpg"
                    "dcemployees"
                    "jpg"
                , ImageWithMetadata
                    "0003"
                    "images/jordan.jpg"
                    "Jordan"
                    "jpg"
                ]

        testAlbums =
            Array.fromList
                [ ImmichAlbum "a" "J" 200 Array.empty "000001" (Date.fromOrdinalDate 2025 1)
                , ImmichAlbum "b" "ToBeSorted" 3000 Array.empty "000002" (Date.fromOrdinalDate 2025 1)
                , ImmichAlbum "c" "The World" 50 Array.empty "000003" (Date.fromOrdinalDate 2025 1)
                , ImmichAlbum "d" "Comics" 50 Array.empty "000004" (Date.fromOrdinalDate 2025 1)
                ]
    in
    ( { count = 7
      , key = ""
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


viewImage : String -> Int -> Array ImageWithMetadata -> Html msg
viewImage imagePrepend index images =
    let
        currentImage =
            Array.get index images
    in
    case currentImage of
        Just image ->
            if image.path == "" then
                div [] [ text "No Image" ]

            else
                div []
                    [ img [ src (imagePrepend ++ image.path), class "img-fluid" ] []
                    , div [] [ text image.title ]
                    ]

        Nothing ->
            div [] [ text "No Image" ]


viewEditAsset : Int -> String -> Int -> String -> Array ImageWithMetadata -> Int -> List AssetChange -> Html Msg
viewEditAsset test imagePrepend count key images index pendingChanges =
    div [ class "text-center" ]
        [ div [] [ text ("Count: " ++ String.fromInt count) ]
        , div [] [ text ("Key: " ++ key) ]
        , button
            [ class "btn btn-primary", onClick Increment ]
            [ text "+" ]
        , viewImage imagePrepend index images
        , text (String.fromInt index ++ "  ")
        , text (String.fromInt test)
        ]

viewSelectAlbum : String -> List ImmichAlbum -> Html Msg
viewSelectAlbum searchString matchingAlbums =
    div []
        [ div [] [ text "Select Album" ]
        , div [] [ text searchString ]
        , div []
            [ text "Matching Albums: "
            , div []
                (List.map
                    (\album ->
                        div [] [ text album.albumName ]
                    )
                    matchingAlbums
                )
            ]
        ]

view : Model -> Html Msg
view model =
    case model.userMode of
        MainMenu ->
            div [] [ text "Select Asset Source" ]
        SearchAssetInput searchString ->
            div []
                [ text "Input Search String"
                , div [] [ text searchString ]
                ]
        SelectAlbumInput searchString matchingAlbums ->
            viewSelectAlbum searchString matchingAlbums
        EditAsset _ _ index pendingChanges ->
            viewEditAsset model.test model.imagePrepend model.count model.key model.images index pendingChanges



-- UPDATE --


loopImageIndexOverArray : Int -> Int -> Int -> Int
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
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )
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
                            ChangeUserMode (EditAsset NormalMode Uncategorised 0 [])
                        "a" ->
                            ChangeUserMode (SelectAlbumInput "" (getMatchingAlbumsOrdered "" model.albums))
                        "s" ->
                            ChangeUserMode (SearchAssetInput "")
                        _ ->
                            UnknownAction
            in
            handleGeneralActions model generalAction
        SearchAssetInput searchString ->
            let
                isTextAddition =
                    isSupportedSearchLetter key
                userAction =
                    case ( isTextAddition, key ) of
                        ( True, _ ) ->
                            TextInputUpdate (TextInputAddition key)
                        ( False, "Backspace" ) ->
                            TextInputUpdate TextInputBackspace
                        ( False, "Escape" ) ->
                            UserActionGeneralSearch (ChangeUserMode MainMenu)
                        ( False, "Enter" ) ->
                            UserActionGeneralSearch (ChangeUserMode (EditAsset NormalMode (Search searchString) 0 []))
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
        SelectAlbumInput searchString matchingAlbums ->
            let
                isTextAddition =
                    isSupportedSearchLetter key
                userAction =
                    case ( isTextAddition, key ) of
                        ( True, _ ) ->
                            TextSelectInputUpdate (TextInputAddition key)
                        ( False, "Backspace" ) ->
                            TextSelectInputUpdate TextInputBackspace
                        ( False, "Escape" ) ->
                            UserActionGeneralAlbumSelect (ChangeUserMode MainMenu)
                        ( False, "Enter" ) ->
                            SelectAlbumIfMatching
                        _ ->
                            UserActionGeneralAlbumSelect UnknownAction
            in
            case userAction of
                TextSelectInputUpdate (TextInputAddition newKey) ->
                    let
                        newSearchString =
                            searchString ++ newKey
                    in
                    ( { model | userMode = SelectAlbumInput newSearchString (getMatchingAlbumsOrdered newSearchString model.albums) }, Cmd.none )
                TextSelectInputUpdate TextInputBackspace ->
                    let
                        newSearchString =
                            String.slice 0 (String.length searchString - 1) searchString
                    in
                    ( { model | userMode = SelectAlbumInput newSearchString (getMatchingAlbumsOrdered newSearchString model.albums) }, Cmd.none )
                SelectAlbumIfMatching ->
                    case matchingAlbums of
                        [] ->
                            ( model, Cmd.none )
                        [ album ] ->
                            ( { model | userMode = EditAsset NormalMode (Album album) 0 [] }, Cmd.none )
                        _ ->
                            ( model, Cmd.none )
                UserActionGeneralAlbumSelect action ->
                    handleGeneralActions model action
        EditAsset inputMode assetSource index pendingChanges ->
            let
                userAction =
                    if inputMode == InsertMode then
                        case key of
                            "Escape" ->
                                ChangeInputMode NormalMode
                            -- TODO: Handle input here
                            _ ->
                                UserActionGeneralEdit UnknownAction
                    else
                        case key of
                            "ArrowLeft" ->
                                ChangeImageIndex -1
                            "ArrowRight" ->
                                ChangeImageIndex 1
                            "Escape" ->
                                UserActionGeneralEdit (ChangeUserMode MainMenu)
                            "i" ->
                                ChangeInputMode InsertMode
                            "d" ->
                                AddToAssetChangeList Delete
                            "f" ->
                                AddToAssetChangeList Favourite
                            _ ->
                                UserActionGeneralEdit UnknownAction

                -- , ( "r", ReloadData )
                -- , ( "i", ChangeInputMode InsertMode )
                -- , ( "Escape", ChangeUserMode MainMenu )
            in
            case userAction of
                AddToAssetChangeList change ->
                    ( { model | userMode = EditAsset inputMode assetSource index (change :: pendingChanges) }, Cmd.none )
                ChangeInputMode newInputMode ->
                    ( { model | userMode = EditAsset newInputMode assetSource index pendingChanges }, Cmd.none )
                ChangeImageIndex indexChange ->
                    let
                        newIndex =
                            loopImageIndexOverArray index indexChange (Array.length model.images)
                    in
                    ( { model | userMode = EditAsset inputMode assetSource newIndex pendingChanges }, Cmd.none )

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


getMatchingAlbumsOrdered : String -> Array ImmichAlbum -> List ImmichAlbum
getMatchingAlbumsOrdered searchString albums =
    let
        regex =
            Regex.fromStringWith { caseInsensitive = True, multiline = False } (".*" ++ searchString ++ ".*") |> Maybe.withDefault Regex.never

        matchingAlbums =
            if searchString == "" then
                Array.toList albums
            else
                Array.toList albums
                    |> List.filter (\album -> Regex.contains regex album.albumName)
    in
    List.sortBy (\album -> album.assetCount) matchingAlbums


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
