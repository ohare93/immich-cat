module Main exposing (main)
import Array exposing (Array)
import Browser exposing (element)
import Browser.Events exposing (onKeyDown)
import Date
import Dict exposing (Dict)
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
    , bucketMode : BucketMode
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
    = Normal
    | SearchAssetInput String
    | SelectAlbumInput String (List ImmichAlbum)
    | EditAsset AssetSource Int (List AssetChange)

type BucketMode
    = BucketNormal

type AssetChange
    = AddToAlbum ImmichAlbum
    | RemoveFromAlbum ImmichAlbum
    | Delete
    | Favourite

defaultAssetKeys : Dict String AssetChange
defaultAssetKeys =
    Dict.fromList
        [ ( "d", Delete )
        , ( "f", Favourite )
        ]


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
      , userMode = Normal
      , assetSelectMode = NoAssets
      , bucketMode = BucketNormal
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


view : Model -> Html Msg
view model =
    case model.userMode of
        Normal ->
            div [] [ text "Select Asset Source" ]
        SearchAssetInput searchString ->
            div []
                [ text "Input Search String"
                , div [] [ text searchString ]
                ]
        SelectAlbumInput searchString matchingAlbums ->
            viewSelectAlbum searchString matchingAlbums
        EditAsset _ index pendingChanges ->
            viewEditAsset model.test model.imagePrepend model.count model.key model.images index pendingChanges


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
            case model.userMode of
                Normal ->
                    handleKeyInNormal key model
                SearchAssetInput searchString ->
                    handleKeyInSearchAsset key model searchString
                SelectAlbumInput searchString matchingAlbums ->
                    handleKeyInSelectAlbum key model searchString matchingAlbums
                EditAsset assetSource index pendingChanges ->
                    handleKeyInEditAsset key model assetSource index pendingChanges

handleKeyInNormal : String -> Model -> ( Model, Cmd Msg )
handleKeyInNormal key model =
    if key == "u" then
        ( { model | userMode = EditAsset Uncategorised 0 [] }, Cmd.none )

    else if key == "a" then
        ( { model | userMode = SelectAlbumInput "" (getMatchingAlbumsOrdered "" model.albums) }, Cmd.none )

    else if key == "s" || key == "/" then
        ( { model | userMode = SearchAssetInput "" }, Cmd.none )

    else
        ( model, Cmd.none )

handleKeyInSearchAsset : String -> Model -> String -> ( Model, Cmd Msg )
handleKeyInSearchAsset key model searchString =
    if isSupportedSearchLetter key then
        let
            newSearchString =
                searchString ++ key
        in
        ( { model | userMode = SearchAssetInput newSearchString }, Cmd.none )
    else
        case key of
            "Escape" ->
                ( { model | userMode = Normal }, Cmd.none )

            "Backspace" ->
                let
                    newSearchString =
                        String.slice 0 (String.length searchString - 1) searchString
                in
                ( { model | userMode = SearchAssetInput newSearchString }, Cmd.none )

            "Enter" ->
                ( { model | userMode = EditAsset (Search searchString) 0 [] }, Cmd.none )
            _ ->
                ( model, Cmd.none )

handleKeyInSelectAlbum : String -> Model -> String -> List ImmichAlbum -> ( Model, Cmd Msg )
handleKeyInSelectAlbum key model searchString matchingAlbums =
    if isSupportedSearchLetter key then
        let
            newSearchString =
                searchString ++ key
        in
        ( { model | userMode = SelectAlbumInput newSearchString (getMatchingAlbumsOrdered newSearchString model.albums) }, Cmd.none )
    else
        case key of
            "Escape" ->
                ( { model | userMode = Normal }, Cmd.none )

            "Backspace" ->
                let
                    newSearchString =
                        String.slice 0 (String.length searchString - 1) searchString
                in
                ( { model | userMode = SelectAlbumInput newSearchString (getMatchingAlbumsOrdered newSearchString model.albums) }, Cmd.none )

            "Enter" ->
                case matchingAlbums of
                    [] ->
                        ( model, Cmd.none )
                    [ album ] ->
                        ( { model | userMode = EditAsset (Album album) 0 [] }, Cmd.none )
                    _ ->
                        ( model, Cmd.none )
            _ ->
                ( model, Cmd.none )

handleKeyInEditAsset : String -> Model -> AssetSource -> Int -> List AssetChange -> ( Model, Cmd Msg )
handleKeyInEditAsset key model assetSource index pendingChanges =
    case key of
        "ArrowLeft" ->
            let
                newIndex =
                    loopImageIndexOverArray index -1 (Array.length model.images)
            in
            ( { model | userMode = EditAsset assetSource newIndex pendingChanges }, Cmd.none )

        "ArrowRight" ->
            let
                newIndex =
                    loopImageIndexOverArray index 1 (Array.length model.images)
            in
            ( { model | userMode = EditAsset assetSource newIndex pendingChanges }, Cmd.none )

        "Escape" ->
            ( { model | userMode = Normal }, Cmd.none )

        " " ->
            ( { model | key = "" }, Cmd.none )

        _ ->
            ( { model | key = key }, Cmd.none )


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
