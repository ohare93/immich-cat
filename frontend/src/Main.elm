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


-- | Search String
-- | Album ImmichAlbum

type UserMode
    = SelectAssets String
    | EditAsset AssetSource Int (List AssetChange)

type BucketMode
    = Normal


-- type alias TestModel =
--     { images : Array ImageWithMetadata
--     , image : ImageWithMetadata
--     , index : int
--     , changesToBeMade : List AssetChange
--     }

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
                [ ImmichAlbum "a" "Test" 50 Array.empty "000001" (Date.fromOrdinalDate 2025 1)
                , ImmichAlbum "b" "Test2" 50 Array.empty "000002" (Date.fromOrdinalDate 2025 1)
                , ImmichAlbum "c" "Test3" 50 Array.empty "000003" (Date.fromOrdinalDate 2025 1)
                , ImmichAlbum "d" "Test4" 50 Array.empty "000004" (Date.fromOrdinalDate 2025 1)
                ]
    in
    ( { count = 7
      , key = ""
      , imagePrepend = flags.imagePrepend
      , userMode = SelectAssets ""
      , assetSelectMode = NoAssets
      , bucketMode = Normal
      , test = flags.test
      -- Immich fields
      , images = testImages
      , imagesLoadState = ImmichLoading
      , albums = testAlbums
      , albumsLoadState = ImmichLoading
      , apiUrl = flags.immichApiUrl
      , apiKey = flags.immichApiKey
      }
    , getAllAlbums flags.immichApiUrl flags.immichApiKey |> Cmd.map ImmichMsg
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
        SelectAssets searchString ->
            div [] [ text "Select Assets" ]
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



-- UPDATE --


loopImageIndexOverArray : Int -> Int -> Int -> Int
loopImageIndexOverArray index step length =
    modBy length (index + step)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.userMode of
        SelectAssets searchString ->
            updateSelectAssets msg model
        EditAsset assetSource index pendingChanges ->
            updateEditAsset msg model assetSource index pendingChanges


updateSelectAssets : Msg -> Model -> ( Model, Cmd Msg )
updateSelectAssets msg model =
    case msg of
        KeyPress key ->
            case key of
                "u" ->
                    ( { model | userMode = EditAsset Uncategorised 0 [] }, Cmd.none )
                _ ->
                    ( model, Cmd.none )
        _ ->
            ( model, Cmd.none )

updateEditAsset : Msg -> Model -> AssetSource -> Int -> List AssetChange -> ( Model, Cmd Msg )
updateEditAsset msg model assetSource index pendingChanges =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        KeyPress key ->
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

                " " ->
                    ( { model | key = "" }, Cmd.none )

                _ ->
                    ( { model | key = key }, Cmd.none )

        ImmichMsg immichMsg ->
            Immich.update immichMsg model |> Tuple.mapBoth (\a -> a) (Cmd.map ImmichMsg)


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
