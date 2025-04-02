port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Encode exposing (encode)
import Debug
import Date exposing (Date, fromIsoString)
import String exposing (split)
import List exposing (head)

-- MODEL --

type alias Model =
    { images : List Image
    , currentIndex : Int
    , rawAlbumsJson : String
    , rawImagesJson : String
    , apiUrl : String
    , apiKey : String
    , albums : List Album
    , currentImageUrl : String
    , isLoading : Bool
    }


type alias Image =
    { id : String
    , url : String
    , originalMimeType : String
    }


type alias Album =
    { id : String
    , albumName : String
    , assetCount : Int
    , albumThumbnailAssetId : String
    , createdAt: Date
    }


type alias Flags =
    { apiUrl : String
    , apiKey : String
    }


port downloadFile : String -> Cmd msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { images = []
      , currentIndex = 0
      , rawAlbumsJson = ""
      , rawImagesJson = ""
      , apiUrl = flags.apiUrl
      , apiKey = flags.apiKey
      , albums = []
      , currentImageUrl = ""
      , isLoading = False
      }
    , Cmd.none
    )


-- HTTP REQUEST --

corsProxy : String
corsProxy =
    "http://localhost:8080/"


getAllAlbums : String -> String -> Cmd Msg
getAllAlbums url key =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Content-Type" "application/json", Http.header "x-api-key" key ]
        , url = corsProxy ++ url ++ "/albums"
        , body = Http.emptyBody
        , expect = Http.expectJson AlbumsFetched (Decode.list albumDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


fetchImages : String -> String -> Cmd Msg
fetchImages url key =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json", Http.header "Accept" "application/json", Http.header "x-api-key" key ]
        , url = corsProxy ++ url ++ "/search/random"
        , body = Http.jsonBody payload
        , expect = Http.expectJson ImagesFetched (Decode.list imageIdDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


payload : Encode.Value
payload =
    Encode.object
        [ ("isNotInAlbum", Encode.bool True) ]


albumDecoder : Decode.Decoder Album
albumDecoder =
    Decode.map5 Album
        (Decode.field "id" Decode.string)
        (Decode.field "albumName" Decode.string)
        (Decode.field "assetCount" Decode.int)
        (Decode.field "albumThumbnailAssetId" Decode.string)
        (Decode.field "createdAt" dateDecoder)


splitDateTimeToDate : String -> String
splitDateTimeToDate str =
    if String.contains "T" str then
        Maybe.withDefault str (head (split "T" str))
    else
        str


dateDecoder : Decode.Decoder Date
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\dateString ->
                case fromIsoString (splitDateTimeToDate dateString) of
                    Ok date ->
                        Decode.succeed date

                    Err _ ->
                        Decode.fail "Invalid date format"
            )


albumEncoder : Album -> Encode.Value
albumEncoder album =
    Encode.object
        [ ("id", Encode.string album.id)
        , ("albumName", Encode.string album.albumName)
        , ("assetCount", Encode.int album.assetCount)
        , ("albumThumbnailAssetId", Encode.string album.albumThumbnailAssetId)
        , ("createdAt", Encode.string (Date.toIsoString album.createdAt))
        ]


imageIdDecoder : Decode.Decoder String
imageIdDecoder =
    Decode.field "id" Decode.string


imageDecoder : Decode.Decoder Image
imageDecoder =
    Decode.map3 Image
        (Decode.field "id" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "originalMimeType" Decode.string)


-- UPDATE --

type Msg
    = StartLoading
    | AlbumsFetched (Result Http.Error (List Album))
    | ImagesFetched (Result Http.Error (List String))
    | ImageFetched (Result Http.Error Image)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartLoading ->
            ( { model | isLoading = True }, getAllAlbums model.apiUrl model.apiKey )

        AlbumsFetched (Ok albums) ->
            let
                rawAlbumsJson = encode 4 (Encode.list albumEncoder albums)
            in
            ( { model | albums = albums, rawAlbumsJson = rawAlbumsJson, isLoading = False }, fetchImages model.apiUrl model.apiKey )

        AlbumsFetched (Err error) ->
            let
                rawAlbumsJson = "Error: " ++ Debug.toString error
            in
            ( { model | rawAlbumsJson = rawAlbumsJson, isLoading = False }, Cmd.none )

        ImagesFetched (Ok ids) ->
            let
                rawImagesJson = encode 4 (Encode.list Encode.string ids)
            in
            ( { model | rawImagesJson = rawImagesJson }, Cmd.none )

        ImagesFetched (Err error) ->
            let
                rawImagesJson = "Error: " ++ Debug.toString error
            in
            ( { model | rawImagesJson = rawImagesJson }, Cmd.none )

        ImageFetched (Ok image) ->
            let
                imageUrl = model.apiUrl ++ "/assets/" ++ image.id ++ "/original"
            in
            ( { model | currentImageUrl = imageUrl }, downloadFile imageUrl )

        ImageFetched (Err error) ->
            let
                rawImagesJson = "Error: " ++ Debug.toString error
            in
            ( { model | rawImagesJson = rawImagesJson }, Cmd.none )

        -- Add more message types as needed for user interaction


-- VIEW --

viewAlbum : Album -> Html Msg
viewAlbum album =
    li [] [ text (album.albumName ++ " (" ++ String.fromInt album.assetCount ++ " assets)") ]


viewImage : String -> Html Msg
viewImage url =
    img [ src url ] []


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick StartLoading ] [ text "Load Data" ]
        , if model.isLoading then
            div [] [ text "Loading..." ]
          else
            div []
                [ h1 [] [ text "Albums" ]
                , ul [] (List.map viewAlbum model.albums)
                , pre [] [ text model.rawAlbumsJson ]
                , h1 [] [ text "Image IDs" ]
                , ul [] (List.map (\image -> li [] [ text image.id ]) model.images)
                , pre [] [ text model.rawImagesJson ]
                , if model.currentImageUrl /= "" then
                    viewImage model.currentImageUrl
                  else
                    text "No images to display"
                ]
        ]


-- MAIN --

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }