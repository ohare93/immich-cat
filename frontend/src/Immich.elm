module Immich exposing (..)

import Array exposing (Array)
import Date exposing (Date, fromIsoString)
import Html exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import String exposing (split)


type ImmichLoadState
    = ImmichLoading
    | ImmichLoadSuccess
    | ImmichLoadError Http.Error


type alias ImmichAsset =
    { id : String
    , path : String
    , title : String
    , mimeType : String
    , isFavourite : Bool
    , isArchived : Bool
    }


type alias ImmichAlbum =
    { id : String
    , albumName : String
    , assetCount : Int
    , assets : Array ImmichAsset
    , albumThumbnailAssetId : String
    , createdAt : Date
    }


type alias ImmichConfig =
    { apiUrl : String
    , apiKey : String
    }


getAllAlbums : String -> String -> Cmd Msg
getAllAlbums url key =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Content-Type" "application/json", Http.header "x-api-key" key ]
        , url = url ++ "/albums"
        , body = Http.emptyBody
        , expect = Http.expectJson AlbumsFetched (Decode.array albumDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


fetchRandomImages : String -> String -> Cmd Msg
fetchRandomImages url key =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json", Http.header "Accept" "application/json", Http.header "x-api-key" key ]
        , url = url ++ "/search/random"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "isNotInAlbum", Encode.bool True ) ]
                )
        , expect = Http.expectJson RandomImagesFetched (Decode.array imageDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


albumDecoder : Decode.Decoder ImmichAlbum
albumDecoder =
    Decode.map6 ImmichAlbum
        (Decode.field "id" Decode.string)
        (Decode.field "albumName" Decode.string)
        (Decode.field "assetCount" Decode.int)
        (Decode.field "assets" (Decode.array imageDecoder))
        (Decode.field "albumThumbnailAssetId" Decode.string)
        (Decode.field "createdAt" dateDecoder)


splitDateTimeToDate : String -> String
splitDateTimeToDate str =
    if String.contains "T" str then
        Maybe.withDefault str (List.head (split "T" str))

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


imageDecoder : Decode.Decoder ImmichAsset
imageDecoder =
    Decode.map6 ImmichAsset
        (Decode.field "id" Decode.string)
        (Decode.field "originalMimeType" Decode.string)
        (Decode.field "originalFilePath" Decode.string)
        (Decode.field "originalFileName" Decode.string)
        (Decode.field "isFavorite" Decode.bool)
        (Decode.field "isArchived" Decode.bool)


-- UPDATE --


type Msg
    = StartLoading
    | AlbumsFetched (Result Http.Error (Array ImmichAlbum))
    | RandomImagesFetched (Result Http.Error (Array ImmichAsset))


type alias Model r =
    { r
        | images : Array ImmichAsset
        , imagesLoadState : ImmichLoadState
        , albums : Array ImmichAlbum
        , albumsLoadState : ImmichLoadState
        , apiUrl : String
        , apiKey : String
    }


update : Msg -> Model r -> ( Model r, Cmd Msg )
update msg model =
    case msg of
        StartLoading ->
            ( { model | albumsLoadState = ImmichLoading, imagesLoadState = ImmichLoading }, getAllAlbums model.apiUrl model.apiKey )

        AlbumsFetched (Ok albums) ->
            ( { model | albums = albums, albumsLoadState = ImmichLoadSuccess }, fetchRandomImages model.apiUrl model.apiKey )

        AlbumsFetched (Err error) ->
            ( { model | albums = Array.empty, albumsLoadState = ImmichLoadError error }, Cmd.none )

        RandomImagesFetched (Ok images) ->
            ( { model | images = images, imagesLoadState = ImmichLoadSuccess }, Cmd.none )

        RandomImagesFetched (Err error) ->
            ( { model | images = Array.empty, imagesLoadState = ImmichLoadError error }, Cmd.none )
