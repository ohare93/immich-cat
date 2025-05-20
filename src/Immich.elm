module Immich exposing (..)

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

type alias ImmichApiPaths =
    { getAsset : ImmichAssetId -> String
    , apiKey : String
    }

getImmichApiPaths : String -> String -> ImmichApiPaths
getImmichApiPaths immichUrl immichApiKey =
    { getAsset = \id -> immichUrl ++ "/assets/" ++ id ++ "/original"
    , apiKey = immichApiKey
    }


type alias ImmichAssetId =
    String

type alias ImmichAlbumId =
    String


type alias ImmichAsset =
    { id : ImmichAssetId
    , path : String
    , title : String
    , mimeType : String
    , isFavourite : Bool
    , isArchived : Bool
    }


type alias ImmichAlbum =
    { id : ImmichAlbumId
    , albumName : String
    , assetCount : Int
    , assets : List ImmichAsset
    -- , albumThumbnailAssetId : String
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
        , expect = Http.expectJson AlbumsFetched (Decode.list albumDecoder)
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
        , expect = Http.expectJson RandomImagesFetched (Decode.list imageDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


albumDecoder : Decode.Decoder ImmichAlbum
albumDecoder =
    Decode.map5 ImmichAlbum
        (Decode.field "id" Decode.string)
        (Decode.field "albumName" Decode.string)
        (Decode.field "assetCount" Decode.int)
        (Decode.field "assets" (Decode.list imageDecoder))
        -- (Decode.field "albumThumbnailAssetId" Decode.string)
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
        (Decode.field "originalPath" Decode.string)
        (Decode.field "originalFileName" Decode.string)
        (Decode.field "originalMimeType" Decode.string)
        (Decode.field "isFavorite" Decode.bool)
        (Decode.field "isArchived" Decode.bool)


-- UPDATE --


type Msg
    = StartLoading
    | AlbumsFetched (Result Http.Error (List ImmichAlbum))
    | RandomImagesFetched (Result Http.Error (List ImmichAsset))


type alias Model r =
    { r
        | images : List ImmichAsset
        , imagesLoadState : ImmichLoadState
        , albums : List ImmichAlbum
        , albumsLoadState : ImmichLoadState
        , apiUrl : String
        , apiKey : String
    }
