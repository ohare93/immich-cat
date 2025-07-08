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
    { downloadAsset : ImmichAssetId -> String
    , fetchMembershipForAsset : ImmichAssetId -> String
    , searchRandom : String
    , searchAssets : String
    , getAlbum : ImmichAssetId -> String
    , putAlbumAssets : ImmichAlbumId -> String
    , apiKey : String
    }

getImmichApiPaths : String -> String -> ImmichApiPaths
getImmichApiPaths immichUrl immichApiKey =
    { downloadAsset = \id -> immichUrl ++ "/assets/" ++ id ++ "/original"
    , fetchMembershipForAsset = \assetId -> immichUrl ++ "/albums?assetId=" ++ assetId
    , searchRandom = immichUrl ++ "/search/random"
    , searchAssets = immichUrl ++ "/search/metadata"
    , getAlbum = \id -> immichUrl ++ "/albums/" ++ id
    , putAlbumAssets = \id -> immichUrl ++ "/albums/" ++ id ++ "/assets"
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
    , albumMembership : List ImmichAlbumId
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

type SearchModifier
    = NotInAnyAlbum Bool
    | OrderDesc Bool
    | IsFavourited Bool


-- type, archived,

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

getAlbum : ImmichApiPaths -> ImmichAlbumId -> Cmd Msg
getAlbum apiPaths albumId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json", Http.header "x-api-key" apiPaths.apiKey ]
        , url = apiPaths.getAlbum albumId
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "order", Encode.string "desc" ) ]
                )
        , expect = Http.expectJson SingleAlbumFetched albumDecoder
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

fetchAllImages : ImmichApiPaths -> Cmd Msg
fetchAllImages apiPaths =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json", Http.header "Accept" "application/json", Http.header "x-api-key" apiPaths.apiKey ]
        , url = apiPaths.searchAssets
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "isNotInAlbum", Encode.bool True ), ( "order", Encode.string "desc" ) ]
                )
        , expect = Http.expectJson AllImagesFetched nestedAssetsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

fetchMembershipForAsset : ImmichApiPaths -> ImmichAssetId -> Cmd Msg
fetchMembershipForAsset apiPaths assetId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json", Http.header "x-api-key" apiPaths.apiKey ]
        , url = apiPaths.fetchMembershipForAsset assetId
        , body = Http.emptyBody
        , expect = Http.expectJson AssetMembershipFetched (albumToAssetWithMembershipDecoder assetId)
        , timeout = Nothing
        , tracker = Nothing
        }

albumChangeAssetMembership : ImmichApiPaths -> ImmichAlbumId -> List ImmichAssetId -> Bool -> Cmd Msg
albumChangeAssetMembership apiPaths albumId assetIds isAddition =
    Http.request
        { method =
            if isAddition then
                "PUT"
            else
                "DELETE"
        , headers = [ Http.header "x-api-key" apiPaths.apiKey ]
        , url = apiPaths.putAlbumAssets albumId
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "ids", Encode.list Encode.string assetIds ) ]
                )
        , expect = Http.expectWhatever AlbumAssetsChanged
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

nestedAssetsDecoder : Decode.Decoder (List ImmichAsset)
nestedAssetsDecoder =
    Decode.field "assets" (Decode.field "items" (Decode.list imageDecoder))

albumToAssetWithMembershipDecoder : ImmichAssetId -> Decode.Decoder AssetWithMembership
albumToAssetWithMembershipDecoder assetId =
    Decode.map2 AssetWithMembership
        (Decode.succeed assetId)
        (Decode.list (Decode.field "id" Decode.string))


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
    Decode.map7 ImmichAsset
        (Decode.field "id" Decode.string)
        (Decode.field "originalPath" Decode.string)
        (Decode.field "originalFileName" Decode.string)
        (Decode.field "originalMimeType" Decode.string)
        (Decode.field "isFavorite" Decode.bool)
        (Decode.field "isArchived" Decode.bool)
        (Decode.succeed [])

errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out."

        Http.NetworkError ->
            "Network error occurred."

        Http.BadStatus statusCode ->
            "Bad status: " ++ String.fromInt statusCode

        Http.BadBody message ->
            "Problem with response body: " ++ message


-- UPDATE --

type alias AssetWithMembership =
    { assetId : ImmichAssetId
    , albumIds : List ImmichAlbumId
    }

type Msg
    = AlbumsFetched (Result Http.Error (List ImmichAlbum))
    | SingleAlbumFetched (Result Http.Error ImmichAlbum)
    | RandomImagesFetched (Result Http.Error (List ImmichAsset))
    | AllImagesFetched (Result Http.Error (List ImmichAsset))
    | AssetMembershipFetched (Result Http.Error AssetWithMembership)
    | AlbumAssetsChanged (Result Http.Error ())


type alias Model r =
    { r
        | images : List ImmichAsset
        , imagesLoadState : ImmichLoadState
        , albums : List ImmichAlbum
        , albumsLoadState : ImmichLoadState
        , apiUrl : String
        , apiKey : String
    }
