module Immich exposing (..)

import Date exposing (Date, fromIsoString)
import Html exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import String exposing (split)
import Url exposing (percentEncode)
import Url.Builder exposing (absolute, crossOrigin, string)


type ImageOrder
    = Desc
    | Asc
    | Random

type CategorisationFilter
    = All
    | Uncategorised

type alias ImageSearchConfig =
    { order : ImageOrder
    , categorisation : CategorisationFilter
    }

type ImmichLoadState
    = ImmichLoading
    | ImmichLoadSuccess
    | ImmichLoadError Http.Error

type alias ImmichApiPaths =
    { downloadAsset : ImmichAssetId -> String
    , fetchMembershipForAsset : ImmichAssetId -> String
    , searchRandom : String
    , searchAssets : String
    , searchSmart : String
    , getAlbum : ImmichAssetId -> String
    , putAlbumAssets : ImmichAlbumId -> String
    , createAlbum : String
    , updateAsset : ImmichAssetId -> String
    , apiKey : String
    }

-- Helper function to properly join URL paths using crossOrigin
joinUrl : String -> List String -> String
joinUrl baseUrl pathSegments =
    crossOrigin baseUrl pathSegments []

-- Helper function to build URL with query parameters using crossOrigin
buildUrlWithQuery : String -> List String -> List (String, String) -> String
buildUrlWithQuery baseUrl pathSegments queryParams =
    crossOrigin baseUrl pathSegments (List.map (\(key, value) -> string key value) queryParams)

getImmichApiPaths : String -> String -> ImmichApiPaths
getImmichApiPaths baseUrl immichApiKey =
    { downloadAsset = \id -> joinUrl baseUrl ["api", "assets", id, "original"]
    , fetchMembershipForAsset = \assetId -> buildUrlWithQuery baseUrl ["api", "albums"] [("assetId", assetId)]
    , searchRandom = joinUrl baseUrl ["api", "search", "random"]
    , searchAssets = joinUrl baseUrl ["api", "search", "metadata"]
    , searchSmart = joinUrl baseUrl ["api", "search", "smart"]
    , getAlbum = \id -> joinUrl baseUrl ["api", "albums", id]
    , putAlbumAssets = \id -> joinUrl baseUrl ["api", "albums", id, "assets"]
    , createAlbum = joinUrl baseUrl ["api", "albums"]
    , updateAsset = \id -> joinUrl baseUrl ["api", "assets", id]
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
    { baseUrl : String
    , apiKey : String
    }

type SearchModifier
    = NotInAnyAlbum Bool
    | OrderDesc Bool
    | IsFavourited Bool


-- type, archived,

getAllAlbums : String -> String -> Cmd Msg
getAllAlbums baseUrl key =
    Http.request
        { method = "GET"
        , headers = [ Http.header "x-api-key" key ]
        , url = joinUrl baseUrl ["api", "albums"]
        , body = Http.emptyBody
        , expect = Http.expectJson AlbumsFetched (Decode.list albumDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

getAlbum : ImmichApiPaths -> ImmichAlbumId -> Cmd Msg
getAlbum apiPaths albumId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "x-api-key" apiPaths.apiKey ]
        , url = apiPaths.getAlbum albumId
        , body = Http.emptyBody
        , expect = Http.expectJson SingleAlbumFetched albumDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

fetchImages : ImmichApiPaths -> ImageSearchConfig -> Cmd Msg
fetchImages apiPaths config =
    case config.order of
        Random ->
            let
                body = 
                    case config.categorisation of
                        Uncategorised ->
                            Http.jsonBody (Encode.object [ ( "isNotInAlbum", Encode.bool True ) ])
                        All ->
                            Http.jsonBody (Encode.object [])
            in
            Http.request
                { method = "POST"
                , headers = [ Http.header "x-api-key" apiPaths.apiKey ]
                , url = apiPaths.searchRandom
                , body = body
                , expect = Http.expectJson ImagesFetched (Decode.list imageDecoder)
                , timeout = Nothing
                , tracker = Nothing
                }
        
        _ ->
            let
                orderString = 
                    case config.order of
                        Desc -> "desc"
                        Asc -> "asc"
                        Random -> "desc" -- fallback, shouldn't happen
                
                bodyFields =
                    [ ( "order", Encode.string orderString ) ] ++
                    (case config.categorisation of
                        Uncategorised -> [ ( "isNotInAlbum", Encode.bool True ) ]
                        All -> []
                    )
            in
            Http.request
                { method = "POST"
                , headers = [ Http.header "x-api-key" apiPaths.apiKey ]
                , url = apiPaths.searchAssets
                , body = Http.jsonBody (Encode.object bodyFields)
                , expect = Http.expectJson ImagesFetched nestedAssetsDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

searchAssets : ImmichApiPaths -> String -> Cmd Msg
searchAssets apiPaths searchText =
    let
        bodyFields =
            [ ( "query", Encode.string searchText )
            ]
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "x-api-key" apiPaths.apiKey ]
        , url = apiPaths.searchSmart
        , body = Http.jsonBody (Encode.object bodyFields)
        , expect = Http.expectJson ImagesFetched nestedAssetsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

fetchMembershipForAsset : ImmichApiPaths -> ImmichAssetId -> Cmd Msg
fetchMembershipForAsset apiPaths assetId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "x-api-key" apiPaths.apiKey ]
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

createAlbum : ImmichApiPaths -> String -> Cmd Msg
createAlbum apiPaths albumName =
    Http.request
        { method = "POST"
        , headers = [ Http.header "x-api-key" apiPaths.apiKey ]
        , url = apiPaths.createAlbum
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "albumName", Encode.string albumName ) ]
                )
        , expect = Http.expectJson AlbumCreated albumDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

updateAssetFavorite : ImmichApiPaths -> ImmichAssetId -> Bool -> Cmd Msg
updateAssetFavorite apiPaths assetId isFavorite =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "x-api-key" apiPaths.apiKey ]
        , url = apiPaths.updateAsset assetId
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "isFavorite", Encode.bool isFavorite ) ]
                )
        , expect = Http.expectJson AssetUpdated imageDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

updateAssetArchived : ImmichApiPaths -> ImmichAssetId -> Bool -> Cmd Msg
updateAssetArchived apiPaths assetId isArchived =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "x-api-key" apiPaths.apiKey ]
        , url = apiPaths.updateAsset assetId
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "isArchived", Encode.bool isArchived ) ]
                )
        , expect = Http.expectJson AssetUpdated imageDecoder
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
    | ImagesFetched (Result Http.Error (List ImmichAsset))
    | AssetMembershipFetched (Result Http.Error AssetWithMembership)
    | AlbumAssetsChanged (Result Http.Error ())
    | AlbumCreated (Result Http.Error ImmichAlbum)
    | AssetUpdated (Result Http.Error ImmichAsset)


type alias Model r =
    { r
        | images : List ImmichAsset
        , imagesLoadState : ImmichLoadState
        , albums : List ImmichAlbum
        , albumsLoadState : ImmichLoadState
        , baseUrl : String
        , apiKey : String
    }
