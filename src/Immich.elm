module Immich exposing (..)

import Date exposing (Date, fromIsoString)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import String exposing (split)
import Url.Builder exposing (crossOrigin, string)


type ImageOrder
    = CreatedDesc
    | CreatedAsc
    | ModifiedDesc
    | ModifiedAsc
    | Random


type CategorisationFilter
    = All
    | Uncategorised


type alias ImageSearchConfig =
    { order : ImageOrder
    , categorisation : CategorisationFilter
    , mediaType : MediaTypeFilter
    , status : StatusFilter
    }


type MediaTypeFilter
    = AllMedia
    | ImagesOnly
    | VideosOnly


type StatusFilter
    = AllStatuses
    | FavoritesOnly
    | ArchivedOnly


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
    , bulkUpdateAssets : String
    , apiKey : String
    }



-- Helper function to properly join URL paths using crossOrigin


joinUrl : String -> List String -> String
joinUrl baseUrl pathSegments =
    crossOrigin baseUrl pathSegments []



-- Helper function to build URL with query parameters using crossOrigin


buildUrlWithQuery : String -> List String -> List ( String, String ) -> String
buildUrlWithQuery baseUrl pathSegments queryParams =
    crossOrigin baseUrl pathSegments (List.map (\( key, value ) -> string key value) queryParams)


getImmichApiPaths : String -> String -> ImmichApiPaths
getImmichApiPaths baseUrl immichApiKey =
    { downloadAsset = \id -> joinUrl baseUrl [ "api", "assets", id, "original" ]
    , fetchMembershipForAsset = \assetId -> buildUrlWithQuery baseUrl [ "api", "albums" ] [ ( "assetId", assetId ) ]
    , searchRandom = joinUrl baseUrl [ "api", "search", "random" ]
    , searchAssets = joinUrl baseUrl [ "api", "search", "metadata" ]
    , searchSmart = joinUrl baseUrl [ "api", "search", "smart" ]
    , getAlbum = \id -> joinUrl baseUrl [ "api", "albums", id ]
    , putAlbumAssets = \id -> joinUrl baseUrl [ "api", "albums", id, "assets" ]
    , createAlbum = joinUrl baseUrl [ "api", "albums" ]
    , updateAsset = \id -> joinUrl baseUrl [ "api", "assets", id ]
    , bulkUpdateAssets = joinUrl baseUrl [ "api", "assets" ]
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
    , fileCreatedAt : Date
    , fileModifiedAt : Date
    , fileCreatedAtString : String -- Raw ISO timestamp for accurate sorting
    , fileModifiedAtString : String -- Raw ISO timestamp for accurate sorting
    , thumbhash : Maybe String
    , duration : Maybe String -- Duration in HH:MM:SS.mmm format for video assets
    }


type alias ImmichAlbum =
    { id : ImmichAlbumId
    , albumName : String
    , assetCount : Int
    , assets : List ImmichAsset

    -- , albumThumbnailAssetId : String
    , createdAt : Date
    }



-- GENERIC HTTP REQUEST BUILDERS


makeApiRequest :
    { method : String
    , url : String
    , apiKey : String
    , body : Http.Body
    , expect : Http.Expect Msg
    }
    -> Cmd Msg
makeApiRequest config =
    Http.request
        { method = config.method
        , headers = [ Http.header "x-api-key" config.apiKey ]
        , url = config.url
        , body = config.body
        , expect = config.expect
        , timeout = Nothing
        , tracker = Nothing
        }


makeGetRequest : String -> String -> Decode.Decoder a -> (Result Http.Error a -> Msg) -> Cmd Msg
makeGetRequest apiKey url decoder msgConstructor =
    makeApiRequest
        { method = "GET"
        , url = url
        , apiKey = apiKey
        , body = Http.emptyBody
        , expect = Http.expectJson msgConstructor decoder
        }


makePostRequest : String -> String -> Encode.Value -> Decode.Decoder a -> (Result Http.Error a -> Msg) -> Cmd Msg
makePostRequest apiKey url jsonBody decoder msgConstructor =
    makeApiRequest
        { method = "POST"
        , url = url
        , apiKey = apiKey
        , body = Http.jsonBody jsonBody
        , expect = Http.expectJson msgConstructor decoder
        }


makePutRequest : String -> String -> Encode.Value -> Decode.Decoder a -> (Result Http.Error a -> Msg) -> Cmd Msg
makePutRequest apiKey url jsonBody decoder msgConstructor =
    makeApiRequest
        { method = "PUT"
        , url = url
        , apiKey = apiKey
        , body = Http.jsonBody jsonBody
        , expect = Http.expectJson msgConstructor decoder
        }


makeDeleteRequest : String -> String -> Encode.Value -> (Result Http.Error () -> Msg) -> Cmd Msg
makeDeleteRequest apiKey url jsonBody msgConstructor =
    makeApiRequest
        { method = "DELETE"
        , url = url
        , apiKey = apiKey
        , body = Http.jsonBody jsonBody
        , expect = Http.expectWhatever msgConstructor
        }


makePutRequestWithWhatever : String -> String -> Encode.Value -> (Result Http.Error () -> Msg) -> Cmd Msg
makePutRequestWithWhatever apiKey url jsonBody msgConstructor =
    makeApiRequest
        { method = "PUT"
        , url = url
        , apiKey = apiKey
        , body = Http.jsonBody jsonBody
        , expect = Http.expectWhatever msgConstructor
        }



-- BODY CONSTRUCTION HELPERS


makeSimpleJsonBody : List ( String, Encode.Value ) -> Encode.Value
makeSimpleJsonBody fields =
    Encode.object fields


makeAssetIdsBody : List ImmichAssetId -> Encode.Value
makeAssetIdsBody assetIds =
    Encode.object [ ( "ids", Encode.list Encode.string assetIds ) ]


makeSearchBody : ImageSearchConfig -> Int -> Int -> Encode.Value
makeSearchBody config size page =
    makeSearchBodyWithAlbum config Nothing size page


makeSearchBodyWithAlbum : ImageSearchConfig -> Maybe ImmichAlbumId -> Int -> Int -> Encode.Value
makeSearchBodyWithAlbum config maybeAlbumId size page =
    let
        orderField =
            case config.order of
                CreatedDesc ->
                    [ ( "order", Encode.string "desc" )
                    , ( "orderBy", Encode.string "fileCreatedAt" )
                    ]

                CreatedAsc ->
                    [ ( "order", Encode.string "asc" )
                    , ( "orderBy", Encode.string "fileCreatedAt" )
                    ]

                ModifiedDesc ->
                    [ ( "order", Encode.string "desc" )
                    , ( "orderBy", Encode.string "fileModifiedAt" )
                    ]

                ModifiedAsc ->
                    [ ( "order", Encode.string "asc" )
                    , ( "orderBy", Encode.string "fileModifiedAt" )
                    ]

                Random ->
                    []

        -- Random uses different endpoint
        categorisationField =
            case config.categorisation of
                Uncategorised ->
                    [ ( "isNotInAlbum", Encode.bool True ) ]

                All ->
                    []

        mediaTypeField =
            case config.mediaType of
                AllMedia ->
                    []

                ImagesOnly ->
                    [ ( "type", Encode.string "IMAGE" ) ]

                VideosOnly ->
                    [ ( "type", Encode.string "VIDEO" ) ]

        statusField =
            case config.status of
                AllStatuses ->
                    []

                FavoritesOnly ->
                    [ ( "isFavorite", Encode.bool True ) ]

                ArchivedOnly ->
                    [ ( "isArchived", Encode.bool True ) ]

        albumField =
            case maybeAlbumId of
                Nothing ->
                    []

                Just albumId ->
                    [ ( "albumIds", Encode.list Encode.string [ albumId ] ) ]

        paginationFields =
            [ ( "size", Encode.int size )
            , ( "page", Encode.int page )
            ]
    in
    Encode.object (orderField ++ categorisationField ++ mediaTypeField ++ statusField ++ albumField ++ paginationFields)



-- API FUNCTIONS (REFACTORED)


getAllAlbums : String -> String -> Cmd Msg
getAllAlbums baseUrl key =
    makeGetRequest
        key
        (joinUrl baseUrl [ "api", "albums" ])
        (Decode.list albumDecoder)
        AlbumsFetched


getAlbum : ImmichApiPaths -> ImmichAlbumId -> Cmd Msg
getAlbum apiPaths albumId =
    makeGetRequest
        apiPaths.apiKey
        (apiPaths.getAlbum albumId)
        albumDecoder
        SingleAlbumFetched


fetchImagesPaginated : ImmichApiPaths -> ImageSearchConfig -> Int -> Int -> Cmd Msg
fetchImagesPaginated apiPaths config size page =
    let
        -- Use PaginatedImagesFetched for page 1, MoreImagesFetched for page 2+
        messageConstructor =
            if page == 1 then
                PaginatedImagesFetched

            else
                MoreImagesFetched page
    in
    case config.order of
        Random ->
            let
                categorisationField =
                    case config.categorisation of
                        Uncategorised ->
                            [ ( "isNotInAlbum", Encode.bool True ) ]

                        All ->
                            []

                mediaTypeField =
                    case config.mediaType of
                        AllMedia ->
                            []

                        ImagesOnly ->
                            [ ( "type", Encode.string "IMAGE" ) ]

                        VideosOnly ->
                            [ ( "type", Encode.string "VIDEO" ) ]

                statusField =
                    case config.status of
                        AllStatuses ->
                            []

                        FavoritesOnly ->
                            [ ( "isFavorite", Encode.bool True ) ]

                        ArchivedOnly ->
                            [ ( "isArchived", Encode.bool True ) ]

                paginationFields =
                    [ ( "size", Encode.int size )
                    , ( "page", Encode.int page )
                    ]

                randomBody =
                    Encode.object (categorisationField ++ mediaTypeField ++ statusField ++ paginationFields)
            in
            makePostRequest
                apiPaths.apiKey
                apiPaths.searchRandom
                randomBody
                paginatedAssetsDecoder
                messageConstructor

        _ ->
            makePostRequest
                apiPaths.apiKey
                apiPaths.searchAssets
                (makeSearchBody config size page)
                paginatedAssetsDecoder
                messageConstructor


searchAssetsPaginated : ImmichApiPaths -> String -> MediaTypeFilter -> StatusFilter -> Int -> Int -> Cmd Msg
searchAssetsPaginated apiPaths searchText mediaType status size page =
    let
        -- Use PaginatedImagesFetched for page 1, MoreImagesFetched for page 2+
        messageConstructor =
            if page == 1 then
                PaginatedImagesFetched

            else
                MoreImagesFetched page

        queryField =
            [ ( "query", Encode.string searchText ) ]

        mediaTypeField =
            case mediaType of
                AllMedia ->
                    []

                ImagesOnly ->
                    [ ( "type", Encode.string "IMAGE" ) ]

                VideosOnly ->
                    [ ( "type", Encode.string "VIDEO" ) ]

        statusField =
            case status of
                AllStatuses ->
                    []

                FavoritesOnly ->
                    [ ( "isFavorite", Encode.bool True ) ]

                ArchivedOnly ->
                    [ ( "isArchived", Encode.bool True ) ]

        paginationFields =
            [ ( "size", Encode.int size )
            , ( "page", Encode.int page )
            ]
    in
    makePostRequest
        apiPaths.apiKey
        apiPaths.searchSmart
        (makeSimpleJsonBody (queryField ++ mediaTypeField ++ statusField ++ paginationFields))
        paginatedAssetsDecoder
        messageConstructor


fetchAlbumAssetsWithFilters : ImmichApiPaths -> ImmichAlbumId -> ImageOrder -> MediaTypeFilter -> StatusFilter -> Cmd Msg
fetchAlbumAssetsWithFilters apiPaths albumId order mediaType status =
    -- WORKAROUND: Immich API doesn't properly respect orderBy with albumIds
    -- Fall back to getting album data and doing client-side sorting
    makeGetRequest
        apiPaths.apiKey
        (apiPaths.getAlbum albumId)
        albumDecoder
        (AlbumFetchedWithClientSideFiltering order mediaType status)


fetchMembershipForAsset : ImmichApiPaths -> ImmichAssetId -> Cmd Msg
fetchMembershipForAsset apiPaths assetId =
    makeGetRequest
        apiPaths.apiKey
        (apiPaths.fetchMembershipForAsset assetId)
        (albumToAssetWithMembershipDecoder assetId)
        AssetMembershipFetched


albumChangeAssetMembership : ImmichApiPaths -> ImmichAlbumId -> List ImmichAssetId -> Bool -> Cmd Msg
albumChangeAssetMembership apiPaths albumId assetIds isAddition =
    if isAddition then
        makePutRequestWithWhatever
            apiPaths.apiKey
            (apiPaths.putAlbumAssets albumId)
            (makeAssetIdsBody assetIds)
            AlbumAssetsChanged

    else
        makeDeleteRequest
            apiPaths.apiKey
            (apiPaths.putAlbumAssets albumId)
            (makeAssetIdsBody assetIds)
            AlbumAssetsChanged


createAlbum : ImmichApiPaths -> String -> Cmd Msg
createAlbum apiPaths albumName =
    makePostRequest
        apiPaths.apiKey
        apiPaths.createAlbum
        (makeSimpleJsonBody [ ( "albumName", Encode.string albumName ) ])
        albumDecoder
        AlbumCreated


updateAssetFavorite : ImmichApiPaths -> ImmichAssetId -> Bool -> Cmd Msg
updateAssetFavorite apiPaths assetId isFavorite =
    makePutRequest
        apiPaths.apiKey
        (apiPaths.updateAsset assetId)
        (makeSimpleJsonBody [ ( "isFavorite", Encode.bool isFavorite ) ])
        imageDecoder
        AssetUpdated


updateAssetArchived : ImmichApiPaths -> ImmichAssetId -> Bool -> Cmd Msg
updateAssetArchived apiPaths assetId isArchived =
    makePutRequest
        apiPaths.apiKey
        (apiPaths.updateAsset assetId)
        (makeSimpleJsonBody [ ( "isArchived", Encode.bool isArchived ) ])
        imageDecoder
        AssetUpdated


bulkUpdateAssetsFavorite : ImmichApiPaths -> List ImmichAssetId -> Bool -> Cmd Msg
bulkUpdateAssetsFavorite apiPaths assetIds isFavorite =
    let
        body =
            Encode.object
                [ ( "ids", Encode.list Encode.string assetIds )
                , ( "isFavorite", Encode.bool isFavorite )
                ]
    in
    makePutRequest
        apiPaths.apiKey
        apiPaths.bulkUpdateAssets
        body
        (Decode.list imageDecoder)
        BulkAssetsUpdated


bulkUpdateAssetsArchived : ImmichApiPaths -> List ImmichAssetId -> Bool -> Cmd Msg
bulkUpdateAssetsArchived apiPaths assetIds isArchived =
    let
        body =
            Encode.object
                [ ( "ids", Encode.list Encode.string assetIds )
                , ( "isArchived", Encode.bool isArchived )
                ]
    in
    makePutRequest
        apiPaths.apiKey
        apiPaths.bulkUpdateAssets
        body
        (Decode.list imageDecoder)
        BulkAssetsUpdated


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


paginatedAssetsDecoder : Decode.Decoder PaginatedAssetResponse
paginatedAssetsDecoder =
    Decode.map3
        (\assets total count ->
            let
                -- Calculate if there are more pages based on count returned
                -- If we got exactly 1000 items, there might be more
                hasNext =
                    count >= 1000
            in
            PaginatedAssetResponse assets total count hasNext
        )
        (Decode.field "assets" (Decode.field "items" (Decode.list imageDecoder)))
        (Decode.field "assets" (Decode.field "total" Decode.int))
        (Decode.field "assets" (Decode.field "count" Decode.int))


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



-- Parse duration string in HH:MM:SS.mmm format to seconds


parseDurationToSeconds : String -> Maybe Int
parseDurationToSeconds durationStr =
    let
        -- Remove milliseconds if present (everything after the dot)
        withoutMillis =
            if String.contains "." durationStr then
                Maybe.withDefault durationStr (List.head (String.split "." durationStr))

            else
                durationStr

        -- Split by colon to get hours, minutes, seconds
        parts =
            String.split ":" withoutMillis

        -- Convert each part to int
        intParts =
            List.map String.toInt parts
    in
    case intParts of
        [ Just hours, Just minutes, Just seconds ] ->
            Just (hours * 3600 + minutes * 60 + seconds)

        [ Just minutes, Just seconds ] ->
            Just (minutes * 60 + seconds)

        [ Just seconds ] ->
            Just seconds

        _ ->
            Nothing


dateDecoder : Decode.Decoder Date
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\dateString ->
                -- Always fall back to date-only parsing since Elm's Date.fromIsoString doesn't support full timestamps
                case fromIsoString (splitDateTimeToDate dateString) of
                    Ok date ->
                        Decode.succeed date

                    Err _ ->
                        Decode.fail ("Invalid date format: " ++ dateString)
            )



-- New decoder for full timestamp strings (for accurate sorting)


timestampStringDecoder : Decode.Decoder String
timestampStringDecoder =
    Decode.string


imageDecoder : Decode.Decoder ImmichAsset
imageDecoder =
    Decode.map8
        (\id path title mimeType isFavorite isArchived albumMembership timestamps ->
            { id = id
            , path = path
            , title = title
            , mimeType = mimeType
            , isFavourite = isFavorite
            , isArchived = isArchived
            , albumMembership = albumMembership
            , fileCreatedAt = timestamps.createdDate
            , fileModifiedAt = timestamps.modifiedDate
            , fileCreatedAtString = timestamps.createdString
            , fileModifiedAtString = timestamps.modifiedString
            , thumbhash = Nothing
            , duration = Nothing
            }
        )
        (Decode.field "id" Decode.string)
        (Decode.field "originalPath" Decode.string)
        (Decode.field "originalFileName" Decode.string)
        (Decode.field "originalMimeType" Decode.string)
        (Decode.field "isFavorite" Decode.bool)
        (Decode.field "isArchived" Decode.bool)
        (Decode.succeed [])
        (Decode.map4
            (\createdDate modifiedDate createdString modifiedString ->
                { createdDate = createdDate
                , modifiedDate = modifiedDate
                , createdString = createdString
                , modifiedString = modifiedString
                }
            )
            (Decode.field "fileCreatedAt" dateDecoder)
            (Decode.field "fileModifiedAt" dateDecoder)
            (Decode.field "fileCreatedAt" timestampStringDecoder)
            (Decode.field "fileModifiedAt" timestampStringDecoder)
        )
        |> Decode.andThen
            (\asset ->
                Decode.map2 (\thumbhash duration -> { asset | thumbhash = thumbhash, duration = duration })
                    (Decode.maybe (Decode.field "thumbhash" Decode.string))
                    (Decode.oneOf
                        [ Decode.maybe (Decode.field "duration" Decode.string)
                        , Decode.maybe (Decode.field "videoDuration" Decode.string)
                        , Decode.maybe (Decode.at [ "metadata", "duration" ] Decode.string)
                        , Decode.maybe (Decode.at [ "exifInfo", "duration" ] Decode.string)
                        , Decode.succeed Nothing
                        ]
                    )
            )


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


type alias PaginatedAssetResponse =
    { assets : List ImmichAsset
    , total : Int
    , count : Int
    , hasNextPage : Bool
    }


type Msg
    = AlbumsFetched (Result Http.Error (List ImmichAlbum))
    | SingleAlbumFetched (Result Http.Error ImmichAlbum)
    | ImagesFetched (Result Http.Error (List ImmichAsset))
    | PaginatedImagesFetched (Result Http.Error PaginatedAssetResponse)
    | MoreImagesFetched Int (Result Http.Error PaginatedAssetResponse) -- page number
    | AssetMembershipFetched (Result Http.Error AssetWithMembership)
    | AlbumAssetsChanged (Result Http.Error ())
    | AlbumCreated (Result Http.Error ImmichAlbum)
    | AssetUpdated (Result Http.Error ImmichAsset)
    | BulkAssetsUpdated (Result Http.Error (List ImmichAsset))
    | AlbumFetchedWithClientSideFiltering ImageOrder MediaTypeFilter StatusFilter (Result Http.Error ImmichAlbum)
