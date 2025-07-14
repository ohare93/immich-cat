module ImmichTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Fuzz exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Date
import Immich exposing (..)
import Http


-- Test data generators for random testing
generateTestAlbum : String -> String -> Int -> ImmichAlbum
generateTestAlbum id name assetCount =
    { id = id
    , albumName = name
    , assetCount = assetCount
    , assets = []
    , createdAt = Date.fromRataDie 737790 -- January 1, 2020
    }

generateTestAsset : String -> String -> ImmichAsset
generateTestAsset id fileName =
    { id = id
    , path = "/path/to/" ++ fileName
    , title = fileName
    , mimeType = "image/jpeg"
    , isFavourite = False
    , isArchived = False
    , albumMembership = []
    }

-- JSON encoding/decoding tests
suite : Test
suite =
    describe "Immich API Module Tests"
        [ describe "Body Construction Helpers"
            [ test "makeSimpleJsonBody creates correct JSON" <|
                \_ ->
                    let
                        result = makeSimpleJsonBody [ ( "key1", Encode.string "value1" ), ( "key2", Encode.int 42 ) ]
                        expected = Encode.object [ ( "key1", Encode.string "value1" ), ( "key2", Encode.int 42 ) ]
                    in
                    Expect.equal (Encode.encode 0 result) (Encode.encode 0 expected)
            
            , test "makeAssetIdsBody creates correct asset IDs structure" <|
                \_ ->
                    let
                        assetIds = [ "asset1", "asset2", "asset3" ]
                        result = makeAssetIdsBody assetIds
                        expected = Encode.object [ ( "ids", Encode.list Encode.string assetIds ) ]
                    in
                    Expect.equal (Encode.encode 0 result) (Encode.encode 0 expected)
            
            , test "makeSearchBody with Desc order and All categorisation" <|
                \_ ->
                    let
                        config = { order = Desc, categorisation = All, mediaType = AllMedia, status = AllStatuses }
                        result = makeSearchBody config
                        expected = Encode.object [ ( "order", Encode.string "desc" ) ]
                    in
                    Expect.equal (Encode.encode 0 result) (Encode.encode 0 expected)
            
            , test "makeSearchBody with Asc order and Uncategorised" <|
                \_ ->
                    let
                        config = { order = Asc, categorisation = Uncategorised, mediaType = AllMedia, status = AllStatuses }
                        result = makeSearchBody config
                        expected = Encode.object 
                            [ ( "order", Encode.string "asc" )
                            , ( "isNotInAlbum", Encode.bool True )
                            ]
                    in
                    Expect.equal (Encode.encode 0 result) (Encode.encode 0 expected)
                    
            , test "makeSearchBody with Random order (no order field)" <|
                \_ ->
                    let
                        config = { order = Random, categorisation = All, mediaType = AllMedia, status = AllStatuses }
                        result = makeSearchBody config
                        expected = Encode.object []
                    in
                    Expect.equal (Encode.encode 0 result) (Encode.encode 0 expected)
            ]
        
        , describe "JSON Decoders"
            [ test "albumDecoder decodes valid album JSON" <|
                \_ ->
                    let
                        albumJson = """
                        {
                            "id": "album123",
                            "albumName": "Test Album",
                            "assetCount": 5,
                            "assets": [],
                            "createdAt": "2020-01-01"
                        }
                        """
                        result = Decode.decodeString albumDecoder albumJson
                        expected = generateTestAlbum "album123" "Test Album" 5
                    in
                    case result of
                        Ok album ->
                            Expect.all
                                [ \() -> Expect.equal album.id expected.id
                                , \() -> Expect.equal album.albumName expected.albumName
                                , \() -> Expect.equal album.assetCount expected.assetCount
                                ]
                                ()
                        Err _ ->
                            Expect.fail "Failed to decode album JSON"
            
            , test "imageDecoder decodes valid asset JSON" <|
                \_ ->
                    let
                        assetJson = """
                        {
                            "id": "asset123",
                            "originalPath": "/path/to/test.jpg",
                            "originalFileName": "test.jpg",
                            "originalMimeType": "image/jpeg",
                            "isFavorite": true,
                            "isArchived": false
                        }
                        """
                        result = Decode.decodeString imageDecoder assetJson
                    in
                    case result of
                        Ok asset ->
                            Expect.all
                                [ \() -> Expect.equal asset.id "asset123"
                                , \() -> Expect.equal asset.path "/path/to/test.jpg"
                                , \() -> Expect.equal asset.title "test.jpg"
                                , \() -> Expect.equal asset.mimeType "image/jpeg"
                                , \() -> Expect.equal asset.isFavourite True
                                , \() -> Expect.equal asset.isArchived False
                                ]
                                ()
                        Err _ ->
                            Expect.fail "Failed to decode asset JSON"
            
            , test "nestedAssetsDecoder handles nested asset structure" <|
                \_ ->
                    let
                        nestedJson = """
                        {
                            "assets": {
                                "items": [
                                    {
                                        "id": "asset1",
                                        "originalPath": "/path1.jpg",
                                        "originalFileName": "file1.jpg",
                                        "originalMimeType": "image/jpeg",
                                        "isFavorite": false,
                                        "isArchived": false
                                    }
                                ]
                            }
                        }
                        """
                        result = Decode.decodeString nestedAssetsDecoder nestedJson
                    in
                    case result of
                        Ok assets ->
                            case List.head assets of
                                Just asset ->
                                    Expect.equal asset.id "asset1"
                                Nothing ->
                                    Expect.fail "Expected at least one asset"
                        Err _ ->
                            Expect.fail "Failed to decode nested assets JSON"
            ]
        
        , describe "URL Construction"
            [ test "joinUrl constructs correct URLs" <|
                \_ ->
                    let
                        result = joinUrl "https://example.com" ["api", "albums", "123"]
                        expected = "https://example.com/api/albums/123"
                    in
                    Expect.equal result expected
            
            , test "buildUrlWithQuery constructs URLs with query parameters" <|
                \_ ->
                    let
                        result = buildUrlWithQuery "https://example.com" ["api", "albums"] [("assetId", "123"), ("count", "5")]
                    in
                    if String.contains "assetId=123" result && String.contains "count=5" result then
                        Expect.pass
                    else
                        Expect.fail ("Expected URL to contain query parameters, got: " ++ result)
            
            , test "getImmichApiPaths generates correct API paths" <|
                \_ ->
                    let
                        apiPaths = getImmichApiPaths "https://example.com" "test-key"
                    in
                    Expect.all
                        [ \() -> Expect.equal apiPaths.apiKey "test-key"
                        , \() -> Expect.equal (apiPaths.downloadAsset "123") "https://example.com/api/assets/123/original"
                        , \() -> Expect.equal apiPaths.searchRandom "https://example.com/api/search/random"
                        , \() -> Expect.equal apiPaths.createAlbum "https://example.com/api/albums"
                        ]
                        ()
            ]
        
        , describe "Error Handling"
            [ test "errorToString handles different HTTP errors" <|
                \_ ->
                    let
                        testCases =
                            [ ( Http.BadUrl "invalid-url", "Bad URL: invalid-url" )
                            , ( Http.Timeout, "Request timed out." )
                            , ( Http.NetworkError, "Network error occurred." )
                            , ( Http.BadStatus 404, "Bad status: 404" )
                            , ( Http.BadBody "Invalid JSON", "Problem with response body: Invalid JSON" )
                            ]
                    in
                    testCases
                        |> List.map (\(error, expected) -> 
                            Expect.equal (errorToString error) expected)
                        |> List.foldl (\expectation acc -> 
                            Expect.all [(\() -> acc), (\() -> expectation)] ()) (Expect.pass)
            ]
        
        , describe "Integration Tests with Random Data"
            [ fuzz3 string string int "Album encoding/decoding roundtrip" <|
                \id name count ->
                    let
                        album = generateTestAlbum id name (abs count)
                        encoded = 
                            Encode.object
                                [ ( "id", Encode.string album.id )
                                , ( "albumName", Encode.string album.albumName )
                                , ( "assetCount", Encode.int album.assetCount )
                                , ( "assets", Encode.list (always (Encode.object [])) album.assets )
                                , ( "createdAt", Encode.string "2020-01-01" )
                                ]
                        decoded = Decode.decodeValue albumDecoder encoded
                    in
                    case decoded of
                        Ok decodedAlbum ->
                            Expect.all
                                [ \() -> Expect.equal decodedAlbum.id album.id
                                , \() -> Expect.equal decodedAlbum.albumName album.albumName
                                , \() -> Expect.equal decodedAlbum.assetCount album.assetCount
                                ]
                                ()
                        Err _ ->
                            Expect.fail "Roundtrip encoding/decoding failed"
            
            , fuzz2 string string "Asset encoding/decoding roundtrip" <|
                \id fileName ->
                    let
                        asset = generateTestAsset id fileName
                        encoded = 
                            Encode.object
                                [ ( "id", Encode.string asset.id )
                                , ( "originalPath", Encode.string asset.path )
                                , ( "originalFileName", Encode.string asset.title )
                                , ( "originalMimeType", Encode.string asset.mimeType )
                                , ( "isFavorite", Encode.bool asset.isFavourite )
                                , ( "isArchived", Encode.bool asset.isArchived )
                                ]
                        decoded = Decode.decodeValue imageDecoder encoded
                    in
                    case decoded of
                        Ok decodedAsset ->
                            Expect.all
                                [ \() -> Expect.equal decodedAsset.id asset.id
                                , \() -> Expect.equal decodedAsset.title asset.title
                                , \() -> Expect.equal decodedAsset.mimeType asset.mimeType
                                ]
                                ()
                        Err _ ->
                            Expect.fail "Asset roundtrip encoding/decoding failed"
            
            , fuzz (list string) "makeAssetIdsBody handles various asset ID lists" <|
                \assetIds ->
                    let
                        result = makeAssetIdsBody assetIds
                        decoded = Decode.decodeValue 
                            (Decode.field "ids" (Decode.list Decode.string)) 
                            result
                    in
                    case decoded of
                        Ok decodedIds ->
                            Expect.equal decodedIds assetIds
                        Err _ ->
                            Expect.fail "Asset IDs body construction failed"
            ]
        ]