module ProcessImmichMsgTest exposing (..)

import Date
import Dict
import Expect
import Immich exposing (ImmichAlbum, ImmichAsset)
import ProcessImmichMsg exposing (processAlbumChangeSuccess, processAlbumCreatedSuccess)
import Test exposing (Test, describe, test)
import Types exposing (UserMode(..))
import UpdateAsset exposing (AssetState(..))
import UpdateMenus exposing (MenuState(..))
import ViewAlbums exposing (AssetWithActions, InputMode(..), PropertyChange(..))


{-| Create a test ImmichAlbum
-}
createAlbum : String -> String -> ImmichAlbum
createAlbum id name =
    { id = id
    , albumName = name
    , assetCount = 0
    , assets = []
    , createdAt = Date.fromRataDie 0
    }


{-| Create a minimal test ImmichAsset
-}
createAsset : String -> ImmichAsset
createAsset id =
    { id = id
    , path = "/path/test.jpg"
    , title = "test.jpg"
    , mimeType = "image/jpeg"
    , isFavourite = False
    , isArchived = False
    , albumMembership = []
    , fileCreatedAt = Date.fromRataDie 0
    , fileModifiedAt = Date.fromRataDie 0
    , fileCreatedAtString = "2024-01-01T00:00:00Z"
    , fileModifiedAtString = "2024-01-01T00:00:00Z"
    , thumbhash = Nothing
    , duration = Nothing
    }


{-| Create a test AssetWithActions
-}
createAssetWithActions : String -> AssetWithActions
createAssetWithActions id =
    { asset = createAsset id
    , isFavourite = RemainFalse
    , isArchived = RemainFalse
    , albumMembership = Dict.empty
    , isVideoLoaded = False
    }


{-| Create an EditAsset user mode
-}
createEditAssetMode : String -> UserMode
createEditAssetMode assetId =
    let
        asset =
            createAssetWithActions assetId

        search =
            { searchString = ""
            , albumScores = Dict.empty
            , selectedIndex = 0
            , partialKeybinding = ""
            , pagination = { itemsPerPage = 10, currentPage = 0, totalItems = 0 }
            , invalidInputWarning = Nothing
            , inputFocused = False
            }
    in
    ViewAssets (EditAsset NormalMode asset search)


suite : Test
suite =
    describe "ProcessImmichMsg Pure Functions"
        [ describe "processAlbumChangeSuccess"
            [ test "pops first pending change and returns count update" <|
                \_ ->
                    let
                        pendingChanges =
                            [ ( "album-1", True ), ( "album-2", False ) ]

                        result =
                            processAlbumChangeSuccess pendingChanges (MainMenu MainMenuHome)
                    in
                    Expect.all
                        [ \r -> Expect.equal [ ( "album-2", False ) ] r.updatedPendingChanges
                        , \r -> Expect.equal (Just ( "album-1", 1 )) r.albumCountUpdate
                        , \r -> Expect.equal False r.shouldFetchMembership
                        , \r -> Expect.equal Nothing r.assetIdForMembership
                        ]
                        result
            , test "returns +1 count for addition" <|
                \_ ->
                    let
                        pendingChanges =
                            [ ( "album-1", True ) ]

                        result =
                            processAlbumChangeSuccess pendingChanges (MainMenu MainMenuHome)
                    in
                    Expect.equal (Just ( "album-1", 1 )) result.albumCountUpdate
            , test "returns -1 count for removal" <|
                \_ ->
                    let
                        pendingChanges =
                            [ ( "album-1", False ) ]

                        result =
                            processAlbumChangeSuccess pendingChanges (MainMenu MainMenuHome)
                    in
                    Expect.equal (Just ( "album-1", -1 )) result.albumCountUpdate
            , test "triggers membership fetch when queue becomes empty and in EditAsset" <|
                \_ ->
                    let
                        pendingChanges =
                            [ ( "album-1", True ) ]

                        userMode =
                            createEditAssetMode "asset-123"

                        result =
                            processAlbumChangeSuccess pendingChanges userMode
                    in
                    Expect.all
                        [ \r -> Expect.equal [] r.updatedPendingChanges
                        , \r -> Expect.equal True r.shouldFetchMembership
                        , \r -> Expect.equal (Just "asset-123") r.assetIdForMembership
                        ]
                        result
            , test "does NOT trigger membership fetch when changes remain" <|
                \_ ->
                    let
                        pendingChanges =
                            [ ( "album-1", True ), ( "album-2", True ) ]

                        userMode =
                            createEditAssetMode "asset-123"

                        result =
                            processAlbumChangeSuccess pendingChanges userMode
                    in
                    Expect.all
                        [ \r -> Expect.equal [ ( "album-2", True ) ] r.updatedPendingChanges
                        , \r -> Expect.equal False r.shouldFetchMembership
                        , \r -> Expect.equal Nothing r.assetIdForMembership
                        ]
                        result
            , test "does NOT trigger membership fetch when not in EditAsset" <|
                \_ ->
                    let
                        pendingChanges =
                            [ ( "album-1", True ) ]

                        userMode =
                            MainMenu MainMenuHome

                        result =
                            processAlbumChangeSuccess pendingChanges userMode
                    in
                    Expect.all
                        [ \r -> Expect.equal False r.shouldFetchMembership
                        , \r -> Expect.equal Nothing r.assetIdForMembership
                        ]
                        result
            , test "handles empty pending changes gracefully" <|
                \_ ->
                    let
                        result =
                            processAlbumChangeSuccess [] (MainMenu MainMenuHome)
                    in
                    Expect.all
                        [ \r -> Expect.equal [] r.updatedPendingChanges
                        , \r -> Expect.equal Nothing r.albumCountUpdate
                        , \r -> Expect.equal False r.shouldFetchMembership
                        ]
                        result
            ]
        , describe "processAlbumCreatedSuccess"
            [ test "toggles album membership on asset" <|
                \_ ->
                    let
                        album =
                            createAlbum "new-album" "New Album"

                        asset =
                            createAssetWithActions "asset-1"

                        knownAlbums =
                            Dict.singleton "new-album" album

                        result =
                            processAlbumCreatedSuccess album asset knownAlbums
                    in
                    -- After toggle, asset should have ChangeToTrue for the new album
                    Expect.equal (Just ChangeToTrue) (Dict.get "new-album" result.updatedAsset.albumMembership)
            , test "returns correct pending change tuple" <|
                \_ ->
                    let
                        album =
                            createAlbum "new-album" "New Album"

                        asset =
                            createAssetWithActions "asset-1"

                        result =
                            processAlbumCreatedSuccess album asset Dict.empty
                    in
                    Expect.equal ( "new-album", True ) result.pendingChange
            , test "creates fresh album search" <|
                \_ ->
                    let
                        album =
                            createAlbum "new-album" "New Album"

                        asset =
                            createAssetWithActions "asset-1"

                        knownAlbums =
                            Dict.fromList
                                [ ( "album-1", createAlbum "album-1" "Album A" )
                                , ( "album-2", createAlbum "album-2" "Album B" )
                                ]

                        result =
                            processAlbumCreatedSuccess album asset knownAlbums
                    in
                    -- Search string should be empty
                    Expect.equal "" result.newSearch.searchString
            ]
        ]
