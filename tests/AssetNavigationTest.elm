module AssetNavigationTest exposing (..)

import Array
import AssetNavigation exposing (AssetSwitchResult(..), buildAssetViewState, findAssetByIndex, preserveVideoLoadedState)
import Date
import Dict
import Expect
import Immich exposing (ImmichAsset)
import Test exposing (Test, describe, test)
import Types exposing (UserMode(..))
import UpdateAsset exposing (AssetState(..))
import UpdateMenus exposing (MenuState(..))
import ViewAlbums exposing (AssetWithActions, InputMode(..), PropertyChange(..))


{-| Create a test asset with given ID
-}
createAsset : String -> ImmichAsset
createAsset id =
    let
        dummyDate =
            Date.fromRataDie 0
    in
    { id = id
    , path = "/path/to/asset/" ++ id
    , title = "Test Asset " ++ id
    , mimeType = "image/jpeg"
    , fileCreatedAt = dummyDate
    , fileModifiedAt = dummyDate
    , fileCreatedAtString = "2024-01-01T00:00:00.000Z"
    , fileModifiedAtString = "2024-01-01T00:00:00.000Z"
    , isFavourite = False
    , isArchived = False
    , albumMembership = []
    , duration = Nothing
    , thumbhash = Nothing
    }


{-| Create a test AssetWithActions
-}
createAssetWithActions : String -> Bool -> AssetWithActions
createAssetWithActions id isVideoLoaded =
    { asset = createAsset id
    , isFavourite = RemainFalse
    , isArchived = RemainFalse
    , albumMembership = Dict.empty
    , isVideoLoaded = isVideoLoaded
    }


suite : Test
suite =
    describe "AssetNavigation Pure Functions"
        [ describe "findAssetByIndex"
            [ test "returns Just asset when index is valid and asset exists in knownAssets" <|
                \_ ->
                    let
                        asset1 =
                            createAsset "asset-1"

                        asset2 =
                            createAsset "asset-2"

                        asset3 =
                            createAsset "asset-3"

                        currentAssets =
                            Array.fromList [ "asset-1", "asset-2", "asset-3" ]

                        knownAssets =
                            Dict.fromList
                                [ ( "asset-1", asset1 )
                                , ( "asset-2", asset2 )
                                , ( "asset-3", asset3 )
                                ]

                        result =
                            findAssetByIndex currentAssets 1 knownAssets
                    in
                    case result of
                        Just asset ->
                            Expect.equal "asset-2" asset.id

                        Nothing ->
                            Expect.fail "Expected Just asset"
            , test "returns Nothing when index is out of bounds" <|
                \_ ->
                    let
                        asset1 =
                            createAsset "asset-1"

                        currentAssets =
                            Array.fromList [ "asset-1" ]

                        knownAssets =
                            Dict.fromList [ ( "asset-1", asset1 ) ]

                        result =
                            findAssetByIndex currentAssets 5 knownAssets
                    in
                    Expect.equal Nothing result
            , test "returns Nothing when asset ID not in knownAssets" <|
                \_ ->
                    let
                        currentAssets =
                            Array.fromList [ "asset-1", "asset-2" ]

                        knownAssets =
                            Dict.empty

                        result =
                            findAssetByIndex currentAssets 0 knownAssets
                    in
                    Expect.equal Nothing result
            , test "returns first asset when index is 0" <|
                \_ ->
                    let
                        asset1 =
                            createAsset "first"

                        asset2 =
                            createAsset "second"

                        currentAssets =
                            Array.fromList [ "first", "second" ]

                        knownAssets =
                            Dict.fromList
                                [ ( "first", asset1 )
                                , ( "second", asset2 )
                                ]

                        result =
                            findAssetByIndex currentAssets 0 knownAssets
                    in
                    case result of
                        Just asset ->
                            Expect.equal "first" asset.id

                        Nothing ->
                            Expect.fail "Expected Just asset"
            , test "returns Nothing for empty currentAssets list" <|
                \_ ->
                    let
                        asset1 =
                            createAsset "asset-1"

                        knownAssets =
                            Dict.fromList [ ( "asset-1", asset1 ) ]

                        result =
                            findAssetByIndex Array.empty 0 knownAssets
                    in
                    Expect.equal Nothing result
            , test "returns Nothing for negative index (via List.drop behavior)" <|
                \_ ->
                    let
                        asset1 =
                            createAsset "asset-1"

                        currentAssets =
                            Array.fromList [ "asset-1" ]

                        knownAssets =
                            Dict.fromList [ ( "asset-1", asset1 ) ]

                        -- Array.get with negative index returns Nothing
                        result =
                            findAssetByIndex currentAssets -1 knownAssets
                    in
                    -- Array.get -1 returns Nothing (unlike List.drop -1 which returns the whole list)
                    Expect.equal Nothing result
            ]
        , describe "buildAssetViewState"
            [ test "returns AssetFound when asset exists" <|
                \_ ->
                    let
                        asset1 =
                            createAsset "found-asset"

                        currentAssets =
                            Array.fromList [ "found-asset" ]

                        knownAssets =
                            Dict.fromList [ ( "found-asset", asset1 ) ]

                        knownAlbums =
                            Dict.empty

                        result =
                            buildAssetViewState currentAssets 0 knownAssets knownAlbums 800
                    in
                    case result of
                        AssetFound data ->
                            Expect.all
                                [ \d -> Expect.equal "found-asset" d.asset.id
                                , \d -> Expect.equal 0 d.newIndex
                                , \d -> Expect.equal "found-asset" d.assetWithActions.asset.id
                                ]
                                data

                        AssetNotFound ->
                            Expect.fail "Expected AssetFound"
            , test "returns AssetNotFound when asset doesn't exist" <|
                \_ ->
                    let
                        currentAssets =
                            Array.fromList [ "missing-asset" ]

                        knownAssets =
                            Dict.empty

                        knownAlbums =
                            Dict.empty

                        result =
                            buildAssetViewState currentAssets 0 knownAssets knownAlbums 800
                    in
                    Expect.equal AssetNotFound result
            , test "returns AssetNotFound when index is out of bounds" <|
                \_ ->
                    let
                        asset1 =
                            createAsset "asset-1"

                        currentAssets =
                            Array.fromList [ "asset-1" ]

                        knownAssets =
                            Dict.fromList [ ( "asset-1", asset1 ) ]

                        knownAlbums =
                            Dict.empty

                        result =
                            buildAssetViewState currentAssets 10 knownAssets knownAlbums 800
                    in
                    Expect.equal AssetNotFound result
            , test "preserves index in result" <|
                \_ ->
                    let
                        asset1 =
                            createAsset "a1"

                        asset2 =
                            createAsset "a2"

                        asset3 =
                            createAsset "a3"

                        currentAssets =
                            Array.fromList [ "a1", "a2", "a3" ]

                        knownAssets =
                            Dict.fromList
                                [ ( "a1", asset1 )
                                , ( "a2", asset2 )
                                , ( "a3", asset3 )
                                ]

                        result =
                            buildAssetViewState currentAssets 2 knownAssets Dict.empty 800
                    in
                    case result of
                        AssetFound data ->
                            Expect.all
                                [ \d -> Expect.equal "a3" d.asset.id
                                , \d -> Expect.equal 2 d.newIndex
                                ]
                                data

                        AssetNotFound ->
                            Expect.fail "Expected AssetFound"
            ]
        , describe "preserveVideoLoadedState"
            [ test "preserves video loaded state when viewing same asset" <|
                \_ ->
                    let
                        asset =
                            createAsset "same-asset"

                        currentAssetWithActions =
                            createAssetWithActions "same-asset" True

                        -- isVideoLoaded = True
                        newAssetWithActions =
                            createAssetWithActions "same-asset" False

                        -- isVideoLoaded = False
                        dummyAlbumSearch =
                            { searchString = ""
                            , albumScores = Dict.empty
                            , selectedIndex = 0
                            , partialKeybinding = ""
                            , pagination = { currentPage = 0, itemsPerPage = 10, totalItems = 0 }
                            , invalidInputWarning = Nothing
                            , inputFocused = False
                            }

                        currentUserMode =
                            ViewAssets (EditAsset NormalMode currentAssetWithActions dummyAlbumSearch)

                        result =
                            preserveVideoLoadedState currentUserMode asset newAssetWithActions
                    in
                    Expect.equal True result.isVideoLoaded
            , test "does not preserve video loaded state when viewing different asset" <|
                \_ ->
                    let
                        newAsset =
                            createAsset "new-asset"

                        currentAssetWithActions =
                            createAssetWithActions "old-asset" True

                        -- isVideoLoaded = True
                        newAssetWithActions =
                            createAssetWithActions "new-asset" False

                        -- isVideoLoaded = False
                        dummyAlbumSearch =
                            { searchString = ""
                            , albumScores = Dict.empty
                            , selectedIndex = 0
                            , partialKeybinding = ""
                            , pagination = { currentPage = 0, itemsPerPage = 10, totalItems = 0 }
                            , invalidInputWarning = Nothing
                            , inputFocused = False
                            }

                        currentUserMode =
                            ViewAssets (EditAsset NormalMode currentAssetWithActions dummyAlbumSearch)

                        result =
                            preserveVideoLoadedState currentUserMode newAsset newAssetWithActions
                    in
                    Expect.equal False result.isVideoLoaded
            , test "returns base asset when not in ViewAssets mode" <|
                \_ ->
                    let
                        asset =
                            createAsset "test-asset"

                        baseAssetWithActions =
                            createAssetWithActions "test-asset" False

                        -- MainMenu mode, not ViewAssets
                        currentUserMode =
                            MainMenu MainMenuHome

                        result =
                            preserveVideoLoadedState currentUserMode asset baseAssetWithActions
                    in
                    Expect.equal False result.isVideoLoaded
            ]
        ]
