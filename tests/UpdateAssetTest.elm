module UpdateAssetTest exposing (..)

import Array exposing (Array)
import Date
import Dict exposing (Dict)
import Expect
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichAsset, ImmichAssetId)
import Test exposing (..)
import TestGenerators exposing (createTestAlbum, createTestAsset, createTestAssetWithActions)
import UpdateAsset exposing (..)
import ViewAlbums exposing (AlbumSearch, AssetWithActions, InputMode(..), PropertyChange(..))



-- Helper functions to create test data


createTestAlbumSearch : String -> String -> AlbumSearch
createTestAlbumSearch searchString partialKeybinding =
    { searchString = searchString
    , partialKeybinding = partialKeybinding
    , selectedIndex = 0
    , albumScores = Dict.empty
    , pagination = { currentPage = 1, itemsPerPage = 20, totalItems = 0 }
    , invalidInputWarning = Nothing
    , inputFocused = False
    , cachedFilteredCount = 0
    }



-- Test data


testAsset : AssetWithActions
testAsset =
    createTestAssetWithActions "test-id" "Test Asset"


testAlbumKeybindings : Dict ImmichAlbumId String
testAlbumKeybindings =
    Dict.fromList [ ( "album1", "ge" ), ( "album2", "tr" ) ]


testKnownAlbums : Dict ImmichAlbumId ImmichAlbum
testKnownAlbums =
    Dict.fromList
        [ ( "album1", createTestAlbum "album1" "General" )
        , ( "album2", createTestAlbum "album2" "Travel" )
        ]


testCurrentAssets : Array ImmichAssetId
testCurrentAssets =
    Array.fromList [ "asset1", "asset2", "asset3" ]


testKnownAssets : Dict ImmichAssetId ImmichAsset
testKnownAssets =
    Dict.fromList
        [ ( "asset1", createTestAsset "asset1" "Asset 1" )
        , ( "asset2", createTestAsset "asset2" "Asset 2" )
        , ( "asset3", createTestAsset "asset3" "Asset 3" )
        ]



-- Test suite


suite : Test
suite =
    describe "UpdateAsset InputMode Transition Tests"
        [ insertModeTests
        , keybindingModeTests
        , modeTransitionTests
        , edgeCaseTests
        , regressionTests
        ]


insertModeTests : Test
insertModeTests =
    describe "InsertMode Behavior Tests"
        [ test "typing multiple letters in InsertMode updates searchString only" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "" ""

                    initialState =
                        EditAsset InsertMode testAsset initialSearch

                    -- Type "g"
                    result1 =
                        updateAsset (AssetKeyPress "g") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0

                    -- Type "e"
                    result2 =
                        case result1 of
                            StayInAssets (EditAsset mode asset search) ->
                                updateAsset (AssetKeyPress "e") (EditAsset mode asset search) testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0

                            _ ->
                                result1
                in
                case result2 of
                    StayInAssets (EditAsset mode asset search) ->
                        Expect.all
                            [ \_ -> Expect.equal InsertMode mode
                            , \_ -> Expect.equal "ge" search.searchString
                            , \_ -> Expect.equal "" search.partialKeybinding
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected to stay in InsertMode EditAsset state"
        , test "partialKeybinding remains empty during InsertMode" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "" ""

                    initialState =
                        EditAsset InsertMode testAsset initialSearch

                    result =
                        updateAsset (AssetKeyPress "g") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0
                in
                case result of
                    StayInAssets (EditAsset _ _ search) ->
                        Expect.equal "" search.partialKeybinding

                    _ ->
                        Expect.fail "Expected to stay in EditAsset state"
        , test "mode stays as InsertMode throughout typing" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "" ""

                    initialState =
                        EditAsset InsertMode testAsset initialSearch

                    -- Type multiple letters
                    result1 =
                        updateAsset (AssetKeyPress "g") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0

                    result2 =
                        case result1 of
                            StayInAssets state ->
                                updateAsset (AssetKeyPress "e") state testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0

                            _ ->
                                result1

                    result3 =
                        case result2 of
                            StayInAssets state ->
                                updateAsset (AssetKeyPress "n") state testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0

                            _ ->
                                result2
                in
                case result3 of
                    StayInAssets (EditAsset mode _ _) ->
                        Expect.equal InsertMode mode

                    _ ->
                        Expect.fail "Expected to stay in InsertMode"
        , test "Escape transitions back to NormalMode" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "gen" ""

                    initialState =
                        EditAsset InsertMode testAsset initialSearch

                    result =
                        updateAsset (AssetKeyPress "Escape") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0
                in
                case result of
                    StayInAssets (EditAsset mode _ _) ->
                        Expect.equal NormalMode mode

                    _ ->
                        Expect.fail "Expected to stay in NormalMode EditAsset state"
        ]


keybindingModeTests : Test
keybindingModeTests =
    describe "KeybindingMode Behavior Tests"
        [ test "typing letters in KeybindingMode updates partialKeybinding only" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "" "g"

                    initialState =
                        EditAsset KeybindingMode testAsset initialSearch

                    result =
                        updateAsset (AssetKeyPress "e") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0
                in
                case result of
                    StayInAssets (EditAsset _ _ search) ->
                        Expect.all
                            [ \_ -> Expect.equal "ge" search.partialKeybinding
                            , \_ -> Expect.equal "" search.searchString
                            ]
                            ()

                    AssetToggleAlbumMembership _ ->
                        -- This is also valid - if "ge" matches an album
                        Expect.pass

                    _ ->
                        Expect.fail "Expected to stay in KeybindingMode or toggle album"
        , test "searchString remains unchanged during KeybindingMode" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "existing" "g"

                    initialState =
                        EditAsset KeybindingMode testAsset initialSearch

                    result =
                        updateAsset (AssetKeyPress "x") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0
                in
                case result of
                    StayInAssets (EditAsset _ _ search) ->
                        Expect.equal "existing" search.searchString

                    _ ->
                        Expect.fail "Expected to stay in EditAsset state"
        , test "Backspace in KeybindingMode can return to NormalMode" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "" "g"

                    initialState =
                        EditAsset KeybindingMode testAsset initialSearch

                    result =
                        updateAsset (AssetKeyPress "Backspace") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0
                in
                case result of
                    StayInAssets (EditAsset mode _ search) ->
                        Expect.all
                            [ \_ -> Expect.equal NormalMode mode
                            , \_ -> Expect.equal "" search.partialKeybinding
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected to return to NormalMode"
        ]


modeTransitionTests : Test
modeTransitionTests =
    describe "Mode Transition Tests"
        [ test "I from NormalMode transitions to InsertMode" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "" ""

                    initialState =
                        EditAsset NormalMode testAsset initialSearch

                    result =
                        updateAsset (AssetKeyPress "I") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0
                in
                case result of
                    StayInAssets (EditAsset mode _ _) ->
                        Expect.equal InsertMode mode

                    _ ->
                        Expect.fail "Expected to transition to InsertMode"
        , test "keybinding letters from NormalMode transition to KeybindingMode" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "" ""

                    initialState =
                        EditAsset NormalMode testAsset initialSearch

                    result =
                        updateAsset (AssetKeyPress "g") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0
                in
                case result of
                    StayInAssets (EditAsset mode _ search) ->
                        Expect.all
                            [ \_ -> Expect.equal KeybindingMode mode
                            , \_ -> Expect.equal "g" search.partialKeybinding
                            ]
                            ()

                    AssetToggleAlbumMembership _ ->
                        -- This is also valid - if "g" matches an album directly
                        Expect.pass

                    _ ->
                        Expect.fail "Expected to transition to KeybindingMode or toggle album"
        , test "Escape from any mode returns to NormalMode" <|
            \_ ->
                let
                    insertResult =
                        updateAsset (AssetKeyPress "Escape") (EditAsset InsertMode testAsset (createTestAlbumSearch "test" "")) testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0

                    keybindingResult =
                        updateAsset (AssetKeyPress "Escape") (EditAsset KeybindingMode testAsset (createTestAlbumSearch "" "g")) testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0
                in
                case ( insertResult, keybindingResult ) of
                    ( StayInAssets (EditAsset mode1 _ _), StayInAssets (EditAsset mode2 _ _) ) ->
                        Expect.all
                            [ \_ -> Expect.equal NormalMode mode1
                            , \_ -> Expect.equal NormalMode mode2
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected both to return to NormalMode"
        ]


edgeCaseTests : Test
edgeCaseTests =
    describe "Edge Case Tests"
        [ test "empty strings in both fields result in NormalMode" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "" ""

                    initialState =
                        EditAsset KeybindingMode testAsset initialSearch

                    -- This should normalize to NormalMode
                    result =
                        updateAsset (AssetKeyPress "Escape") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0
                in
                case result of
                    StayInAssets (EditAsset mode _ _) ->
                        Expect.equal NormalMode mode

                    _ ->
                        Expect.fail "Expected NormalMode with empty strings"
        , test "mode preservation during legitimate UpdateAssetSearch" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "test" ""

                    initialState =
                        EditAsset InsertMode testAsset initialSearch

                    -- Arrow key navigation should preserve InsertMode
                    result =
                        updateAsset (AssetKeyPress "ArrowUp") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0
                in
                case result of
                    StayInAssets (EditAsset mode _ _) ->
                        Expect.equal InsertMode mode

                    _ ->
                        Expect.fail "Expected to preserve InsertMode during navigation"
        ]


regressionTests : Test
regressionTests =
    describe "Regression Tests"
        [ test "first letter in InsertMode works correctly" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "" ""

                    initialState =
                        EditAsset InsertMode testAsset initialSearch

                    result =
                        updateAsset (AssetKeyPress "g") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0
                in
                case result of
                    StayInAssets (EditAsset mode _ search) ->
                        Expect.all
                            [ \_ -> Expect.equal InsertMode mode
                            , \_ -> Expect.equal "g" search.searchString
                            , \_ -> Expect.equal "" search.partialKeybinding
                            ]
                            ()

                    _ ->
                        Expect.fail "First letter should work in InsertMode"
        , test "second letter in InsertMode should continue search string" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "g" ""

                    initialState =
                        EditAsset InsertMode testAsset initialSearch

                    result =
                        updateAsset (AssetKeyPress "e") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0
                in
                case result of
                    StayInAssets (EditAsset mode _ search) ->
                        Expect.all
                            [ \_ -> Expect.equal InsertMode mode
                            , \_ -> Expect.equal "ge" search.searchString
                            , \_ -> Expect.equal "" search.partialKeybinding
                            ]
                            ()

                    _ ->
                        Expect.fail "Second letter should continue search string in InsertMode"
        , test "keybinding functionality still works in NormalMode" <|
            \_ ->
                let
                    initialSearch =
                        createTestAlbumSearch "" ""

                    initialState =
                        EditAsset NormalMode testAsset initialSearch

                    -- Type "g" then "e" for "ge" keybinding
                    result1 =
                        updateAsset (AssetKeyPress "g") initialState testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0

                    result2 =
                        case result1 of
                            StayInAssets state ->
                                updateAsset (AssetKeyPress "e") state testAlbumKeybindings testKnownAlbums 800 testCurrentAssets testKnownAssets 0

                            _ ->
                                result1
                in
                case result2 of
                    AssetToggleAlbumMembership album ->
                        Expect.equal "album1" album.id

                    _ ->
                        Expect.fail "Keybinding 'ge' should trigger album toggle"
        ]
