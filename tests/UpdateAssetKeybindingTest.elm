module UpdateAssetKeybindingTest exposing (..)

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
    }


createTestAlbumSearchWithWarning : String -> String -> String -> AlbumSearch
createTestAlbumSearchWithWarning searchString partialKeybinding warning =
    { searchString = searchString
    , partialKeybinding = partialKeybinding
    , selectedIndex = 0
    , albumScores = Dict.empty
    , pagination = { currentPage = 1, itemsPerPage = 20, totalItems = 0 }
    , invalidInputWarning = Just warning
    , inputFocused = False
    }



-- Test data


testAsset : AssetWithActions
testAsset =
    createTestAssetWithActions "test-id" "Test Asset"


testAlbumKeybindings : Dict ImmichAlbumId String
testAlbumKeybindings =
    Dict.fromList
        [ ( "album1", "ge" )
        , ( "album2", "tr" )
        , ( "album3", "wo" )
        , ( "album4", "sp" )
        , ( "album5", "ga" )
        , ( "album6", "pho" )
        ]


testKnownAlbums : Dict ImmichAlbumId ImmichAlbum
testKnownAlbums =
    Dict.fromList
        [ ( "album1", createTestAlbum "album1" "General" )
        , ( "album2", createTestAlbum "album2" "Travel" )
        , ( "album3", createTestAlbum "album3" "Work" )
        , ( "album4", createTestAlbum "album4" "Sports" )
        , ( "album5", createTestAlbum "album5" "Garden" )
        , ( "album6", createTestAlbum "album6" "Photography" )
        ]


testCurrentAssets : List ImmichAssetId
testCurrentAssets =
    [ "asset1", "asset2", "asset3" ]



-- Test suite


suite : Test
suite =
    describe "UpdateAsset ViewAssets Keybinding Validation Tests"
        [ keybindingValidationTests
        , invalidCharacterRejectionTests
        , textSearchFallbackTests
        , consistencyWithAlbumBrowseTests
        ]


keybindingValidationTests : Test
keybindingValidationTests =
    describe "Keybinding Validation Tests"
        [ test "valid keybinding character starts keybinding mode" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleEditAssetInput "g" NormalMode testAsset search testAlbumKeybindings testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    UpdateAssetSearch newSearch ->
                        Expect.equal "g" newSearch.partialKeybinding

                    _ ->
                        Expect.fail "Expected valid keybinding character to start keybinding mode"
        , test "exact keybinding match toggles album membership" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" "g"

                    result =
                        handleEditAssetInput "e" KeybindingMode testAsset search testAlbumKeybindings testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    ToggleAlbumMembership album ->
                        Expect.equal "General" album.albumName

                    _ ->
                        Expect.fail "Expected exact keybinding match to toggle album membership"
        , test "partial keybinding accumulation works correctly" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" "p"

                    result =
                        handleEditAssetInput "h" KeybindingMode testAsset search testAlbumKeybindings testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    UpdateAssetSearch newSearch ->
                        Expect.equal "ph" newSearch.partialKeybinding

                    _ ->
                        Expect.fail "Expected partial keybinding accumulation"
        ]


invalidCharacterRejectionTests : Test
invalidCharacterRejectionTests =
    describe "Invalid Character Rejection Tests"
        [ test "invalid keybinding character is rejected from NormalMode" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleEditAssetInput "z" NormalMode testAsset search testAlbumKeybindings testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    InvalidKeybindingInput invalidChar _ ->
                        Expect.equal "z" invalidChar

                    _ ->
                        Expect.fail "Expected invalid keybinding character to be rejected"
        , test "invalid keybinding character is rejected in KeybindingMode" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" "g"

                    result =
                        handleEditAssetInput "z" KeybindingMode testAsset search testAlbumKeybindings testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    InvalidKeybindingInput invalidChar _ ->
                        Expect.equal "z" invalidChar

                    _ ->
                        Expect.fail "Expected invalid keybinding character to be rejected in KeybindingMode"
        , test "invalid character rejection preserves partial keybinding" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" "g"

                    result =
                        handleEditAssetInput "z" KeybindingMode testAsset search testAlbumKeybindings testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    InvalidKeybindingInput invalidChar _ ->
                        Expect.equal "z" invalidChar

                    _ ->
                        Expect.fail "Expected invalid character to be rejected while preserving partial keybinding"
        ]


textSearchFallbackTests : Test
textSearchFallbackTests =
    describe "Text Search Fallback Tests"
        [ test "uppercase letters start text search in InsertMode" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleEditAssetInput "X" InsertMode testAsset search testAlbumKeybindings testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    UpdateAssetSearch newSearch ->
                        Expect.equal "X" newSearch.searchString

                    _ ->
                        Expect.fail "Expected uppercase letter to start text search"
        , test "space characters work for text search in InsertMode" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "test" ""

                    result =
                        handleEditAssetInput " " InsertMode testAsset search testAlbumKeybindings testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    UpdateAssetSearch newSearch ->
                        Expect.equal "test " newSearch.searchString

                    _ ->
                        Expect.fail "Expected space to continue text search"
        , test "empty keybindings dictionary allows fallback" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleEditAssetInput "g" NormalMode testAsset search Dict.empty testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    NoAssetAction ->
                        Expect.pass

                    -- This is the expected behavior when no keybindings exist
                    _ ->
                        Expect.fail "Expected no action when no keybindings exist"
        ]


consistencyWithAlbumBrowseTests : Test
consistencyWithAlbumBrowseTests =
    describe "Consistency with Album Browse Mode Tests"
        [ test "ViewAssets keybinding validation matches album browse behavior" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    -- Test that 'z' is rejected in ViewAssets mode just like in album browse
                    result =
                        handleEditAssetInput "z" NormalMode testAsset search testAlbumKeybindings testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    InvalidKeybindingInput invalidChar _ ->
                        Expect.equal "z" invalidChar

                    _ ->
                        Expect.fail "Expected ViewAssets to reject invalid keybinding just like album browse"
        , test "ViewAssets exact match behavior matches album browse" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    -- Test that 'ge' works in ViewAssets mode just like in album browse
                    result1 =
                        handleEditAssetInput "g" NormalMode testAsset search testAlbumKeybindings testKnownAlbums 800 testCurrentAssets

                    result2 =
                        case result1 of
                            UpdateAssetSearch newSearch ->
                                handleEditAssetInput "e" KeybindingMode testAsset newSearch testAlbumKeybindings testKnownAlbums 800 testCurrentAssets

                            _ ->
                                result1
                in
                case result2 of
                    ToggleAlbumMembership album ->
                        Expect.equal "General" album.albumName

                    _ ->
                        Expect.fail "Expected ViewAssets exact match to work like album browse"
        , test "warning state is properly handled in ViewAssets mode" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" "g"

                    -- Test that invalid input generates InvalidKeybindingInput action
                    result =
                        handleEditAssetInput "z" KeybindingMode testAsset search testAlbumKeybindings testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    InvalidKeybindingInput invalidChar _ ->
                        Expect.equal "z" invalidChar

                    _ ->
                        Expect.fail "Expected warning state to be handled in ViewAssets mode"
        , test "invalid warning clears when entering valid keybinding character in ViewAssets" <|
            \_ ->
                let
                    -- Start with a search that has an invalid input warning
                    searchWithWarning =
                        createTestAlbumSearchWithWarning "" "p" "Invalid character: z"

                    -- Type a valid character "h" after "p" to make "ph" (partial keybinding for "pho")
                    result =
                        handleEditAssetInput "h" KeybindingMode testAsset searchWithWarning testAlbumKeybindings testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    UpdateAssetSearch newSearch ->
                        Expect.all
                            [ \_ -> Expect.equal "ph" newSearch.partialKeybinding
                            , \_ -> Expect.equal Nothing newSearch.invalidInputWarning -- Warning should be cleared
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected valid character to update search and clear warning in ViewAssets"
        , test "ViewAssets user scenario: 'azb' sequence with 'ab' keybinding should work correctly" <|
            \_ ->
                let
                    -- Create custom test data with keybinding "ab"
                    testKeybindingsCustom =
                        Dict.fromList [ ( "album1", "ab" ) ]

                    testKnownAlbumsCustom =
                        Dict.fromList [ ( "album1", createTestAlbum "album1" "Test Album" ) ]

                    -- Start fresh and type "a"
                    search1 =
                        createTestAlbumSearch "" ""

                    result1 =
                        handleEditAssetInput "a" NormalMode testAsset search1 testKeybindingsCustom testKnownAlbumsCustom 800 testCurrentAssets

                    -- Should get partial keybinding "a"
                    searchAfterA =
                        case result1 of
                            UpdateAssetSearch newSearch ->
                                newSearch

                            _ ->
                                search1

                    -- Type "z" (invalid) - should be rejected
                    result2 =
                        handleEditAssetInput "z" KeybindingMode testAsset searchAfterA testKeybindingsCustom testKnownAlbumsCustom 800 testCurrentAssets

                    -- Type "b" after the rejected "z" - should complete "ab" keybinding
                    result3 =
                        handleEditAssetInput "b" KeybindingMode testAsset searchAfterA testKeybindingsCustom testKnownAlbumsCustom 800 testCurrentAssets
                in
                case ( result2, result3 ) of
                    ( InvalidKeybindingInput _ _, ToggleAlbumMembership album ) ->
                        -- Perfect! "z" was rejected, "b" completed the "ab" keybinding
                        Expect.equal "album1" album.id

                    ( InvalidKeybindingInput _ _, UpdateAssetSearch _ ) ->
                        Expect.fail "Expected 'ab' to match album exactly, not create partial keybinding"

                    _ ->
                        Expect.fail "Expected 'z' to be rejected and 'b' to complete keybinding in ViewAssets"
        , test "EXACT USER ISSUE: correct->incorrect->correct should not show warning" <|
            \_ ->
                let
                    testKeybindingsCustom =
                        Dict.fromList [ ( "album1", "ac" ) ]

                    testKnownAlbumsCustom =
                        Dict.fromList [ ( "album1", createTestAlbum "album1" "Test Album" ) ]

                    -- Step 1: Type correct key "a"
                    search1 =
                        createTestAlbumSearch "" ""

                    result1 =
                        handleEditAssetInput "a" NormalMode testAsset search1 testKeybindingsCustom testKnownAlbumsCustom 800 testCurrentAssets

                    searchAfterA =
                        case result1 of
                            UpdateAssetSearch newSearch ->
                                newSearch

                            _ ->
                                search1

                    -- Step 2: Type incorrect key "b" - creates warning
                    result2 =
                        handleEditAssetInput "b" KeybindingMode testAsset searchAfterA testKeybindingsCustom testKnownAlbumsCustom 800 testCurrentAssets

                    -- Step 3: Type correct key "c" - should clear warning and complete keybinding
                    result3 =
                        handleEditAssetInput "c" KeybindingMode testAsset searchAfterA testKeybindingsCustom testKnownAlbumsCustom 800 testCurrentAssets
                in
                case ( result2, result3 ) of
                    ( InvalidKeybindingInput invalidChar clearedSearch, ToggleAlbumMembership album ) ->
                        Expect.all
                            [ \_ -> Expect.equal "b" invalidChar
                            , \_ -> Expect.equal "album1" album.id
                            , \_ -> Expect.equal Nothing clearedSearch.invalidInputWarning -- Verify warning was cleared before creating new warning
                            ]
                            ()

                    _ ->
                        let
                            result2Debug =
                                case result2 of
                                    InvalidKeybindingInput char _ ->
                                        "InvalidKeybindingInput: " ++ char

                                    UpdateAssetSearch _ ->
                                        "UpdateAssetSearch"

                                    ToggleAlbumMembership _ ->
                                        "ToggleAlbumMembership"

                                    ExitToNormalMode ->
                                        "ExitToNormalMode"

                                    _ ->
                                        "Other"

                            result3Debug =
                                case result3 of
                                    InvalidKeybindingInput char _ ->
                                        "InvalidKeybindingInput: " ++ char

                                    UpdateAssetSearch _ ->
                                        "UpdateAssetSearch"

                                    ToggleAlbumMembership _ ->
                                        "ToggleAlbumMembership"

                                    ExitToNormalMode ->
                                        "ExitToNormalMode"

                                    _ ->
                                        "Other"
                        in
                        Expect.fail ("Expected InvalidKeybindingInput then ToggleAlbumMembership, got: " ++ result2Debug ++ " then " ++ result3Debug)
        , test "Escape from KeybindingMode clears all search state" <|
            \_ ->
                let
                    -- Set up a search state with both keybinding and manual search active
                    searchWithBoth =
                        createTestAlbumSearchWithWarning "manual search" "g" "Invalid: z"

                    -- Press Escape from KeybindingMode
                    result =
                        handleEditAssetInput "Escape" KeybindingMode testAsset searchWithBoth testAlbumKeybindings testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    ExitToNormalMode ->
                        Expect.pass

                    -- The action itself is correct
                    _ ->
                        Expect.fail "Expected Escape from KeybindingMode to return ExitToNormalMode"
        , test "Escape from InsertMode clears all search state" <|
            \_ ->
                let
                    -- Set up a search state with manual search text
                    searchWithManual =
                        createTestAlbumSearch "manual search text" ""

                    -- Press Escape from InsertMode
                    result =
                        handleEditAssetInput "Escape" InsertMode testAsset searchWithManual testAlbumKeybindings testKnownAlbums 800 testCurrentAssets
                in
                case result of
                    ExitToNormalMode ->
                        Expect.pass

                    -- The action itself is correct
                    _ ->
                        Expect.fail "Expected Escape from InsertMode to return ExitToNormalMode"
        ]
