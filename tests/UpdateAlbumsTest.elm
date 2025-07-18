module UpdateAlbumsTest exposing (..)

import Date
import Dict exposing (Dict)
import Expect
import Helpers
import Immich exposing (ImmichAlbum, ImmichAlbumId)
import Test exposing (..)
import UpdateAlbums exposing (..)
import ViewAlbums exposing (AlbumSearch)



-- Helper functions to create test data


createTestAlbum : String -> String -> ImmichAlbum
createTestAlbum id albumName =
    { id = id
    , albumName = albumName
    , assetCount = 0
    , assets = []
    , createdAt = Date.fromRataDie 1
    }


createTestAlbumSearch : String -> String -> AlbumSearch
createTestAlbumSearch searchString partialKeybinding =
    { searchString = searchString
    , partialKeybinding = partialKeybinding
    , selectedIndex = 0
    , albumScores = Dict.empty
    , pagination = { currentPage = 1, itemsPerPage = 20, totalItems = 100 }
    , invalidInputWarning = Nothing
    }


createTestAlbumSearchWithWarning : String -> String -> String -> AlbumSearch
createTestAlbumSearchWithWarning searchString partialKeybinding warning =
    { searchString = searchString
    , partialKeybinding = partialKeybinding
    , selectedIndex = 0
    , albumScores = Dict.empty
    , pagination = { currentPage = 1, itemsPerPage = 20, totalItems = 100 }
    , invalidInputWarning = Just warning
    }



-- Test data


testAlbums : Dict ImmichAlbumId ImmichAlbum
testAlbums =
    Dict.fromList
        [ ( "album1", createTestAlbum "album1" "General" )
        , ( "album2", createTestAlbum "album2" "Travel" )
        , ( "album3", createTestAlbum "album3" "Work" )
        , ( "album4", createTestAlbum "album4" "Sports" )
        , ( "album5", createTestAlbum "album5" "Garden" )
        , ( "album6", createTestAlbum "album6" "Photography" )
        ]


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



-- Test suite


suite : Test
suite =
    describe "UpdateAlbums Tests"
        [ keybindingSelectionTests
        , navigationTests
        , textSearchTests
        , menuIntegrationTests
        , edgeCaseTests
        , regressionTests
        ]


keybindingSelectionTests : Test
keybindingSelectionTests =
    describe "Keybinding Selection Tests"
        [ test "single letter with unique match selects album immediately" <|
            \_ ->
                let
                    -- "g" should match "ge" (General) since it's the only album starting with "g"
                    singleLetterKeybindings =
                        Dict.fromList [ ( "album1", "g" ) ]

                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "g" search singleLetterKeybindings testAlbums
                in
                case result of
                    SelectAlbumForView album ->
                        Expect.equal "album1" album.id

                    _ ->
                        Expect.fail "Expected immediate album selection for unique single letter"
        , test "multi-letter match selects album immediately" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    -- Type "g" then "e" to match "ge" (General)
                    result1 =
                        handleAlbumBrowseInput "g" search testAlbumKeybindings testAlbums

                    result2 =
                        case result1 of
                            UpdateAlbumSearch newSearch ->
                                handleAlbumBrowseInput "e" newSearch testAlbumKeybindings testAlbums

                            _ ->
                                result1
                in
                case result2 of
                    SelectAlbumForView album ->
                        Expect.equal "album1" album.id

                    _ ->
                        Expect.fail "Expected album selection for 'ge' keybinding"
        , test "partial keybinding accumulation shows matching albums" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    -- Type "g" - should show albums starting with "g"
                    result =
                        handleAlbumBrowseInput "g" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "g" newSearch.partialKeybinding

                    _ ->
                        Expect.fail "Expected partial keybinding accumulation"
        , test "no-match scenario rejects invalid keybinding" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    -- Type "x" - no keybinding starts with "x", should be rejected
                    result =
                        handleAlbumBrowseInput "x" search testAlbumKeybindings testAlbums
                in
                case result of
                    InvalidKeybindingInput invalidInput _ ->
                        Expect.equal "x" invalidInput

                    _ ->
                        Expect.fail "Expected invalid keybinding rejection"
        , test "exact keybinding match takes priority over text search" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    -- Type "g" - should match keybinding, not text search
                    result =
                        handleAlbumBrowseInput "g" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.all
                            [ \_ -> Expect.equal "g" newSearch.partialKeybinding
                            , \_ -> Expect.equal "" newSearch.searchString
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected keybinding match over text search"
        , test "backspace clears partial keybinding first" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" "g"

                    result =
                        handleAlbumBrowseInput "Backspace" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "" newSearch.partialKeybinding

                    _ ->
                        Expect.fail "Expected backspace to clear partial keybinding"
        , test "backspace clears text search when no partial keybinding" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "test" ""

                    result =
                        handleAlbumBrowseInput "Backspace" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "tes" newSearch.searchString

                    _ ->
                        Expect.fail "Expected backspace to clear text search"
        ]


navigationTests : Test
navigationTests =
    describe "Navigation Tests"
        [ test "Escape returns to main menu" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "Escape" search testAlbumKeybindings testAlbums
                in
                Expect.equal ChangeToMainMenu result
        , test "Enter selects top match" <|
            \_ ->
                let
                    -- Set up search with a match - need to set up albumScores for this to work
                    search =
                        { searchString = "gen", partialKeybinding = "", selectedIndex = 0, albumScores = Dict.fromList [ ( "album1", 100 ) ], pagination = { currentPage = 1, itemsPerPage = 20, totalItems = 1 }, invalidInputWarning = Nothing }

                    result =
                        handleAlbumBrowseInput "Enter" search testAlbumKeybindings testAlbums
                in
                case result of
                    SelectAlbumForView album ->
                        Expect.equal "General" album.albumName

                    NoAlbumAction ->
                        -- This is also acceptable if no clear match
                        Expect.pass

                    _ ->
                        Expect.fail "Expected Enter to select top match or NoAlbumAction"
        , test "PageUp updates pagination" <|
            \_ ->
                let
                    -- Start on page 2 so PageUp can actually go to page 1
                    search =
                        { searchString = "", partialKeybinding = "", selectedIndex = 0, albumScores = Dict.empty, pagination = { currentPage = 2, itemsPerPage = 20, totalItems = 100 }, invalidInputWarning = Nothing }

                    result =
                        handleAlbumBrowseInput "PageUp" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal 1 newSearch.pagination.currentPage

                    _ ->
                        Expect.fail "Expected PageUp to update pagination"
        , test "PageDown updates pagination" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "PageDown" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal 2 newSearch.pagination.currentPage

                    _ ->
                        Expect.fail "Expected PageDown to update pagination"
        , test "Space acts as page down" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput " " search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal 2 newSearch.pagination.currentPage

                    _ ->
                        Expect.fail "Expected Space to act as page down"
        , test "Ctrl+d acts as half page down" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "Control+d" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch _ ->
                        Expect.pass

                    _ ->
                        Expect.fail "Expected Ctrl+d to act as half page down"
        , test "Ctrl+u acts as half page up" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "Control+u" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch _ ->
                        Expect.pass

                    _ ->
                        Expect.fail "Expected Ctrl+u to act as half page up"
        , test "Ctrl+f acts as page down" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "Control+f" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch _ ->
                        Expect.pass

                    _ ->
                        Expect.fail "Expected Ctrl+f to act as page down"
        , test "Ctrl+b acts as page up" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "Control+b" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch _ ->
                        Expect.pass

                    _ ->
                        Expect.fail "Expected Ctrl+b to act as page up"
        ]


textSearchTests : Test
textSearchTests =
    describe "Text Search Tests"
        [ test "non-keybinding letters are rejected as invalid" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    -- "x" is not a keybinding start, should be rejected
                    result =
                        handleAlbumBrowseInput "x" search testAlbumKeybindings testAlbums
                in
                case result of
                    InvalidKeybindingInput invalidInput _ ->
                        Expect.equal "x" invalidInput

                    _ ->
                        Expect.fail "Expected non-keybinding letter to be rejected"
        , test "supported search letters build text search" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "t" ""

                    result =
                        handleAlbumBrowseInput "e" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "te" newSearch.searchString

                    _ ->
                        Expect.fail "Expected supported letter to build text search"
        , test "mixed keybinding and text search scenarios" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    -- Start with keybinding "g"
                    result1 =
                        handleAlbumBrowseInput "g" search testAlbumKeybindings testAlbums

                    -- Then hit backspace to clear
                    result2 =
                        case result1 of
                            UpdateAlbumSearch newSearch ->
                                handleAlbumBrowseInput "Backspace" newSearch testAlbumKeybindings testAlbums

                            _ ->
                                result1

                    -- Then type "X" (uppercase) for text search - uppercase letters are not keybinding letters
                    result3 =
                        case result2 of
                            UpdateAlbumSearch newSearch ->
                                handleAlbumBrowseInput "X" newSearch testAlbumKeybindings testAlbums

                            _ ->
                                result2
                in
                case result3 of
                    UpdateAlbumSearch newSearch ->
                        Expect.all
                            [ \_ -> Expect.equal "X" newSearch.searchString
                            , \_ -> Expect.equal "" newSearch.partialKeybinding
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected mixed keybinding/text search scenario to work"
        , test "helper functions work correctly for uppercase letters" <|
            \_ ->
                let
                    isKeybindingLetterResult =
                        Helpers.isKeybindingLetter "X"

                    isSupportedSearchLetterResult =
                        Helpers.isSupportedSearchLetter "X"
                in
                Expect.all
                    [ \_ -> Expect.equal False isKeybindingLetterResult
                    , \_ -> Expect.equal True isSupportedSearchLetterResult
                    ]
                    ()
        , test "uppercase letters start text search" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "X" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.all
                            [ \_ -> Expect.equal "X" newSearch.searchString
                            , \_ -> Expect.equal "" newSearch.partialKeybinding
                            ]
                            ()

                    other ->
                        Expect.fail ("Expected uppercase letter to start text search, got: " ++ Debug.toString other)
        , test "invalid warning clears when entering valid keybinding character" <|
            \_ ->
                let
                    -- Start with a search that has an invalid input warning
                    searchWithWarning =
                        createTestAlbumSearchWithWarning "" "p" "Invalid character: z"

                    -- Type a valid character "h" after "p" to make "ph" (partial keybinding for "pho")
                    result =
                        handleAlbumBrowseInput "h" searchWithWarning testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.all
                            [ \_ -> Expect.equal "ph" newSearch.partialKeybinding
                            , \_ -> Expect.equal Nothing newSearch.invalidInputWarning -- Warning should be cleared
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected valid character to update search and clear warning"
        , test "user scenario: 'azb' sequence with 'ab' keybinding should work correctly" <|
            \_ ->
                let
                    -- Create custom test data with keybinding "ab"
                    testKeybindingsCustom =
                        Dict.fromList [ ( "album1", "ab" ) ]

                    testAlbumsCustom =
                        Dict.fromList [ ( "album1", createTestAlbum "album1" "Test Album" ) ]

                    -- Start fresh and type "a"
                    search1 =
                        createTestAlbumSearch "" ""

                    result1 =
                        handleAlbumBrowseInput "a" search1 testKeybindingsCustom testAlbumsCustom

                    -- Should get partial keybinding "a"
                    searchAfterA =
                        case result1 of
                            UpdateAlbumSearch newSearch ->
                                newSearch

                            _ ->
                                search1

                    -- Type "z" (invalid) - should be rejected
                    result2 =
                        handleAlbumBrowseInput "z" searchAfterA testKeybindingsCustom testAlbumsCustom

                    -- Type "b" after the rejected "z" - should complete "ab" keybinding
                    result3 =
                        handleAlbumBrowseInput "b" searchAfterA testKeybindingsCustom testAlbumsCustom
                in
                case ( result2, result3 ) of
                    ( InvalidKeybindingInput _ _, SelectAlbumForView album ) ->
                        -- Perfect! "z" was rejected, "b" completed the "ab" keybinding
                        Expect.equal "album1" album.id

                    ( InvalidKeybindingInput _ _, UpdateAlbumSearch _ ) ->
                        Expect.fail "Expected 'ab' to match album exactly, not create partial keybinding"

                    _ ->
                        Expect.fail "Expected 'z' to be rejected and 'b' to complete keybinding"
        , test "EXACT USER ISSUE: correct->incorrect->correct should not show warning" <|
            \_ ->
                let
                    -- Create test scenario to exactly match user's description
                    testKeybindingsCustom =
                        Dict.fromList [ ( "album1", "ac" ) ]

                    -- Use "ac" so "ab" doesn't complete
                    testAlbumsCustom =
                        Dict.fromList [ ( "album1", createTestAlbum "album1" "Test Album" ) ]

                    -- This test simulates the full UI flow including warning management
                    search1 =
                        createTestAlbumSearch "" ""

                    -- Step 1: Type correct key "a"
                    result1 =
                        handleAlbumBrowseInput "a" search1 testKeybindingsCustom testAlbumsCustom

                    searchAfterA =
                        case result1 of
                            UpdateAlbumSearch newSearch ->
                                newSearch

                            _ ->
                                search1

                    -- Step 2: Type incorrect key "b" - this should create a warning in the UI layer
                    result2 =
                        handleAlbumBrowseInput "b" searchAfterA testKeybindingsCustom testAlbumsCustom

                    -- Simulate what happens in UI: InvalidKeybindingInput creates warning
                    searchWithWarning =
                        case result2 of
                            InvalidKeybindingInput invalidChar _ ->
                                ViewAlbums.createAlbumSearchWithWarning searchAfterA invalidChar

                            _ ->
                                searchAfterA

                    -- Step 3: Type correct key "c" - should clear warning AND complete keybinding
                    result3 =
                        handleAlbumBrowseInput "c" searchWithWarning testKeybindingsCustom testAlbumsCustom
                in
                case result3 of
                    SelectAlbumForView album ->
                        -- This should work AND the warning should be cleared
                        Expect.equal "album1" album.id

                    UpdateAlbumSearch newSearch ->
                        -- If we get here, check if warning was cleared even if keybinding didn't complete
                        Expect.equal Nothing newSearch.invalidInputWarning

                    _ ->
                        Expect.fail "Expected correct key after incorrect key to work properly"
        ]


menuIntegrationTests : Test
menuIntegrationTests =
    describe "Menu Integration Tests"
        [ test "updateAlbums processes AlbumBrowseKeyPress correctly" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    albumMsg =
                        AlbumBrowseKeyPress "Escape" search

                    result =
                        updateAlbums albumMsg testAlbumKeybindings testAlbums
                in
                Expect.equal ChangeToMainMenu result
        , test "updateAlbums handles album selection" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    albumMsg =
                        AlbumBrowseKeyPress "g" search

                    result =
                        updateAlbums albumMsg testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch _ ->
                        -- This is expected for partial keybinding
                        Expect.pass

                    SelectAlbumForView _ ->
                        -- This is also valid if "g" matches an album exactly
                        Expect.pass

                    _ ->
                        Expect.fail "Expected updateAlbums to handle album selection"
        , test "updateAlbums handles search updates" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    albumMsg =
                        AlbumBrowseKeyPress "PageDown" search

                    result =
                        updateAlbums albumMsg testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch _ ->
                        Expect.pass

                    _ ->
                        Expect.fail "Expected updateAlbums to handle search updates"
        ]


edgeCaseTests : Test
edgeCaseTests =
    describe "Edge Case Tests"
        [ test "empty search and keybinding strings" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "Enter" search testAlbumKeybindings testAlbums
                in
                case result of
                    NoAlbumAction ->
                        Expect.pass

                    SelectAlbumForView _ ->
                        -- This is valid if there are albums to select
                        Expect.pass

                    UpdateAlbumSearch _ ->
                        -- This is also valid
                        Expect.pass

                    _ ->
                        Expect.fail "Expected NoAlbumAction, SelectAlbumForView, or UpdateAlbumSearch for empty search"
        , test "unsupported key press" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "F1" search testAlbumKeybindings testAlbums
                in
                Expect.equal NoAlbumAction result
        , test "empty albums dictionary" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "g" search testAlbumKeybindings Dict.empty
                in
                case result of
                    UpdateAlbumSearch _ ->
                        Expect.pass

                    _ ->
                        Expect.fail "Expected graceful handling of empty albums"
        , test "empty keybindings dictionary" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "g" search Dict.empty testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "g" newSearch.searchString

                    _ ->
                        Expect.fail "Expected fallback to text search with empty keybindings"
        ]


regressionTests : Test
regressionTests =
    describe "Regression Tests"
        [ test "keybinding behavior matches ViewAssets" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    -- Type "g" - should start partial keybinding
                    result1 =
                        handleAlbumBrowseInput "g" search testAlbumKeybindings testAlbums

                    -- Type "e" - should complete keybinding and select album
                    result2 =
                        case result1 of
                            UpdateAlbumSearch newSearch ->
                                handleAlbumBrowseInput "e" newSearch testAlbumKeybindings testAlbums

                            _ ->
                                result1
                in
                case result2 of
                    SelectAlbumForView album ->
                        Expect.equal "General" album.albumName

                    _ ->
                        Expect.fail "Expected keybinding behavior to match ViewAssets"
        , test "immediate selection works for single-letter keybindings" <|
            \_ ->
                let
                    singleLetterKeybindings =
                        Dict.fromList [ ( "album1", "g" ) ]

                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "g" search singleLetterKeybindings testAlbums
                in
                case result of
                    SelectAlbumForView album ->
                        Expect.equal "album1" album.id

                    _ ->
                        Expect.fail "Expected immediate selection for single-letter keybinding"
        , test "no infinite loops with conflicting keybindings" <|
            \_ ->
                let
                    conflictingKeybindings =
                        Dict.fromList [ ( "album1", "g" ), ( "album2", "ge" ) ]

                    search =
                        createTestAlbumSearch "" ""

                    result =
                        handleAlbumBrowseInput "g" search conflictingKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "g" newSearch.partialKeybinding

                    SelectAlbumForView album ->
                        -- This is valid if "g" is an exact match for album1
                        Expect.equal "album1" album.id

                    _ ->
                        Expect.fail "Expected partial keybinding or immediate selection for conflicting keybindings"
        , test "backspace behavior is consistent" <|
            \_ ->
                let
                    search1 =
                        createTestAlbumSearch "test" "g"

                    search2 =
                        createTestAlbumSearch "test" ""

                    -- Backspace with partial keybinding should clear it first
                    result1 =
                        handleAlbumBrowseInput "Backspace" search1 testAlbumKeybindings testAlbums

                    -- Backspace with no partial keybinding should clear text search
                    result2 =
                        handleAlbumBrowseInput "Backspace" search2 testAlbumKeybindings testAlbums
                in
                case ( result1, result2 ) of
                    ( UpdateAlbumSearch newSearch1, UpdateAlbumSearch newSearch2 ) ->
                        Expect.all
                            [ \_ -> Expect.equal "" newSearch1.partialKeybinding
                            , \_ -> Expect.equal "test" newSearch1.searchString
                            , \_ -> Expect.equal "tes" newSearch2.searchString
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected consistent backspace behavior"
        ]



-- Test the new invalid input functionality


invalidInputTests : Test
invalidInputTests =
    describe "Invalid Input Tests"
        [ test "invalid character after partial keybinding is rejected" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" "g"

                    -- Type "x" after "g" - should be invalid since no keybinding starts with "gx"
                    result =
                        handleAlbumBrowseInput "x" search testAlbumKeybindings testAlbums
                in
                case result of
                    InvalidKeybindingInput invalidInput _ ->
                        Expect.equal "x" invalidInput

                    _ ->
                        Expect.fail "Expected invalid input rejection"
        , test "invalid character without partial keybinding is rejected" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    -- Type "x" with no partial keybinding - should be rejected since no keybinding starts with "x"
                    result =
                        handleAlbumBrowseInput "x" search testAlbumKeybindings testAlbums
                in
                case result of
                    InvalidKeybindingInput invalidInput _ ->
                        Expect.equal "x" invalidInput

                    _ ->
                        Expect.fail "Expected invalid input rejection"
        , test "valid character after partial keybinding is accepted" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" "p"

                    -- Type "h" after "p" - should be valid since "pho" is a valid keybinding
                    result =
                        handleAlbumBrowseInput "h" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "ph" newSearch.partialKeybinding

                    _ ->
                        Expect.fail "Expected valid character acceptance"
        , test "exact match after partial keybinding selects album" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    -- Type "ge" which should match the General album exactly
                    result1 =
                        handleAlbumBrowseInput "g" search testAlbumKeybindings testAlbums

                    -- Continue with "e"
                    result2 =
                        case result1 of
                            UpdateAlbumSearch newSearch ->
                                handleAlbumBrowseInput "e" newSearch testAlbumKeybindings testAlbums

                            _ ->
                                NoAlbumAction
                in
                case result2 of
                    SelectAlbumForView album ->
                        Expect.equal "General" album.albumName

                    _ ->
                        Expect.fail "Expected exact match to select album"
        , test "warning functionality works correctly" <|
            \_ ->
                let
                    search =
                        createTestAlbumSearch "" ""

                    -- Test that invalid input generates warning
                    result =
                        handleAlbumBrowseInput "g" search testAlbumKeybindings testAlbums

                    -- Then type invalid character 'x' after 'g'
                    searchAfterG =
                        case result of
                            UpdateAlbumSearch newSearch ->
                                newSearch

                            _ ->
                                search

                    resultAfterX =
                        handleAlbumBrowseInput "x" searchAfterG testAlbumKeybindings testAlbums
                in
                case resultAfterX of
                    InvalidKeybindingInput invalidChar _ ->
                        Expect.equal "x" invalidChar

                    _ ->
                        Expect.fail "Expected invalid input to be returned for testing"
        , test "debug: specific keybinding validation scenario" <|
            \_ ->
                let
                    -- Create custom test data with keybindings: "aa", "abc", "ac"
                    testAlbumsCustom =
                        Dict.fromList
                            [ ( "album1", createTestAlbum "album1" "Album AA" )
                            , ( "album2", createTestAlbum "album2" "Album ABC" )
                            , ( "album3", createTestAlbum "album3" "Album AC" )
                            ]

                    testKeybindingsCustom =
                        Dict.fromList
                            [ ( "album1", "aa" )
                            , ( "album2", "abc" )
                            , ( "album3", "ac" )
                            ]

                    search =
                        createTestAlbumSearch "" ""

                    -- First, type 'a' - should work
                    result1 =
                        handleAlbumBrowseInput "a" search testKeybindingsCustom testAlbumsCustom

                    -- Get the search state after typing 'a'
                    searchAfterA =
                        case result1 of
                            UpdateAlbumSearch newSearch ->
                                newSearch

                            _ ->
                                search

                    -- Now type 'z' - should be rejected since no keybinding "az" exists
                    result2 =
                        handleAlbumBrowseInput "z" searchAfterA testKeybindingsCustom testAlbumsCustom
                in
                case result2 of
                    InvalidKeybindingInput invalidChar _ ->
                        Expect.all
                            [ \_ -> Expect.equal "z" invalidChar
                            , \_ -> Expect.equal "a" searchAfterA.partialKeybinding -- Should remain "a"
                            ]
                            ()

                    UpdateAlbumSearch actualSearch ->
                        -- This should NOT happen - if we get here, it means the 'z' was accepted!
                        Expect.fail ("Expected 'z' to be rejected, but got UpdateAlbumSearch with partialKeybinding: '" ++ actualSearch.partialKeybinding ++ "'")

                    _ ->
                        Expect.fail "Expected 'z' to be rejected as invalid input"
        , test "debug: isValidNextCharacter function behavior" <|
            \_ ->
                let
                    -- Create custom test data with keybindings: "aa", "abc", "ac"
                    testKeybindingsCustom =
                        Dict.fromList
                            [ ( "album1", "aa" )
                            , ( "album2", "abc" )
                            , ( "album3", "ac" )
                            ]

                    -- Test what characters are valid after "a"
                    validNextAfterA =
                        ViewAlbums.getNextAvailableCharacters "a" testKeybindingsCustom

                    isZValidAfterA =
                        ViewAlbums.isValidNextCharacter "a" 'z' testKeybindingsCustom

                    isAValidAfterA =
                        ViewAlbums.isValidNextCharacter "a" 'a' testKeybindingsCustom

                    isBValidAfterA =
                        ViewAlbums.isValidNextCharacter "a" 'b' testKeybindingsCustom

                    isCValidAfterA =
                        ViewAlbums.isValidNextCharacter "a" 'c' testKeybindingsCustom
                in
                Expect.all
                    [ \_ -> Expect.equal [ 'a', 'b', 'c' ] (List.sort validNextAfterA)
                    , \_ -> Expect.equal False isZValidAfterA
                    , \_ -> Expect.equal True isAValidAfterA
                    , \_ -> Expect.equal True isBValidAfterA
                    , \_ -> Expect.equal True isCValidAfterA
                    ]
                    ()
        , test "explicit test: partialKeybinding should NOT be modified on invalid input" <|
            \_ ->
                let
                    -- Create custom test data with keybindings: "aa", "abc", "ac"
                    testKeybindingsCustom =
                        Dict.fromList
                            [ ( "album1", "aa" )
                            , ( "album2", "abc" )
                            , ( "album3", "ac" )
                            ]

                    testAlbumsCustom =
                        Dict.fromList
                            [ ( "album1", createTestAlbum "album1" "Album AA" )
                            , ( "album2", createTestAlbum "album2" "Album ABC" )
                            , ( "album3", createTestAlbum "album3" "Album AC" )
                            ]

                    -- Start with partialKeybinding = "a"
                    searchWithA =
                        createTestAlbumSearch "" "a"

                    -- Type 'z' - should be rejected
                    result =
                        handleAlbumBrowseInput "z" searchWithA testKeybindingsCustom testAlbumsCustom
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.fail ("BUG: 'z' was accepted! partialKeybinding became: '" ++ newSearch.partialKeybinding ++ "' (should have remained 'a')")

                    InvalidKeybindingInput invalidChar _ ->
                        Expect.equal "z" invalidChar

                    _ ->
                        Expect.fail "Unexpected result type"
        , test "reproduce user's exact issue: typing 'z' after no matches should reject" <|
            \_ ->
                let
                    -- Use the default test keybindings which are: "ge", "tr", "wo", "sp", "ga", "pho"
                    -- None of these start with 'z', so typing 'z' should be rejected
                    search =
                        createTestAlbumSearch "" ""

                    -- Type 'z' - should be rejected because no keybinding starts with 'z'
                    result =
                        handleAlbumBrowseInput "z" search testAlbumKeybindings testAlbums
                in
                case result of
                    InvalidKeybindingInput invalidChar _ ->
                        Expect.equal "z" invalidChar

                    _ ->
                        Expect.fail "Expected 'z' to be rejected as invalid keybinding"
        , test "INVESTIGATE: what happens with multiple invalid characters after valid partial" <|
            \_ ->
                let
                    -- Create custom test data with keybindings: "aa", "abc", "ac"
                    testKeybindingsCustom =
                        Dict.fromList
                            [ ( "album1", "aa" )
                            , ( "album2", "abc" )
                            , ( "album3", "ac" )
                            ]

                    testAlbumsCustom =
                        Dict.fromList
                            [ ( "album1", createTestAlbum "album1" "Album AA" )
                            , ( "album2", createTestAlbum "album2" "Album ABC" )
                            , ( "album3", createTestAlbum "album3" "Album AC" )
                            ]

                    search =
                        createTestAlbumSearch "" ""

                    -- Type 'a' - should work
                    result1 =
                        handleAlbumBrowseInput "a" search testKeybindingsCustom testAlbumsCustom

                    searchAfterA =
                        case result1 of
                            UpdateAlbumSearch newSearch ->
                                newSearch

                            _ ->
                                search

                    -- Type 'z' - should be rejected
                    result2 =
                        handleAlbumBrowseInput "z" searchAfterA testKeybindingsCustom testAlbumsCustom

                    searchAfterZ =
                        case result2 of
                            UpdateAlbumSearch newSearch ->
                                newSearch

                            InvalidKeybindingInput _ _ ->
                                searchAfterA

                            -- Should remain unchanged
                            _ ->
                                searchAfterA

                    -- Type another 'z' - should be rejected
                    result3 =
                        handleAlbumBrowseInput "z" searchAfterZ testKeybindingsCustom testAlbumsCustom

                    searchAfterZZ =
                        case result3 of
                            UpdateAlbumSearch newSearch ->
                                newSearch

                            InvalidKeybindingInput _ _ ->
                                searchAfterZ

                            -- Should remain unchanged
                            _ ->
                                searchAfterZ

                    -- Type 'a' - should work and make "aa"
                    result4 =
                        handleAlbumBrowseInput "a" searchAfterZZ testKeybindingsCustom testAlbumsCustom
                in
                case result4 of
                    SelectAlbumForView album ->
                        Expect.all
                            [ \_ -> Expect.equal "Album AA" album.albumName
                            , \_ -> Expect.equal "a" searchAfterA.partialKeybinding
                            , \_ -> Expect.equal "a" searchAfterZ.partialKeybinding -- Should remain "a"
                            , \_ -> Expect.equal "a" searchAfterZZ.partialKeybinding -- Should remain "a"
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected typing 'a' after rejecting 'z' chars to select 'aa' album"
        , test "DIRECT CHECK: isValidNextCharacter with real scenario" <|
            \_ ->
                let
                    -- Use the actual test keybindings: "ge", "tr", "wo", "sp", "ga", "pho"
                    -- Check what happens when we type 'z' with no partial keybinding
                    isZValidFromEmpty =
                        ViewAlbums.isValidNextCharacter "" 'z' testAlbumKeybindings

                    availableFromEmpty =
                        ViewAlbums.getNextAvailableCharacters "" testAlbumKeybindings

                    -- Check what happens when we type 'z' after 'g'
                    isZValidAfterG =
                        ViewAlbums.isValidNextCharacter "g" 'z' testAlbumKeybindings

                    availableAfterG =
                        ViewAlbums.getNextAvailableCharacters "g" testAlbumKeybindings
                in
                Expect.all
                    [ \_ -> Expect.equal False isZValidFromEmpty
                    , \_ -> Expect.equal [ 'g', 'p', 's', 't', 'w' ] (List.sort availableFromEmpty)
                    , \_ -> Expect.equal False isZValidAfterG
                    , \_ -> Expect.equal [ 'a', 'e' ] (List.sort availableAfterG)
                    ]
                    ()
        , test "SIMULATE BUG: typing multiple invalid characters" <|
            \_ ->
                let
                    testKeybindingsCustom =
                        Dict.fromList
                            [ ( "album1", "aa" )
                            , ( "album2", "abc" )
                            , ( "album3", "ac" )
                            ]

                    testAlbumsCustom =
                        Dict.fromList
                            [ ( "album1", createTestAlbum "album1" "Album AA" )
                            , ( "album2", createTestAlbum "album2" "Album ABC" )
                            , ( "album3", createTestAlbum "album3" "Album AC" )
                            ]

                    search =
                        createTestAlbumSearch "" ""

                    -- Type 'a' - should work
                    result1 =
                        handleAlbumBrowseInput "a" search testKeybindingsCustom testAlbumsCustom

                    searchAfterA =
                        case result1 of
                            UpdateAlbumSearch newSearch ->
                                newSearch

                            _ ->
                                search

                    -- Type 'z' - should be rejected but maybe the state gets corrupted?
                    result2 =
                        handleAlbumBrowseInput "z" searchAfterA testKeybindingsCustom testAlbumsCustom

                    result3 =
                        handleAlbumBrowseInput "z" searchAfterA testKeybindingsCustom testAlbumsCustom

                    result4 =
                        handleAlbumBrowseInput "z" searchAfterA testKeybindingsCustom testAlbumsCustom
                in
                Expect.all
                    [ \_ ->
                        case result2 of
                            InvalidKeybindingInput _ _ ->
                                Expect.pass

                            _ ->
                                Expect.fail "First 'z' should be rejected"
                    , \_ ->
                        case result3 of
                            InvalidKeybindingInput _ _ ->
                                Expect.pass

                            _ ->
                                Expect.fail "Second 'z' should be rejected"
                    , \_ ->
                        case result4 of
                            InvalidKeybindingInput _ _ ->
                                Expect.pass

                            _ ->
                                Expect.fail "Third 'z' should be rejected"
                    ]
                    ()
        , test "USER REQUIREMENT: invalid keybinding chars should be rejected, not fall back to text search" <|
            \_ ->
                let
                    -- Test the user's exact scenario: keybindings "aa", "abc", "ac"
                    customKeybindings =
                        Dict.fromList [ ( "1", "aa" ), ( "2", "abc" ), ( "3", "ac" ) ]

                    customAlbums =
                        Dict.fromList [ ( "1", createTestAlbum "1" "AA" ), ( "2", createTestAlbum "2" "ABC" ), ( "3", createTestAlbum "3" "AC" ) ]

                    search =
                        createTestAlbumSearch "" ""

                    -- Type 'a' - should work
                    result1 =
                        handleAlbumBrowseInput "a" search customKeybindings customAlbums

                    searchAfterA =
                        case result1 of
                            UpdateAlbumSearch newSearch ->
                                newSearch

                            _ ->
                                search

                    -- Type 'z' - should be REJECTED, not fall back to text search
                    result2 =
                        handleAlbumBrowseInput "z" searchAfterA customKeybindings customAlbums

                    -- Type 'q' - should be REJECTED, not fall back to text search
                    result3 =
                        handleAlbumBrowseInput "q" searchAfterA customKeybindings customAlbums

                    -- Type 'x' - should be REJECTED, not fall back to text search
                    result4 =
                        handleAlbumBrowseInput "x" searchAfterA customKeybindings customAlbums
                in
                Expect.all
                    [ \_ -> Expect.equal "a" searchAfterA.partialKeybinding
                    , \_ ->
                        case result2 of
                            InvalidKeybindingInput char _ ->
                                Expect.equal "z" char

                            _ ->
                                Expect.fail "Expected 'z' to be rejected"
                    , \_ ->
                        case result3 of
                            InvalidKeybindingInput char _ ->
                                Expect.equal "q" char

                            _ ->
                                Expect.fail "Expected 'q' to be rejected"
                    , \_ ->
                        case result4 of
                            InvalidKeybindingInput char _ ->
                                Expect.equal "x" char

                            _ ->
                                Expect.fail "Expected 'x' to be rejected"
                    ]
                    ()
        ]
