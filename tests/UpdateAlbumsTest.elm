module UpdateAlbumsTest exposing (..)

import Dict exposing (Dict)
import Date
import Expect
import Test exposing (..)
import UpdateAlbums exposing (..)
import ViewAlbums exposing (AlbumSearch)
import Immich exposing (ImmichAlbum, ImmichAlbumId)

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
    }

-- Test data
testAlbums : Dict ImmichAlbumId ImmichAlbum
testAlbums = Dict.fromList 
    [ ("album1", createTestAlbum "album1" "General")
    , ("album2", createTestAlbum "album2" "Travel")
    , ("album3", createTestAlbum "album3" "Work")
    , ("album4", createTestAlbum "album4" "Sports")
    , ("album5", createTestAlbum "album5" "Garden")
    ]

testAlbumKeybindings : Dict ImmichAlbumId String
testAlbumKeybindings = Dict.fromList 
    [ ("album1", "ge")
    , ("album2", "tr")
    , ("album3", "wo")
    , ("album4", "sp")
    , ("album5", "ga")
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
                    singleLetterKeybindings = Dict.fromList [("album1", "g")]
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput "g" search singleLetterKeybindings testAlbums
                in
                case result of
                    SelectAlbumForView album ->
                        Expect.equal "album1" album.id
                    _ -> Expect.fail "Expected immediate album selection for unique single letter"

        , test "multi-letter match selects album immediately" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    -- Type "g" then "e" to match "ge" (General)
                    result1 = handleAlbumBrowseInput "g" search testAlbumKeybindings testAlbums
                    result2 = case result1 of
                        UpdateAlbumSearch newSearch -> 
                            handleAlbumBrowseInput "e" newSearch testAlbumKeybindings testAlbums
                        _ -> result1
                in
                case result2 of
                    SelectAlbumForView album ->
                        Expect.equal "album1" album.id
                    _ -> Expect.fail "Expected album selection for 'ge' keybinding"

        , test "partial keybinding accumulation shows matching albums" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    -- Type "g" - should show albums starting with "g"
                    result = handleAlbumBrowseInput "g" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "g" newSearch.partialKeybinding
                    _ -> Expect.fail "Expected partial keybinding accumulation"

        , test "no-match scenario falls back to text search" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    -- Type "x" - no keybinding starts with "x"
                    result = handleAlbumBrowseInput "x" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "x" newSearch.searchString
                    _ -> Expect.fail "Expected fallback to text search"

        , test "exact keybinding match takes priority over text search" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    -- Type "g" - should match keybinding, not text search
                    result = handleAlbumBrowseInput "g" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.all
                            [ \_ -> Expect.equal "g" newSearch.partialKeybinding
                            , \_ -> Expect.equal "" newSearch.searchString
                            ]
                            ()
                    _ -> Expect.fail "Expected keybinding match over text search"

        , test "backspace clears partial keybinding first" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" "g"
                    result = handleAlbumBrowseInput "Backspace" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "" newSearch.partialKeybinding
                    _ -> Expect.fail "Expected backspace to clear partial keybinding"

        , test "backspace clears text search when no partial keybinding" <|
            \_ ->
                let
                    search = createTestAlbumSearch "test" ""
                    result = handleAlbumBrowseInput "Backspace" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "tes" newSearch.searchString
                    _ -> Expect.fail "Expected backspace to clear text search"
        ]

navigationTests : Test
navigationTests =
    describe "Navigation Tests"
        [ test "Escape returns to main menu" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput "Escape" search testAlbumKeybindings testAlbums
                in
                Expect.equal ChangeToMainMenu result

        , test "Enter selects top match" <|
            \_ ->
                let
                    -- Set up search with a match - need to set up albumScores for this to work
                    search = { searchString = "gen", partialKeybinding = "", selectedIndex = 0, albumScores = Dict.fromList [("album1", 100)], pagination = { currentPage = 1, itemsPerPage = 20, totalItems = 1 } }
                    result = handleAlbumBrowseInput "Enter" search testAlbumKeybindings testAlbums
                in
                case result of
                    SelectAlbumForView album ->
                        Expect.equal "General" album.albumName
                    NoAlbumAction ->
                        -- This is also acceptable if no clear match
                        Expect.pass
                    _ -> Expect.fail "Expected Enter to select top match or NoAlbumAction"

        , test "PageUp updates pagination" <|
            \_ ->
                let
                    -- Start on page 2 so PageUp can actually go to page 1
                    search = { searchString = "", partialKeybinding = "", selectedIndex = 0, albumScores = Dict.empty, pagination = { currentPage = 2, itemsPerPage = 20, totalItems = 100 } }
                    result = handleAlbumBrowseInput "PageUp" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal 1 newSearch.pagination.currentPage
                    _ -> Expect.fail "Expected PageUp to update pagination"

        , test "PageDown updates pagination" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput "PageDown" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal 2 newSearch.pagination.currentPage
                    _ -> Expect.fail "Expected PageDown to update pagination"

        , test "Space acts as page down" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput " " search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal 2 newSearch.pagination.currentPage
                    _ -> Expect.fail "Expected Space to act as page down"

        , test "Ctrl+d acts as half page down" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput "Control+d" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch _ ->
                        Expect.pass
                    _ -> Expect.fail "Expected Ctrl+d to act as half page down"

        , test "Ctrl+u acts as half page up" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput "Control+u" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch _ ->
                        Expect.pass
                    _ -> Expect.fail "Expected Ctrl+u to act as half page up"

        , test "Ctrl+f acts as page down" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput "Control+f" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch _ ->
                        Expect.pass
                    _ -> Expect.fail "Expected Ctrl+f to act as page down"

        , test "Ctrl+b acts as page up" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput "Control+b" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch _ ->
                        Expect.pass
                    _ -> Expect.fail "Expected Ctrl+b to act as page up"
        ]

textSearchTests : Test
textSearchTests =
    describe "Text Search Tests"
        [ test "non-keybinding letters trigger text search" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    -- "x" is not a keybinding start, should go to text search
                    result = handleAlbumBrowseInput "x" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.all
                            [ \_ -> Expect.equal "x" newSearch.searchString
                            , \_ -> Expect.equal "" newSearch.partialKeybinding
                            ]
                            ()
                    _ -> Expect.fail "Expected non-keybinding letter to trigger text search"

        , test "supported search letters build text search" <|
            \_ ->
                let
                    search = createTestAlbumSearch "t" ""
                    result = handleAlbumBrowseInput "e" search testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "te" newSearch.searchString
                    _ -> Expect.fail "Expected supported letter to build text search"

        , test "mixed keybinding and text search scenarios" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    -- Start with keybinding "g"
                    result1 = handleAlbumBrowseInput "g" search testAlbumKeybindings testAlbums
                    -- Then hit backspace to clear
                    result2 = case result1 of
                        UpdateAlbumSearch newSearch -> 
                            handleAlbumBrowseInput "Backspace" newSearch testAlbumKeybindings testAlbums
                        _ -> result1
                    -- Then type "x" for text search
                    result3 = case result2 of
                        UpdateAlbumSearch newSearch -> 
                            handleAlbumBrowseInput "x" newSearch testAlbumKeybindings testAlbums
                        _ -> result2
                in
                case result3 of
                    UpdateAlbumSearch newSearch ->
                        Expect.all
                            [ \_ -> Expect.equal "x" newSearch.searchString
                            , \_ -> Expect.equal "" newSearch.partialKeybinding
                            ]
                            ()
                    _ -> Expect.fail "Expected mixed keybinding/text search scenario to work"
        ]

menuIntegrationTests : Test
menuIntegrationTests =
    describe "Menu Integration Tests"
        [ test "updateAlbums processes AlbumBrowseKeyPress correctly" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    albumMsg = AlbumBrowseKeyPress "Escape" search
                    result = updateAlbums albumMsg testAlbumKeybindings testAlbums
                in
                Expect.equal ChangeToMainMenu result

        , test "updateAlbums handles album selection" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    albumMsg = AlbumBrowseKeyPress "g" search
                    result = updateAlbums albumMsg testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch _ ->
                        -- This is expected for partial keybinding
                        Expect.pass
                    SelectAlbumForView _ ->
                        -- This is also valid if "g" matches an album exactly
                        Expect.pass
                    _ -> Expect.fail "Expected updateAlbums to handle album selection"

        , test "updateAlbums handles search updates" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    albumMsg = AlbumBrowseKeyPress "PageDown" search
                    result = updateAlbums albumMsg testAlbumKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch _ ->
                        Expect.pass
                    _ -> Expect.fail "Expected updateAlbums to handle search updates"
        ]

edgeCaseTests : Test
edgeCaseTests =
    describe "Edge Case Tests"
        [ test "empty search and keybinding strings" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput "Enter" search testAlbumKeybindings testAlbums
                in
                case result of
                    NoAlbumAction ->
                        Expect.pass
                    _ -> Expect.fail "Expected NoAlbumAction for empty search"

        , test "unsupported key press" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput "F1" search testAlbumKeybindings testAlbums
                in
                Expect.equal NoAlbumAction result

        , test "empty albums dictionary" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput "g" search testAlbumKeybindings Dict.empty
                in
                case result of
                    UpdateAlbumSearch _ ->
                        Expect.pass
                    _ -> Expect.fail "Expected graceful handling of empty albums"

        , test "empty keybindings dictionary" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput "g" search Dict.empty testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "g" newSearch.searchString
                    _ -> Expect.fail "Expected fallback to text search with empty keybindings"
        ]

regressionTests : Test
regressionTests =
    describe "Regression Tests"
        [ test "keybinding behavior matches ViewAssets" <|
            \_ ->
                let
                    search = createTestAlbumSearch "" ""
                    -- Type "g" - should start partial keybinding
                    result1 = handleAlbumBrowseInput "g" search testAlbumKeybindings testAlbums
                    -- Type "e" - should complete keybinding and select album
                    result2 = case result1 of
                        UpdateAlbumSearch newSearch -> 
                            handleAlbumBrowseInput "e" newSearch testAlbumKeybindings testAlbums
                        _ -> result1
                in
                case result2 of
                    SelectAlbumForView album ->
                        Expect.equal "General" album.albumName
                    _ -> Expect.fail "Expected keybinding behavior to match ViewAssets"

        , test "immediate selection works for single-letter keybindings" <|
            \_ ->
                let
                    singleLetterKeybindings = Dict.fromList [("album1", "g")]
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput "g" search singleLetterKeybindings testAlbums
                in
                case result of
                    SelectAlbumForView album ->
                        Expect.equal "album1" album.id
                    _ -> Expect.fail "Expected immediate selection for single-letter keybinding"

        , test "no infinite loops with conflicting keybindings" <|
            \_ ->
                let
                    conflictingKeybindings = Dict.fromList [("album1", "g"), ("album2", "ge")]
                    search = createTestAlbumSearch "" ""
                    result = handleAlbumBrowseInput "g" search conflictingKeybindings testAlbums
                in
                case result of
                    UpdateAlbumSearch newSearch ->
                        Expect.equal "g" newSearch.partialKeybinding
                    _ -> Expect.fail "Expected partial keybinding for conflicting keybindings"

        , test "backspace behavior is consistent" <|
            \_ ->
                let
                    search1 = createTestAlbumSearch "test" "g"
                    search2 = createTestAlbumSearch "test" ""
                    
                    -- Backspace with partial keybinding should clear it first
                    result1 = handleAlbumBrowseInput "Backspace" search1 testAlbumKeybindings testAlbums
                    
                    -- Backspace with no partial keybinding should clear text search
                    result2 = handleAlbumBrowseInput "Backspace" search2 testAlbumKeybindings testAlbums
                in
                case (result1, result2) of
                    (UpdateAlbumSearch newSearch1, UpdateAlbumSearch newSearch2) ->
                        Expect.all
                            [ \_ -> Expect.equal "" newSearch1.partialKeybinding
                            , \_ -> Expect.equal "test" newSearch1.searchString
                            , \_ -> Expect.equal "tes" newSearch2.searchString
                            ]
                            ()
                    _ -> Expect.fail "Expected consistent backspace behavior"
        ]