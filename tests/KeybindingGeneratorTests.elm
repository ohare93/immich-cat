module KeybindingGeneratorTests exposing (..)

import Dict exposing (Dict)
import Date
import Expect
import Test exposing (..)
import KeybindingGenerator exposing (..)
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichAsset)


-- Helper function to create test albums
createTestAlbum : String -> String -> ImmichAlbum
createTestAlbum id albumName =
    { id = id
    , albumName = albumName
    , assetCount = 0
    , assets = []
    , createdAt = Date.fromRataDie 1  -- dummy date
    }


-- Comprehensive test suite
suite : Test
suite =
    describe "Keybinding Generator Tests"
        [ candidateGenerationTests
        , regressionTests
        , prefixConflictTests
        , shortAlbumTests
        , utilityFunctionTests
        ]


candidateGenerationTests : Test
candidateGenerationTests =
    describe "Candidate generation prioritization"
        [ test "single word 'general' prioritizes 'ge' over 'g'" <|
            \_ ->
                let
                    candidates = generateSimpleCandidates "general"
                in
                case candidates of
                    first :: _ ->
                        Expect.equal "ge" first
                    [] ->
                        Expect.fail "No candidates generated"
        
        , test "multi-word 'test things' prioritizes 'tt' (word abbrev)" <|
            \_ ->
                let
                    candidates = generateSimpleCandidates "test things"
                in
                case candidates of
                    first :: _ ->
                        Expect.equal "tt" first
                    [] ->
                        Expect.fail "No candidates generated"
        
        , test "single word 'test' prioritizes 'te' over 't'" <|
            \_ ->
                let
                    candidates = generateSimpleCandidates "test"
                in
                case candidates of
                    first :: _ ->
                        Expect.equal "te" first
                    [] ->
                        Expect.fail "No candidates generated"
        
        , test "generateTwoCharVariants works correctly" <|
            \_ ->
                generateTwoCharVariants "travel"
                    |> Expect.equal ["tr", "ta", "tv", "te"]
        
        , test "generateRepeatingVariants for short names" <|
            \_ ->
                generateRepeatingVariants "go"
                    |> Expect.equal ["go", "gog", "gogo"]
        ]


regressionTests : Test
regressionTests =
    describe "Regression tests for specific reported issues"
        [ test "general gets 'ge' not 'gn'" <|
            \_ ->
                let
                    albums = [ createTestAlbum "1" "general" ]
                    keybindings = generateAlbumKeybindings albums
                in
                Expect.equal (Just "ge") (Dict.get "1" keybindings)
        
        , test "single letter album 'h' gets reasonable keybinding" <|
            \_ ->
                let
                    albums = [ createTestAlbum "1" "h" ]
                    keybindings = generateAlbumKeybindings albums
                in
                case Dict.get "1" keybindings of
                    Just kb -> 
                        Expect.all
                            [ \_ -> Expect.lessThan 10 (String.length kb)
                            , \_ -> Expect.equal True (String.startsWith "h" kb)
                            ]
                            ()
                    Nothing -> Expect.fail "No keybinding generated for 'h'"
        
        , test "test vs test things should not conflict" <|
            \_ ->
                let
                    albums = 
                        [ createTestAlbum "1" "test"
                        , createTestAlbum "2" "test things"
                        ]
                    keybindings = generateAlbumKeybindings albums
                in
                Expect.equal 
                    (Dict.fromList [("1", "te"), ("2", "tt")])
                    keybindings
        
        , test "albums with numbers should work" <|
            \_ ->
                let
                    albums = 
                        [ createTestAlbum "1" "2023"
                        , createTestAlbum "2" "Photos 2022"
                        ]
                    keybindings = generateAlbumKeybindings albums
                in
                Expect.all
                    [ \_ -> Expect.notEqual Nothing (Dict.get "1" keybindings)
                    , \_ -> Expect.notEqual Nothing (Dict.get "2" keybindings)
                    ]
                    ()
        ]


prefixConflictTests : Test
prefixConflictTests =
    describe "Prefix conflict resolution"
        [ test "Travel, Tourism, Test should not all get 't'" <|
            \_ ->
                let
                    albums = 
                        [ createTestAlbum "1" "Travel"
                        , createTestAlbum "2" "Tourism"
                        , createTestAlbum "3" "Test"
                        ]
                    keybindings = generateAlbumKeybindings albums
                    values = Dict.values keybindings
                    tCount = List.filter (\kb -> kb == "t") values |> List.length
                in
                Expect.equal 0 tCount -- None should get just "t"
        
        , test "j vs Japan conflict resolution - j should get jj, Japan should get ja" <|
            \_ ->
                let
                    albums = 
                        [ createTestAlbum "1" "j"
                        , createTestAlbum "2" "Japan"
                        ]
                    keybindings = generateAlbumKeybindings albums
                in
                Expect.all
                    [ \_ -> Expect.equal (Just "jj") (Dict.get "1" keybindings)
                    , \_ -> Expect.equal (Just "ja") (Dict.get "2" keybindings) 
                    , \_ -> Expect.notEqual (Just "j") (Dict.get "1" keybindings) -- j should NOT get single "j"
                    ]
                    ()
        
        , test "J vs Japan vs J Test smart conflict resolution" <|
            \_ ->
                let
                    albums = 
                        [ createTestAlbum "1" "J"
                        , createTestAlbum "2" "Japan"
                        , createTestAlbum "3" "J Test"
                        ]
                    keybindings = generateAlbumKeybindings albums
                in
                Expect.all
                    [ \_ -> Expect.equal (Just "jj") (Dict.get "1" keybindings)  -- J gets jj, not j
                    , \_ -> Expect.equal (Just "ja") (Dict.get "2" keybindings)  -- Japan gets ja
                    , \_ -> Expect.equal (Just "jt") (Dict.get "3" keybindings)  -- J Test gets jt
                    ]
                    ()
        
        , test "J test test should get jt, not jtt (repeated letters)" <|
            \_ ->
                let
                    albums = 
                        [ createTestAlbum "1" "J"
                        , createTestAlbum "2" "Japan"
                        , createTestAlbum "3" "J test test"
                        ]
                    keybindings = generateAlbumKeybindings albums
                in
                Expect.all
                    [ \_ -> Expect.equal (Just "jj") (Dict.get "1" keybindings)
                    , \_ -> Expect.equal (Just "ja") (Dict.get "2" keybindings)
                    , \_ -> Expect.equal (Just "jt") (Dict.get "3" keybindings)  -- Should get jt, not jtt
                    , \_ -> Expect.notEqual (Just "jtt") (Dict.get "3" keybindings)  -- Should NOT get jtt
                    ]
                    ()
        
        , test "J test stuff should get jt, not jts (3+ char abbreviation)" <|
            \_ ->
                let
                    albums = 
                        [ createTestAlbum "1" "J"
                        , createTestAlbum "2" "Japan"
                        , createTestAlbum "3" "J test stuff"
                        ]
                    keybindings = generateAlbumKeybindings albums
                in
                Expect.all
                    [ \_ -> Expect.equal (Just "jj") (Dict.get "1" keybindings)
                    , \_ -> Expect.equal (Just "ja") (Dict.get "2" keybindings)
                    , \_ -> Expect.equal (Just "jt") (Dict.get "3" keybindings)  -- Should get jt, not jts
                    , \_ -> Expect.notEqual (Just "jts") (Dict.get "3" keybindings)  -- Should NOT get jts
                    ]
                    ()
        
        , test "Church, Christianity, Christmas prefix conflict resolution" <|
            \_ ->
                let
                    albums = 
                        [ createTestAlbum "1" "Church"
                        , createTestAlbum "2" "Christianity"
                        , createTestAlbum "3" "Christmas"
                        ]
                    keybindings = generateAlbumKeybindings albums
                    values = Dict.values keybindings
                    uniqueValues = removeDuplicates values
                    prefixConflicts = findPrefixConflicts values
                in
                Expect.all
                    [ \_ -> Expect.equal (List.length values) (List.length uniqueValues)
                    , \_ -> Expect.equal [] prefixConflicts
                    ]
                    ()
        
        , test "no keybinding should be a prefix of another" <|
            \_ ->
                let
                    albums = 
                        [ createTestAlbum "1" "Category"
                        , createTestAlbum "2" "Cat"
                        , createTestAlbum "3" "Car"
                        , createTestAlbum "4" "Care"
                        ]
                    keybindings = generateAlbumKeybindings albums
                    values = Dict.values keybindings
                    prefixConflicts = findPrefixConflicts values
                in
                Expect.equal [] prefixConflicts
        
        , test "wouldCreatePrefixConflict detects conflicts correctly" <|
            \_ ->
                let
                    used = Dict.fromList [("c", True), ("ch", True)]
                in
                Expect.all
                    [ \_ -> Expect.equal True (wouldCreatePrefixConflict "cr" used)  -- "c" is prefix of "cr"
                    , \_ -> Expect.equal False (wouldCreatePrefixConflict "te" used) -- No conflict
                    , \_ -> Expect.equal True (wouldCreatePrefixConflict "c" used)   -- Already used
                    ]
                    ()
        ]


shortAlbumTests : Test
shortAlbumTests =
    describe "Short album name handling"
        [ test "single letter albums get repeating patterns" <|
            \_ ->
                let
                    albums = 
                        [ createTestAlbum "1" "h"
                        , createTestAlbum "2" "b"
                        ]
                    keybindings = generateAlbumKeybindings albums
                in
                case (Dict.get "1" keybindings, Dict.get "2" keybindings) of
                    (Just kb1, Just kb2) ->
                        Expect.all
                            [ \_ -> Expect.notEqual kb1 kb2
                            , \_ -> Expect.equal True (String.startsWith "h" kb1)
                            , \_ -> Expect.equal True (String.startsWith "b" kb2)
                            ]
                            ()
                    _ -> Expect.fail "Both albums should get keybindings"
        
        , test "two letter albums work properly" <|
            \_ ->
                let
                    albums = 
                        [ createTestAlbum "1" "go"
                        , createTestAlbum "2" "to"
                        ]
                    keybindings = generateAlbumKeybindings albums
                in
                case (Dict.get "1" keybindings, Dict.get "2" keybindings) of
                    (Just kb1, Just kb2) ->
                        Expect.all
                            [ \_ -> Expect.notEqual kb1 kb2
                            , \_ -> Expect.equal True (String.length kb1 >= 2)
                            , \_ -> Expect.equal True (String.length kb2 >= 2)
                            ]
                            ()
                    _ -> Expect.fail "Both albums should get keybindings"
        ]


utilityFunctionTests : Test
utilityFunctionTests =
    describe "Utility functions"
        [ test "removeDuplicates function works correctly" <|
            \_ ->
                removeDuplicates ["a", "b", "a", "c", "b"]
                    |> Expect.equal ["a", "b", "c"]
        
        , test "removeDuplicates keeps first occurrence" <|
            \_ ->
                removeDuplicates ["first", "second", "first"]
                    |> Expect.equal ["first", "second"]
        
        , test "isValidKeybinding accepts valid keybindings" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal True (isValidKeybinding "abc")
                    , \_ -> Expect.equal True (isValidKeybinding "test123")
                    , \_ -> Expect.equal True (isValidKeybinding "a1")
                    ]
                    ()
        
        , test "isValidKeybinding rejects invalid keybindings" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal False (isValidKeybinding "ABC")  -- uppercase
                    , \_ -> Expect.equal False (isValidKeybinding "test-123")  -- hyphen
                    , \_ -> Expect.equal False (isValidKeybinding "")  -- empty
                    ]
                    ()
        ]


-- Helper function to find prefix conflicts
findPrefixConflicts : List String -> List (String, String)
findPrefixConflicts keybindings =
    List.concatMap (\kb1 -> 
        List.filterMap (\kb2 -> 
            if kb1 /= kb2 && String.startsWith kb1 kb2 then
                Just (kb1, kb2)
            else
                Nothing
        ) keybindings
    ) keybindings