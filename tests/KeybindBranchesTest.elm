module KeybindBranchesTest exposing (..)

import Date
import Dict
import Expect
import Fuzz
import Immich exposing (ImmichAlbum)
import KeybindBranches exposing (createBranchDictionary, generateAlbumKeybindings, generateBranches)
import Set
import Test exposing (Test, describe, fuzz, test)
import TestGenerators exposing (testAlbumGenerator)



-- Helper function to create test albums


createTestAlbum : String -> String -> ImmichAlbum
createTestAlbum id albumName =
    { id = id
    , albumName = albumName
    , assetCount = 0
    , assets = []
    , createdAt = Date.fromRataDie 1
    }


suite : Test
suite =
    describe "KeybindBranches Tests"
        [ branchGenerationTests
        , conflictResolutionTests
        , prefixConflictTests
        , iterativeShorteningTests
        , integrationTests
        , fuzzTests
        ]


branchGenerationTests : Test
branchGenerationTests =
    describe "Branch Generation Tests"
        [ test "Single word 'Tech' generates ['tech']" <|
            \_ ->
                let
                    branches =
                        generateBranches "Tech"
                in
                Expect.equal [ "tech" ] branches
        , test "Multi-word 'The World' generates ['tworld', 'thworld', 'theworld']" <|
            \_ ->
                let
                    branches =
                        generateBranches "The World"
                in
                Expect.equal [ "tworld", "thworld", "theworld" ] branches
        , test "Multi-word 'To Be Sorted' generates at least 2 branches including first and last" <|
            \_ ->
                let
                    branches =
                        generateBranches "To Be Sorted"
                in
                Expect.all
                    [ \_ -> Expect.atLeast 2 (List.length branches)
                    , \_ -> Expect.equal True (List.member "tbsorted" branches)
                    , \_ -> Expect.equal True (List.member "tobesorted" branches)
                    ]
                    ()
        , test "Long single word 'Christianity' generates ['christianity'] in Stage 1" <|
            \_ ->
                let
                    branches =
                        generateBranches "Christianity"
                in
                Expect.equal [ "christianity" ] branches
        , test "Short single word 'Cat' generates ['cat']" <|
            \_ ->
                let
                    branches =
                        generateBranches "Cat"
                in
                Expect.equal [ "cat" ] branches
        , test "Four letter word 'Cool' generates ['cool']" <|
            \_ ->
                let
                    branches =
                        generateBranches "Cool"
                in
                Expect.equal [ "cool" ] branches
        ]


conflictResolutionTests : Test
conflictResolutionTests =
    describe "Conflict Resolution Tests"
        [ test "Conflicting branches are removed entirely" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "Christianity"
                        , createTestAlbum "2" "Christy"
                        ]

                    branchDict =
                        createBranchDictionary albums

                    -- Both Christianity and Christy would generate "chry",
                    -- so neither should get it
                    hasChry =
                        Dict.member "chry" branchDict
                in
                Expect.equal False hasChry
        , test "Non-conflicting branches are preserved" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "Tech"
                        , createTestAlbum "2" "General"
                        ]

                    branchDict =
                        createBranchDictionary albums

                    hasTech =
                        Dict.member "tech" branchDict

                    hasGeneral =
                        Dict.member "general" branchDict
                in
                Expect.all
                    [ \_ -> Expect.equal True hasTech
                    , \_ -> Expect.equal True hasGeneral
                    ]
                    ()
        ]


prefixConflictTests : Test
prefixConflictTests =
    describe "Prefix Conflict Tests"
        [ test "'Tech' vs 'Technology' creates prefix conflict" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "Tech"
                        , createTestAlbum "2" "Technology"
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    techBinding =
                        Dict.get "1" finalKeybindings

                    technologyBinding =
                        Dict.get "2" finalKeybindings
                in
                case ( techBinding, technologyBinding ) of
                    ( Just tech, Just technology ) ->
                        -- For now, just check they're different and neither is empty
                        Expect.all
                            [ \_ -> Expect.notEqual tech technology
                            , \_ -> Expect.notEqual "" tech
                            , \_ -> Expect.notEqual "" technology
                            ]
                            ()

                    _ ->
                        Expect.fail "Both albums should have keybindings"
        ]


iterativeShorteningTests : Test
iterativeShorteningTests =
    describe "Iterative Shortening Tests"
        [ test "Debug branch dictionary creation for Comics/Communism/Comedians" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "Comics"
                        , createTestAlbum "2" "Communism"
                        , createTestAlbum "3" "Comedians"
                        ]

                    branchDict =
                        createBranchDictionary albums

                    keys =
                        Dict.keys branchDict |> List.sort
                in
                -- Should see which branches survive after conflict removal
                -- Stage 1: only full words for single words, so no "comm" branch yet
                Expect.equal [ "comedians", "comics", "communism" ] keys
        , test "Debug full pipeline for Comics/Communism/Comedians" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "Comics"
                        , createTestAlbum "2" "Communism"
                        , createTestAlbum "3" "Comedians"
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    -- Get actual results for debugging
                    comicsResult =
                        Dict.get "1" finalKeybindings |> Maybe.withDefault "MISSING"

                    communismResult =
                        Dict.get "2" finalKeybindings |> Maybe.withDefault "MISSING"

                    comediansResult =
                        Dict.get "3" finalKeybindings |> Maybe.withDefault "MISSING"
                in
                -- Test the specific expected outputs
                Expect.all
                    [ \_ -> Expect.equal "comi" comicsResult
                    , \_ -> Expect.equal "comm" communismResult
                    , \_ -> Expect.equal "come" comediansResult
                    ]
                    ()
        , test "Debug Apple/Banana shortening" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "Apple"
                        , createTestAlbum "2" "Banana"
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    appleResult =
                        Dict.get "1" finalKeybindings |> Maybe.withDefault "MISSING"

                    bananaResult =
                        Dict.get "2" finalKeybindings |> Maybe.withDefault "MISSING"
                in
                -- Just check what we're actually getting
                Expect.all
                    [ \_ -> Expect.notEqual "MISSING" appleResult
                    , \_ -> Expect.notEqual "MISSING" bananaResult
                    , \_ -> Expect.notEqual appleResult bananaResult
                    ]
                    ()
        , test "Based/Based Test/Boss Took should get [ba, bte, bto]" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "Based"
                        , createTestAlbum "2" "Based Test"
                        , createTestAlbum "3" "Boss Took"
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    basedResult =
                        Dict.get "1" finalKeybindings |> Maybe.withDefault "MISSING"

                    basedTestResult =
                        Dict.get "2" finalKeybindings |> Maybe.withDefault "MISSING"

                    bossTookResult =
                        Dict.get "3" finalKeybindings |> Maybe.withDefault "MISSING"
                in
                Expect.all
                    [ \_ -> Expect.equal "ba" basedResult
                    , \_ -> Expect.equal "bte" basedTestResult
                    , \_ -> Expect.equal "bto" bossTookResult
                    ]
                    ()
        , test "Based alone should get b (optimal)" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "Based" ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    basedResult =
                        Dict.get "1" finalKeybindings |> Maybe.withDefault "MISSING"
                in
                Expect.equal "b" basedResult
        , test "Priority processing: unused branches don't interfere" <|
            \_ ->
                let
                    -- Recreate the actual scenario: Based Test has "basedtest" as 3rd preference
                    albums =
                        [ createTestAlbum "1" "Based" -- First pref: "based"
                        , createTestAlbum "2" "Based Test" -- First pref: "btest", 3rd pref: "basedtest"
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    basedResult =
                        Dict.get "1" finalKeybindings |> Maybe.withDefault "MISSING"

                    basedTestResult =
                        Dict.get "2" finalKeybindings |> Maybe.withDefault "MISSING"
                in
                -- With priority-based processing:
                -- Round 0: "based" vs "btest" (no conflict!) → both assigned
                -- "basedtest" (3rd pref) never processed, so no interference
                Expect.all
                    [ \_ -> Expect.atMost 2 (String.length basedResult) -- "based" should shorten to ~"ba" or "b"
                    , \_ -> Expect.atMost 4 (String.length basedTestResult) -- "btest" should shorten to ~"bte"
                    ]
                    ()
        , test "Ukraine/Unknown/Unexpected should get [uk, unk, une]" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "Ukraine"
                        , createTestAlbum "2" "Unknown"
                        , createTestAlbum "3" "Unexpected"
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    ukraineResult =
                        Dict.get "1" finalKeybindings |> Maybe.withDefault "MISSING"

                    unknownResult =
                        Dict.get "2" finalKeybindings |> Maybe.withDefault "MISSING"

                    unexpectedResult =
                        Dict.get "3" finalKeybindings |> Maybe.withDefault "MISSING"
                in
                -- Debug what we actually get
                Expect.all
                    [ \_ -> Expect.equal "uk" ukraineResult
                    , \_ -> Expect.equal "unk" unknownResult
                    , \_ -> Expect.equal "une" unexpectedResult
                    ]
                    ()
        , test "Minimal conflict test: Ukraine vs Unknown only" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "Ukraine" -- "ukraine" 7 chars
                        , createTestAlbum "2" "Unknown" -- "unknown" 7 chars
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    ukraineResult =
                        Dict.get "1" finalKeybindings |> Maybe.withDefault "MISSING"

                    unknownResult =
                        Dict.get "2" finalKeybindings |> Maybe.withDefault "MISSING"
                in
                -- Both start with "u" - neither should get just "u"
                Expect.all
                    [ \_ -> Expect.atLeast 2 (String.length ukraineResult) -- Should be at least 2 chars
                    , \_ -> Expect.atLeast 2 (String.length unknownResult) -- Should be at least 2 chars
                    , \_ -> Expect.notEqual ukraineResult unknownResult -- Should be different
                    ]
                    ()
        , test "Non-alphanumeric characters should be filtered out" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "Media - LotR" -- Should become "mlotr", not "m-lotr"
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    mediaResult =
                        Dict.get "1" finalKeybindings |> Maybe.withDefault "MISSING"
                in
                -- Debug what branches are generated for "Media - LotR"
                Expect.all
                    [ \_ -> Expect.notEqual "MISSING" mediaResult
                    , \_ -> Expect.equal False (String.contains "-" mediaResult) -- No hyphens
                    , \_ -> Expect.equal "m" mediaResult -- Optimal shortest result
                    ]
                    ()
        , test "Branch generation filters non-alphanumeric characters" <|
            \_ ->
                let
                    branches =
                        generateBranches "Media - LotR"
                in
                -- "Media - LotR" should become ["Media", "LotR"] -> ["mlotr", "melotr", "medialotr"]
                Expect.all
                    [ \_ -> Expect.atLeast 1 (List.length branches)
                    , \_ -> Expect.equal True (List.all (\branch -> not (String.contains "-" branch)) branches)
                    , \_ -> Expect.equal True (List.member "mlotr" branches) -- First branch should be "mlotr"
                    ]
                    ()
        , test "Single character albums get repeated character as first priority" <|
            \_ ->
                let
                    branches =
                        generateBranches "J"
                in
                -- "J" should generate ["jjjjjjjjjj"] as first priority
                Expect.all
                    [ \_ -> Expect.equal 1 (List.length branches) -- Should only have one branch
                    , \_ -> Expect.equal "jjjjjjjjjj" (List.head branches |> Maybe.withDefault "")
                    ]
                    ()
        , test "Single character album with conflicts shortens appropriately" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "J" -- Should get "jjjjjjjjjj" -> shortened as needed
                        , createTestAlbum "2" "Jazz" -- Should get "jazz" -> shortened to avoid conflicts
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    jResult =
                        Dict.get "1" finalKeybindings |> Maybe.withDefault "MISSING"

                    jazzResult =
                        Dict.get "2" finalKeybindings |> Maybe.withDefault "MISSING"
                in
                -- J should get multiple j's shortened to avoid conflict with jazz
                Expect.all
                    [ \_ -> Expect.notEqual "MISSING" jResult
                    , \_ -> Expect.notEqual "MISSING" jazzResult
                    , \_ -> Expect.notEqual jResult jazzResult -- Should be different
                    , \_ -> Expect.equal True (String.startsWith "j" jResult) -- J result should start with j
                    , \_ -> Expect.atLeast 2 (String.length jResult) -- Should be multiple j's to avoid conflict
                    ]
                    ()
        , test "Multiple single characters with other albums" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "J" -- "jjjjjjjjjj"
                        , createTestAlbum "2" "K" -- "kkkkkkkkkk"
                        , createTestAlbum "3" "Jazz" -- "jazz"
                        , createTestAlbum "4" "Kitchen" -- "kitchen"
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    jResult =
                        Dict.get "1" finalKeybindings |> Maybe.withDefault "MISSING"

                    kResult =
                        Dict.get "2" finalKeybindings |> Maybe.withDefault "MISSING"

                    jazzResult =
                        Dict.get "3" finalKeybindings |> Maybe.withDefault "MISSING"

                    kitchenResult =
                        Dict.get "4" finalKeybindings |> Maybe.withDefault "MISSING"
                in
                -- All should be assigned and unique
                Expect.all
                    [ \_ -> Expect.notEqual "MISSING" jResult
                    , \_ -> Expect.notEqual "MISSING" kResult
                    , \_ -> Expect.notEqual "MISSING" jazzResult
                    , \_ -> Expect.notEqual "MISSING" kitchenResult

                    -- All should be different
                    , \_ -> Expect.equal 4 (List.length (removeDuplicates [ jResult, kResult, jazzResult, kitchenResult ]))
                    ]
                    ()
        , test "Iterative shortening removes longest strings first" <|
            \_ ->
                let
                    -- Test with Comics/Communism/Comedians - should get ["comi", "comm", "come"]
                    albums =
                        [ createTestAlbum "1" "Comics"
                        , createTestAlbum "2" "Communism"
                        , createTestAlbum "3" "Comedians"
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    comicsBinding =
                        Dict.get "1" finalKeybindings

                    communismBinding =
                        Dict.get "2" finalKeybindings

                    comediansBinding =
                        Dict.get "3" finalKeybindings
                in
                case ( comicsBinding, communismBinding, comediansBinding ) of
                    ( Just comics, Just communism, Just comedians ) ->
                        -- Let's see what the algorithm actually produces first
                        Expect.all
                            [ \_ -> Expect.equal "comi" comics
                            , \_ -> Expect.equal "comm" communism
                            , \_ -> Expect.equal "come" comedians
                            ]
                            ()

                    _ ->
                        Expect.fail "All albums should have keybindings"
        , test "Basic shortening works without conflicts" <|
            \_ ->
                let
                    -- Test with non-conflicting albums that should all get keybinds
                    albums =
                        [ createTestAlbum "1" "Apple"
                        , createTestAlbum "2" "Banana"
                        , createTestAlbum "3" "Cherry"
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    appleBinding =
                        Dict.get "1" finalKeybindings

                    bananaBinding =
                        Dict.get "2" finalKeybindings

                    cherryBinding =
                        Dict.get "3" finalKeybindings

                    allBindings =
                        [ appleBinding, bananaBinding, cherryBinding ]

                    allPresent =
                        List.all (\binding -> binding /= Nothing) allBindings
                in
                if allPresent then
                    let
                        actualBindings =
                            List.filterMap identity allBindings

                        allUnique =
                            List.length actualBindings == List.length (removeDuplicates actualBindings)

                        noPrefixConflicts =
                            not (hasPrefixConflicts actualBindings)
                    in
                    Expect.all
                        [ \_ -> Expect.equal True allUnique
                        , \_ -> Expect.equal True noPrefixConflicts
                        ]
                        ()

                else
                    Expect.fail "All albums should have keybindings"
        ]


integrationTests : Test
integrationTests =
    describe "Integration Tests"
        [ test "Complex real-world album set has no conflicts" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "Tech"
                        , createTestAlbum "2" "Technology"
                        , createTestAlbum "3" "The World"
                        , createTestAlbum "4" "To Be Sorted"
                        , createTestAlbum "5" "Christianity"
                        , createTestAlbum "6" "Christmas"
                        , createTestAlbum "7" "Comics"
                        , createTestAlbum "8" "Communism"
                        , createTestAlbum "9" "General"
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    allKeybindings =
                        Dict.values finalKeybindings

                    -- Check no duplicates
                    noDuplicates =
                        List.length allKeybindings == List.length (removeDuplicates allKeybindings)

                    -- Check no prefix conflicts
                    noPrefixConflicts =
                        not (hasPrefixConflicts allKeybindings)

                    -- Check all albums got keybindings
                    allAlbumsHaveKeybindings =
                        List.length allKeybindings == List.length albums
                in
                Expect.all
                    [ \_ -> Expect.equal True noDuplicates
                    , \_ -> Expect.equal True noPrefixConflicts
                    , \_ -> Expect.equal True allAlbumsHaveKeybindings
                    ]
                    ()
        , test "Each album gets the shortest valid keybind" <|
            \_ ->
                let
                    albums =
                        [ createTestAlbum "1" "Apple"
                        , createTestAlbum "2" "Banana"
                        ]

                    finalKeybindings =
                        generateAlbumKeybindings albums

                    appleBinding =
                        Dict.get "1" finalKeybindings

                    bananaBinding =
                        Dict.get "2" finalKeybindings
                in
                case ( appleBinding, bananaBinding ) of
                    ( Just apple, Just banana ) ->
                        -- The algorithm should shorten to optimal length
                        -- Apple/Banana should get the shortest non-conflicting keybinds: "a" and "b"
                        Expect.all
                            [ \_ -> Expect.equal "a" apple
                            , \_ -> Expect.equal "b" banana
                            , \_ -> Expect.notEqual apple banana
                            ]
                            ()

                    _ ->
                        Expect.fail "Both albums should have keybindings"
        ]


fuzzTests : Test
fuzzTests =
    describe "Fuzz Tests"
        [ fuzz testAlbumGenerator "keybindings are lowercase alphanumeric" <|
            \album ->
                let
                    keybindings =
                        generateAlbumKeybindings [ album ]

                    allKeys =
                        Dict.values keybindings
                in
                allKeys
                    |> List.all (\k -> String.all Char.isAlphaNum k && String.toLower k == k)
                    |> Expect.equal True
        , fuzz (Fuzz.listOfLengthBetween 0 5 testAlbumGenerator) "generateAlbumKeybindings is deterministic for any input" <|
            \albums ->
                let
                    first =
                        generateAlbumKeybindings albums

                    second =
                        generateAlbumKeybindings albums
                in
                Expect.equal first second
        ]



-- Helper functions


removeDuplicates : List String -> List String
removeDuplicates list =
    list
        |> List.foldl
            (\item acc ->
                if List.member item acc then
                    acc

                else
                    item :: acc
            )
            []
        |> List.reverse


hasPrefixConflicts : List String -> Bool
hasPrefixConflicts keybindings =
    List.any
        (\kb1 ->
            List.any
                (\kb2 ->
                    kb1 /= kb2 && (String.startsWith kb1 kb2 || String.startsWith kb2 kb1)
                )
                keybindings
        )
        keybindings
