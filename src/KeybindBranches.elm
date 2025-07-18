module KeybindBranches exposing (createBranchDictionary, generateAlbumKeybindings, generateBranches)

import Dict exposing (Dict)
import Immich exposing (ImmichAlbum, ImmichAlbumId)
import Regex



-- Generate keybind branches for an album name following the user's methodology


generateBranches : String -> List String
generateBranches albumName =
    let
        -- Remove non-alphanumeric characters using regex
        nonAlphanumericRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "[^a-zA-Z0-9\\s]"

        cleanName =
            String.toLower (String.trim albumName)
                |> Regex.replace nonAlphanumericRegex (\_ -> "")
                |> String.trim

        words =
            String.words cleanName |> List.filter (not << String.isEmpty)
    in
    case words of
        [] ->
            []

        [ singleWord ] ->
            -- Single word logic
            if String.length singleWord == 1 then
                -- Single character: repeat 10 times for conflict resolution flexibility
                [ String.repeat 10 singleWord ]

            else
                -- Regular single word: only full word for long words
                [ singleWord ]

        multipleWords ->
            -- Multi-word logic: progressive branches
            -- "The World": ["tworld", "thworld", "theworld"]
            -- "To Be Sorted": ["tbsorted", "tbesorted", "tobesorted"]
            case multipleWords of
                [] ->
                    []

                [ word1, word2 ] ->
                    -- Two words: progressive prefixes of first word + second word
                    -- "The World": t+world, th+world, the+world
                    List.range 1 (String.length word1)
                        |> List.map (\prefixLen -> String.left prefixLen word1 ++ word2)

                firstWord :: restWords ->
                    -- Three or more words: progressive expansion
                    -- "To Be Sorted": ["tbsorted", "tbesorted", "tobesorted"]
                    -- Start with first letter + first letters of middle + last word
                    -- Then progressively expand the first word while keeping pattern
                    let
                        lastWord =
                            List.reverse restWords |> List.head |> Maybe.withDefault ""

                        middleWords =
                            List.reverse restWords |> List.drop 1 |> List.reverse

                        generateBranch : Int -> String
                        generateBranch firstWordPrefixLen =
                            let
                                firstPart =
                                    String.left firstWordPrefixLen firstWord

                                -- Progressive expansion of middle words
                                middlePart =
                                    if firstWordPrefixLen == 1 then
                                        -- First iteration: first letters only -> "tbsorted"
                                        middleWords |> List.map (String.left 1) |> String.join ""

                                    else if firstWordPrefixLen == 2 then
                                        -- Second iteration: partial expansion -> "tbesorted"
                                        -- Take first letters of early middle words, more of later ones
                                        case middleWords of
                                            [ singleMiddle ] ->
                                                String.left 2 singleMiddle

                                            multipleMiddle ->
                                                -- For multiple middle words, progressive expansion
                                                List.indexedMap
                                                    (\i word ->
                                                        if i < List.length multipleMiddle - 1 then
                                                            String.left 1 word

                                                        else
                                                            String.left 2 word
                                                    )
                                                    multipleMiddle
                                                    |> String.join ""

                                    else
                                        -- Later iterations: full middle words
                                        middleWords |> String.join ""
                            in
                            firstPart ++ middlePart ++ lastWord
                    in
                    List.range 1 (String.length firstWord)
                        |> List.map generateBranch



-- Create a branch dictionary, removing conflicts entirely


createBranchDictionary : List ImmichAlbum -> Dict String ( ImmichAlbumId, Int )
createBranchDictionary albums =
    let
        -- Generate all branches for all albums with preference priority
        allBranches =
            List.concatMap
                (\album ->
                    List.indexedMap (\priority branch -> ( branch, ( album.id, priority ) )) (generateBranches album.albumName)
                )
                albums

        -- Group branches by string to detect conflicts
        groupedBranches =
            List.foldl
                (\( branch, ( albumId, priority ) ) acc ->
                    Dict.update branch
                        (\maybeList ->
                            case maybeList of
                                Nothing ->
                                    Just [ ( albumId, priority ) ]

                                Just list ->
                                    Just (( albumId, priority ) :: list)
                        )
                        acc
                )
                Dict.empty
                allBranches

        -- Keep only branches with no conflicts (single album)
        nonConflictingBranches =
            Dict.filter (\_ albumData -> List.length albumData == 1) groupedBranches

        -- Convert back to Dict String (ImmichAlbumId, Int)
        finalDict =
            Dict.map
                (\_ albumData ->
                    case albumData of
                        [ singleData ] ->
                            singleData

                        _ ->
                            ( "", -1 )
                 -- This shouldn't happen due to filtering above
                )
                nonConflictingBranches
    in
    finalDict



-- Resolve prefix conflicts using numbering system


resolvePrefixConflicts : Dict String ( ImmichAlbumId, Int ) -> Dict String ( ImmichAlbumId, Int )
resolvePrefixConflicts branchDict =
    let
        allBranches =
            Dict.keys branchDict

        -- Find conflicts where one branch is a prefix of another from DIFFERENT albums
        findPrefixConflicts : List String -> List ( String, String )
        findPrefixConflicts branches =
            List.concatMap
                (\b1 ->
                    List.filterMap
                        (\b2 ->
                            if b1 /= b2 && String.startsWith b1 b2 then
                                -- Check if these branches belong to different albums
                                case ( Dict.get b1 branchDict, Dict.get b2 branchDict ) of
                                    ( Just ( albumId1, _ ), Just ( albumId2, _ ) ) ->
                                        if albumId1 /= albumId2 then
                                            Just ( b1, b2 )

                                        else
                                            Nothing

                                    -- Same album, no conflict
                                    _ ->
                                        Nothing

                            else
                                Nothing
                        )
                        branches
                )
                branches

        conflicts =
            findPrefixConflicts allBranches

        -- Get all shorter branches that are prefixes (these need numbering)
        conflictingPrefixes =
            List.map Tuple.first conflicts |> removeDuplicates

        -- Add number suffix to conflicting prefixes
        resolveConflict : String -> String
        resolveConflict branch =
            if List.member branch conflictingPrefixes then
                branch ++ "1"

            else
                branch

        -- Apply conflict resolution
        resolvedDict =
            Dict.foldl
                (\branch ( albumId, priority ) acc ->
                    let
                        resolvedBranch =
                            resolveConflict branch
                    in
                    Dict.insert resolvedBranch ( albumId, priority ) acc
                )
                Dict.empty
                branchDict
    in
    resolvedDict



-- Remove duplicates from a list


removeDuplicates : List String -> List String
removeDuplicates list =
    List.foldl
        (\item acc ->
            if List.member item acc then
                acc

            else
                item :: acc
        )
        []
        list
        |> List.reverse



-- Iterative shortening with recursive character removal
-- Following user specification: "remove a single character from the end of one of the entries"
-- "starting with the longest strings first"


iterativeShortening : Dict String ( ImmichAlbumId, Int ) -> Dict String ( ImmichAlbumId, Int )
iterativeShortening branchDict =
    let
        initialPairs =
            Dict.toList branchDict

        -- Recursive function that implements the longest-first shortening strategy
        shortenWithTwoPiles : List ( String, ( ImmichAlbumId, Int ) ) -> List ( String, ( ImmichAlbumId, Int ) ) -> List ( String, ( ImmichAlbumId, Int ) )
        shortenWithTwoPiles workingPairs finalizedPairs =
            case workingPairs of
                [] ->
                    -- All pairs have been processed
                    finalizedPairs

                _ ->
                    let
                        -- Find the maximum length among working pairs
                        maxLength =
                            List.map (\( kb, _ ) -> String.length kb) workingPairs
                                |> List.maximum
                                |> Maybe.withDefault 0

                        -- Split pairs into longest and shorter
                        ( longestPairs, shorterPairs ) =
                            List.partition (\( kb, _ ) -> String.length kb == maxLength) workingPairs

                        -- Try shortening only the longest strings
                        shortenedLongest =
                            List.map
                                (\( kb, ( albumId, priority ) ) ->
                                    if String.length kb > 1 then
                                        ( String.dropRight 1 kb, ( albumId, priority ) )

                                    else
                                        ( kb, ( albumId, priority ) )
                                 -- Can't shorten further
                                )
                                longestPairs

                        -- Check for conflicts among shortened strings
                        shortenedKeybinds =
                            List.map Tuple.first shortenedLongest

                        -- Find which shortened strings have conflicts
                        conflictedOriginals =
                            List.filterMap
                                (\( originalKb, ( albumId, priority ) ) ->
                                    let
                                        shortenedKb =
                                            if String.length originalKb > 1 then
                                                String.dropRight 1 originalKb

                                            else
                                                originalKb

                                        -- Check if this shortened kb conflicts with:
                                        -- 1. Other shortened strings from same iteration
                                        -- 2. Existing shorter strings from previous iterations
                                        -- 3. All strings currently in finalized pile
                                        otherShortenedKbs =
                                            List.filterMap
                                                (\( otherOriginalKb, _ ) ->
                                                    if otherOriginalKb == originalKb then
                                                        Nothing

                                                    else
                                                        Just
                                                            (if String.length otherOriginalKb > 1 then
                                                                String.dropRight 1 otherOriginalKb

                                                             else
                                                                otherOriginalKb
                                                            )
                                                )
                                                longestPairs

                                        existingShorterKbs =
                                            List.map Tuple.first shorterPairs

                                        finalizedKbs =
                                            List.map Tuple.first finalizedPairs

                                        allOtherKbs =
                                            otherShortenedKbs ++ existingShorterKbs ++ finalizedKbs

                                        hasConflict =
                                            -- Duplicate check
                                            List.member shortenedKb allOtherKbs
                                                || -- Prefix conflicts
                                                   List.any
                                                    (\otherKb ->
                                                        (String.startsWith shortenedKb otherKb && shortenedKb /= otherKb)
                                                            || (String.startsWith otherKb shortenedKb && otherKb /= shortenedKb)
                                                    )
                                                    allOtherKbs
                                    in
                                    if hasConflict then
                                        Just ( originalKb, ( albumId, priority ) )
                                        -- Move ORIGINAL to finalized

                                    else
                                        Nothing
                                )
                                longestPairs

                        -- Get original pairs that had no conflicts when shortened
                        nonConflictedOriginals =
                            List.filter
                                (\originalPair ->
                                    not (List.member originalPair conflictedOriginals)
                                )
                                longestPairs

                        -- Convert non-conflicted originals to their shortened versions
                        newWorkingPairs =
                            List.map
                                (\( kb, ( albumId, priority ) ) ->
                                    if String.length kb > 1 then
                                        ( String.dropRight 1 kb, ( albumId, priority ) )

                                    else
                                        ( kb, ( albumId, priority ) )
                                )
                                nonConflictedOriginals

                        -- Add shorter pairs back to working pile
                        updatedWorkingPairs =
                            newWorkingPairs ++ shorterPairs

                        -- Add conflicted originals to finalized pile
                        updatedFinalizedPairs =
                            finalizedPairs ++ conflictedOriginals
                    in
                    if maxLength <= 1 || List.isEmpty longestPairs then
                        -- All remaining strings are 1 character or no longest pairs to process
                        finalizedPairs ++ workingPairs

                    else
                        shortenWithTwoPiles updatedWorkingPairs updatedFinalizedPairs
    in
    shortenWithTwoPiles initialPairs []
        |> Dict.fromList



-- Assign final keybinds: each album gets shortest valid keybind from its original branches


assignFinalKeybinds : List ImmichAlbum -> Dict String ( ImmichAlbumId, Int ) -> Dict ImmichAlbumId String
assignFinalKeybinds albums finishedDict =
    let
        -- Convert to list and process each keybind with preference
        keybindList =
            Dict.toList finishedDict

        -- Apply keybinds in order, using preference to decide conflicts
        assignKeybind : ( String, ( ImmichAlbumId, Int ) ) -> Dict ImmichAlbumId ( String, Int ) -> Dict ImmichAlbumId ( String, Int )
        assignKeybind ( keybind, ( albumId, priority ) ) acc =
            case Dict.get albumId acc of
                Nothing ->
                    -- Album doesn't have a keybind yet, assign this one
                    Dict.insert albumId ( keybind, priority ) acc

                Just ( existingKeybind, existingPriority ) ->
                    -- Album already has a keybind, check if this one is better (lower priority)
                    if priority < existingPriority then
                        Dict.insert albumId ( keybind, priority ) acc

                    else
                        acc

        -- Keep the existing better keybind
        -- Process all keybinds
        albumKeybindMap =
            List.foldl assignKeybind Dict.empty keybindList
    in
    -- Extract just the keybind strings
    Dict.map (\_ ( keybind, _ ) -> keybind) albumKeybindMap



-- Stage 2: Add optional short forms for long single words if no conflicts


addOptionalShortForms : List ImmichAlbum -> Dict ImmichAlbumId String -> Dict ImmichAlbumId String
addOptionalShortForms albums currentKeybindings =
    let
        -- Get current keybinds that are assigned
        assignedKeybinds =
            Dict.values currentKeybindings

        -- For each album with a long keybind (> 4 chars), try adding short form
        tryAddShortForm : ImmichAlbum -> Dict ImmichAlbumId String -> Dict ImmichAlbumId String
        tryAddShortForm album acc =
            case Dict.get album.id acc of
                Nothing ->
                    acc

                -- Album has no keybind
                Just currentKeybind ->
                    -- Check if this is a single word > 4 chars
                    let
                        cleanName =
                            String.toLower (String.trim album.albumName)

                        words =
                            String.words cleanName |> List.filter (not << String.isEmpty)

                        currentKeybindLength =
                            String.length currentKeybind
                    in
                    case words of
                        [ singleWord ] ->
                            if String.length singleWord > 4 && currentKeybindLength > 4 then
                                -- Generate short form
                                let
                                    shortForm =
                                        String.left 3 singleWord ++ String.right 1 singleWord

                                    allCurrentKeybinds =
                                        Dict.values acc

                                    -- Check for conflicts
                                    hasExactConflict =
                                        List.member shortForm allCurrentKeybinds

                                    hasPrefixConflict =
                                        List.any
                                            (\kb ->
                                                (String.startsWith shortForm kb && shortForm /= kb)
                                                    || (String.startsWith kb shortForm && kb /= shortForm)
                                            )
                                            allCurrentKeybinds
                                in
                                if not hasExactConflict && not hasPrefixConflict && String.length shortForm < currentKeybindLength then
                                    -- Safe to use short form and it's actually shorter
                                    Dict.insert album.id shortForm acc

                                else
                                    acc
                                -- Keep current keybind

                            else
                                acc

                        -- Not applicable
                        _ ->
                            acc

        -- Not a single word
    in
    List.foldl tryAddShortForm currentKeybindings albums



-- Process a single priority round for a subset of albums


processPriorityRound : Int -> List ImmichAlbum -> ( Dict ImmichAlbumId String, List ImmichAlbum )
processPriorityRound priority albums =
    let
        -- Create branch dictionary for this priority level only
        priorityBranchDict =
            albums
                |> List.concatMap
                    (\album ->
                        generateBranches album.albumName
                            |> List.indexedMap (\prio branch -> ( branch, ( album.id, prio ) ))
                            |> List.filter (\( _, ( _, prio ) ) -> prio == priority)
                    )
                |> List.foldl
                    (\( branch, ( albumId, prio ) ) acc ->
                        Dict.insert branch ( albumId, prio ) acc
                    )
                    Dict.empty

        -- Apply the standard algorithm to this priority level
        resolvedDict =
            resolvePrefixConflicts priorityBranchDict

        shortenedDict =
            iterativeShortening resolvedDict

        assignedKeybindings =
            assignFinalKeybinds albums shortenedDict

        -- Find which albums got assigned in this round
        assignedAlbumIds =
            Dict.keys assignedKeybindings

        -- Filter out assigned albums for next round
        remainingAlbums =
            List.filter (\album -> not (List.member album.id assignedAlbumIds)) albums
    in
    ( assignedKeybindings, remainingAlbums )



-- Main function that matches the original interface


generateAlbumKeybindings : List ImmichAlbum -> Dict ImmichAlbumId String
generateAlbumKeybindings albums =
    let
        -- Priority-based processing: process preference levels in order
        processAllPriorities : Int -> List ImmichAlbum -> Dict ImmichAlbumId String -> Dict ImmichAlbumId String
        processAllPriorities currentPriority remainingAlbums accumulatedKeybindings =
            if List.isEmpty remainingAlbums || currentPriority > 10 then
                -- All albums assigned or we've tried enough priorities
                accumulatedKeybindings

            else
                let
                    ( newKeybindings, stillRemainingAlbums ) =
                        processPriorityRound currentPriority remainingAlbums

                    -- Merge new keybindings with accumulated ones
                    updatedKeybindings =
                        Dict.union newKeybindings accumulatedKeybindings
                in
                if List.length stillRemainingAlbums == List.length remainingAlbums then
                    -- No progress made this round, try next priority
                    processAllPriorities (currentPriority + 1) stillRemainingAlbums updatedKeybindings

                else
                    -- Progress made, continue with next priority for remaining albums
                    processAllPriorities (currentPriority + 1) stillRemainingAlbums updatedKeybindings
    in
    processAllPriorities 0 albums Dict.empty
