module KeybindBranches exposing (createBranchDictionary, generateAlbumKeybindings, generateBranches)

import Dict exposing (Dict)
import Immich exposing (ImmichAlbum, ImmichAlbumId)
import Regex




generateBranches : String -> List String
generateBranches albumName =
    let
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
            if String.length singleWord == 1 then
                [ String.repeat 10 singleWord ]

            else
                [ singleWord ]

        multipleWords ->
            case multipleWords of
                [] ->
                    []

                [ word1, word2 ] ->
                    List.range 1 (String.length word1)
                        |> List.map (\prefixLen -> String.left prefixLen word1 ++ word2)

                firstWord :: restWords ->
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

                                middlePart =
                                    if firstWordPrefixLen == 1 then
                                        middleWords |> List.map (String.left 1) |> String.join ""

                                    else if firstWordPrefixLen == 2 then
                                        case middleWords of
                                            [ singleMiddle ] ->
                                                String.left 2 singleMiddle

                                            multipleMiddle ->
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
                                        middleWords |> String.join ""
                            in
                            firstPart ++ middlePart ++ lastWord
                    in
                    List.range 1 (String.length firstWord)
                        |> List.map generateBranch




createBranchDictionary : List ImmichAlbum -> Dict String ( ImmichAlbumId, Int )
createBranchDictionary albums =
    let
        allBranches =
            List.concatMap
                (\album ->
                    List.indexedMap (\priority branch -> ( branch, ( album.id, priority ) )) (generateBranches album.albumName)
                )
                albums

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

        nonConflictingBranches =
            Dict.filter (\_ albumData -> List.length albumData == 1) groupedBranches

        finalDict =
            Dict.map
                (\_ albumData ->
                    case albumData of
                        [ singleData ] ->
                            singleData

                        _ ->
                            ( "", -1 )
                )
                nonConflictingBranches
    in
    finalDict

resolvePrefixConflicts : Dict String ( ImmichAlbumId, Int ) -> Dict String ( ImmichAlbumId, Int )
resolvePrefixConflicts branchDict =
     let
        allBranches =
             Dict.keys branchDict

        findPrefixConflicts : List String -> List ( String, String )
        findPrefixConflicts branches =
            List.concatMap
                (\b1 ->
                    List.filterMap
                        (\b2 ->
                            if b1 /= b2 && String.startsWith b1 b2 then
                                case ( Dict.get b1 branchDict, Dict.get b2 branchDict ) of
                                    ( Just ( albumId1, _ ), Just ( albumId2, _ ) ) ->
                                        if albumId1 /= albumId2 then
                                            Just ( b1, b2 )

                                        else
                                            Nothing

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

        conflictingPrefixes =
            List.map Tuple.first conflicts |> removeDuplicates

        resolveConflict : String -> String
        resolveConflict branch =
            if List.member branch conflictingPrefixes then
                branch ++ "1"

            else
                branch

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




iterativeShortening : Dict String ( ImmichAlbumId, Int ) -> Dict String ( ImmichAlbumId, Int )
iterativeShortening branchDict =
    let
        initialPairs =
            Dict.toList branchDict

        shortenWithTwoPiles : List ( String, ( ImmichAlbumId, Int ) ) -> List ( String, ( ImmichAlbumId, Int ) ) -> List ( String, ( ImmichAlbumId, Int ) )
        shortenWithTwoPiles workingPairs finalizedPairs =
            case workingPairs of
                [] ->
                    finalizedPairs

                _ ->
                    let
                        maxLength =
                            List.map (\( kb, _ ) -> String.length kb) workingPairs
                                |> List.maximum
                                |> Maybe.withDefault 0

                        ( longestPairs, shorterPairs ) =
                            List.partition (\( kb, _ ) -> String.length kb == maxLength) workingPairs

                        conflictedOriginals =
                            List.filterMap
                                (\( originalKb, ( albumId, priority ) ) ->
                                    let
                                        shortenedKb =
                                            if String.length originalKb > 1 then
                                                String.dropRight 1 originalKb

                                            else
                                                originalKb

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
                                            List.member shortenedKb allOtherKbs
                                                || 
                                                   List.any
                                                    (\otherKb ->
                                                        (String.startsWith shortenedKb otherKb && shortenedKb /= otherKb)
                                                            || (String.startsWith otherKb shortenedKb && otherKb /= shortenedKb)
                                                    )
                                                    allOtherKbs
                                    in
                                    if hasConflict then
                                        Just ( originalKb, ( albumId, priority ) )

                                    else
                                        Nothing
                                )
                                longestPairs

                        nonConflictedOriginals =
                            List.filter
                                (\originalPair ->
                                    not (List.member originalPair conflictedOriginals)
                                )
                                longestPairs

                        newWorkingPairs =
                            List.map
                                (\( kb, ( albumId, priority ) ) ->
                                    if String.length kb > 1 then
                                        ( String.dropRight 1 kb, ( albumId, priority ) )

                                    else
                                        ( kb, ( albumId, priority ) )
                                )
                                nonConflictedOriginals

                        updatedWorkingPairs =
                            newWorkingPairs ++ shorterPairs

                        updatedFinalizedPairs =
                            finalizedPairs ++ conflictedOriginals
                    in
                    if maxLength <= 1 || List.isEmpty longestPairs then
                        finalizedPairs ++ workingPairs

                    else
                        shortenWithTwoPiles updatedWorkingPairs updatedFinalizedPairs
    in
    shortenWithTwoPiles initialPairs []
        |> Dict.fromList





assignFinalKeybinds : List ImmichAlbum -> Dict String ( ImmichAlbumId, Int ) -> Dict ImmichAlbumId String
assignFinalKeybinds albums finishedDict =
    let
        keybindList =
            Dict.toList finishedDict

        assignKeybind : ( String, ( ImmichAlbumId, Int ) ) -> Dict ImmichAlbumId ( String, Int ) -> Dict ImmichAlbumId ( String, Int )
        assignKeybind ( keybind, ( albumId, priority ) ) acc =
            case Dict.get albumId acc of
                Nothing ->
                    Dict.insert albumId ( keybind, priority ) acc

                Just ( existingKeybind, existingPriority ) ->
                    if priority < existingPriority then
                        Dict.insert albumId ( keybind, priority ) acc

                    else
                        acc

        albumKeybindMap =
            List.foldl assignKeybind Dict.empty keybindList
    in
    Dict.map (\_ ( keybind, _ ) -> keybind) albumKeybindMap





processPriorityRound : Int -> List ImmichAlbum -> ( Dict ImmichAlbumId String, List ImmichAlbum )
processPriorityRound priority albums =
    let
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

        resolvedDict =
            resolvePrefixConflicts priorityBranchDict

        shortenedDict =
            iterativeShortening resolvedDict

        assignedKeybindings =
            assignFinalKeybinds albums shortenedDict

        assignedAlbumIds =
            Dict.keys assignedKeybindings

        remainingAlbums =
            List.filter (\album -> not (List.member album.id assignedAlbumIds)) albums
    in
    ( assignedKeybindings, remainingAlbums )





generateAlbumKeybindings : List ImmichAlbum -> Dict ImmichAlbumId String
generateAlbumKeybindings albums =
    let
        processAllPriorities : Int -> List ImmichAlbum -> Dict ImmichAlbumId String -> Dict ImmichAlbumId String
        processAllPriorities currentPriority remainingAlbums accumulatedKeybindings =
            if List.isEmpty remainingAlbums || currentPriority > 10 then
                accumulatedKeybindings

            else
                let
                    ( newKeybindings, stillRemainingAlbums ) =
                        processPriorityRound currentPriority remainingAlbums

                    updatedKeybindings =
                        Dict.union newKeybindings accumulatedKeybindings
                in
                if List.length stillRemainingAlbums == List.length remainingAlbums then
                    processAllPriorities (currentPriority + 1) stillRemainingAlbums updatedKeybindings

                else
                    processAllPriorities (currentPriority + 1) stillRemainingAlbums updatedKeybindings
    in
    processAllPriorities 0 albums Dict.empty
