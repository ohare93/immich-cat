module KeybindingGenerator exposing 
    ( generateAlbumKeybindings
    , generateSimpleCandidates
    , assignKeybindingsInOrder
    , findFirstAvailableNonPrefix
    , wouldCreatePrefixConflict
    , generateTwoCharVariants
    , generateRepeatingVariants
    , isValidKeybinding
    , removeDuplicates
    , smartSortAlbums
    , hasRepeatedChars
    , compareByLengthAndQuality
    , optimizeKeybindingsToShorter
    )

import Dict exposing (Dict)
import Immich exposing (ImmichAlbum, ImmichAlbumId)
import Regex


-- Main keybinding generation function
generateAlbumKeybindings : List ImmichAlbum -> Dict ImmichAlbumId String
generateAlbumKeybindings albums =
    let
        albumCandidates = List.map (\album -> (album.id, generateSimpleCandidates album.albumName)) albums
        -- Smart sorting: avoid single letters when they would block longer meaningful names
        sortedAlbumCandidates = smartSortAlbums albumCandidates
        resolvedKeybindings = assignKeybindingsInOrder sortedAlbumCandidates
        -- Post-process to upgrade keybindings when safe
        optimizedKeybindings = optimizeKeybindingsToShorter albums resolvedKeybindings
    in
    Dict.fromList optimizedKeybindings


-- Smart sorting to avoid single letters blocking longer meaningful names
smartSortAlbums : List (ImmichAlbumId, List String) -> List (ImmichAlbumId, List String)
smartSortAlbums albumCandidates =
    let
        -- Check if a single-letter candidate would block longer albums
        wouldBlockOthers : (ImmichAlbumId, List String) -> Bool
        wouldBlockOthers (albumId, candidates) =
            case candidates of
                singleChar :: _ ->
                    if String.length singleChar == 1 then
                        -- Check if other albums have candidates starting with this letter
                        albumCandidates
                        |> List.filter (\(otherId, _) -> otherId /= albumId)
                        |> List.any (\(_, otherCandidates) ->
                            List.any (\candidate -> String.startsWith singleChar candidate && String.length candidate > 1) otherCandidates
                        )
                    else
                        False
                [] -> False
        
        -- Separate albums that would block others vs those that wouldn't
        (blockingAlbums, nonBlockingAlbums) = List.partition wouldBlockOthers albumCandidates
        
        -- Sort non-blocking albums first (by shortest candidate), then blocking albums
        sortedNonBlocking = 
            List.sortBy (\(_, candidates) -> 
                case candidates of
                    [] -> 999
                    shortest :: _ -> String.length shortest
            ) nonBlockingAlbums
        
        sortedBlocking = 
            List.sortBy (\(_, candidates) -> 
                case candidates of
                    [] -> 999
                    shortest :: _ -> String.length shortest
            ) blockingAlbums
    in
    sortedNonBlocking ++ sortedBlocking


assignKeybindingsInOrder : List (ImmichAlbumId, List String) -> List (ImmichAlbumId, String)
assignKeybindingsInOrder albumCandidates =
    let
        assignHelper remaining used =
            case remaining of
                [] -> []
                (albumId, candidates) :: rest ->
                    case findFirstAvailableNonPrefix candidates used of
                        Just keybinding ->
                            let newUsed = Dict.insert keybinding True used
                            in (albumId, keybinding) :: assignHelper rest newUsed
                        Nothing ->
                            let fallback = String.left 3 albumId
                                newUsed = Dict.insert fallback True used
                            in (albumId, fallback) :: assignHelper rest newUsed
    in
    assignHelper albumCandidates Dict.empty


findFirstAvailableNonPrefix : List String -> Dict String Bool -> Maybe String
findFirstAvailableNonPrefix candidates used =
    case candidates of
        [] -> Nothing
        candidate :: rest ->
            if Dict.member candidate used then
                findFirstAvailableNonPrefix rest used
            else if wouldCreatePrefixConflict candidate used then
                findFirstAvailableNonPrefix rest used
            else
                Just candidate


wouldCreatePrefixConflict : String -> Dict String Bool -> Bool
wouldCreatePrefixConflict candidate used =
    let
        usedKeybindings = Dict.keys used
        
        existingIsPrefixOfCandidate = 
            List.any (\existingKb -> 
                String.startsWith existingKb candidate && existingKb /= candidate
            ) usedKeybindings
        
        candidateIsPrefixOfExisting = 
            List.any (\existingKb -> 
                String.startsWith candidate existingKb && candidate /= existingKb
            ) usedKeybindings
    in
    existingIsPrefixOfCandidate || candidateIsPrefixOfExisting


generateSimpleCandidates : String -> List String
generateSimpleCandidates albumName =
    let
        cleanName = String.toLower (String.trim albumName)
        words = String.split " " cleanName |> List.filter (not << String.isEmpty)
        
        firstChar = String.left 1 cleanName
        firstTwoChars = String.left 2 cleanName
        firstThreeChars = String.left 3 cleanName
        
        wordAbbrev = words |> List.map (String.left 1) |> String.join ""
        twoCharVariants = generateTwoCharVariants cleanName
        repeatingVariants = generateRepeatingVariants cleanName
        
        allCandidates = 
            if String.length cleanName <= 2 then
                repeatingVariants ++       
                [ wordAbbrev ] ++          
                twoCharVariants            
            else if List.length words == 1 then
                -- For single words, prioritize 2-char prefix over single letter to avoid conflicts
                [ firstTwoChars ] ++       -- "general" -> "ge" gets priority 
                twoCharVariants ++         
                [ firstThreeChars ] ++     
                [ String.left 4 cleanName ] ++ 
                [ firstChar ] ++           -- Single letter as backup
                [ wordAbbrev ]             -- word abbreviation (same as firstChar for single words)              
            else
                -- For multi-word names, prefer shorter word abbreviations
                let
                    -- Sort word abbreviation candidates by length
                    sortedWordAbbrevs = 
                        if String.length wordAbbrev <= 2 then
                            [wordAbbrev]  -- Good short abbreviation like "tt"
                        else
                            -- Long abbreviation - generate shorter alternatives first
                            twoCharVariants ++ [wordAbbrev]
                in
                sortedWordAbbrevs ++       -- Prefer shorter abbreviations
                [ firstTwoChars ] ++       
                [ firstThreeChars ] ++     
                [ String.left 4 cleanName ] ++ 
                [ firstChar ]              
    in
    allCandidates
    |> List.filter (not << String.isEmpty)
    |> List.filter isValidKeybinding
    |> removeDuplicates
    -- Keep the existing logic - don't sort by length to preserve smart prioritization


generateTwoCharVariants : String -> List String
generateTwoCharVariants cleanName =
    let
        firstChar = String.left 1 cleanName
        chars = String.toList cleanName |> List.drop 1 |> List.take 4
        variants = List.map (\c -> firstChar ++ String.fromChar c) chars
    in
    variants


generateRepeatingVariants : String -> List String
generateRepeatingVariants cleanName =
    let
        baseLength = String.length cleanName
    in
    if baseLength <= 2 then
        List.range 1 4
            |> List.map (\targetLength ->
                String.repeat (targetLength // baseLength + 1) cleanName
                |> String.left targetLength
            )
            |> List.filter (\variant -> String.length variant >= baseLength)
    else
        []


isValidKeybinding : String -> Bool
isValidKeybinding keybinding =
    let
        regex = Regex.fromString "^[a-z0-9]+$" |> Maybe.withDefault Regex.never
    in
    Regex.contains regex keybinding


removeDuplicates : List String -> List String
removeDuplicates list =
    removeDuplicatesHelper list []


removeDuplicatesHelper : List String -> List String -> List String
removeDuplicatesHelper remaining seen =
    case remaining of
        [] -> List.reverse seen
        head :: tail ->
            if List.member head seen then
                removeDuplicatesHelper tail seen
            else
                removeDuplicatesHelper tail (head :: seen)


-- Check if a string has repeated characters (like "jtt" has repeated "t")
hasRepeatedChars : String -> Bool
hasRepeatedChars str =
    let
        chars = String.toList str
        uniqueChars = removeDuplicates (List.map String.fromChar chars)
    in
    List.length chars /= List.length uniqueChars


-- Compare candidates: shorter is always better, within same length prefer quality
compareByLengthAndQuality : String -> String -> Order
compareByLengthAndQuality a b =
    let
        aLen = String.length a
        bLen = String.length b
    in
    if aLen /= bLen then
        -- Different lengths: shorter wins
        compare aLen bLen
    else
        -- Same length: prefer non-repeating characters
        case (hasRepeatedChars a, hasRepeatedChars b) of
            (True, False) -> GT   -- b is better (no repeats)
            (False, True) -> LT   -- a is better (no repeats)
            _ -> EQ               -- Same quality, keep original order


-- Post-process to upgrade keybindings to shorter variants when safe
-- This is specifically for cases like "cool" being alone but getting "co" instead of "c"
optimizeKeybindingsToShorter : List ImmichAlbum -> List (ImmichAlbumId, String) -> List (ImmichAlbumId, String)
optimizeKeybindingsToShorter albums keybindings =
    let
        allKeybindings = List.map Tuple.second keybindings
        albumDict = Dict.fromList (List.map (\album -> (album.id, album.albumName)) albums)
        
        -- Only upgrade in very specific cases where single letter is clearly better
        shouldUpgradeToSingleLetter : ImmichAlbumId -> String -> Bool
        shouldUpgradeToSingleLetter albumId currentKb =
            case Dict.get albumId albumDict of
                Nothing -> False
                Just albumName ->
                    if String.length currentKb == 2 then
                        let
                            firstChar = String.left 1 currentKb
                            cleanAlbumName = String.toLower (String.trim albumName)
                            
                            -- Count how many albums start with this letter
                            albumsWithSameLetter = 
                                albums
                                    |> List.map (\album -> String.toLower (String.trim album.albumName))
                                    |> List.filter (\name -> String.startsWith firstChar name)
                            
                            -- Check if this is a simple word that could naturally get a single letter
                            isSimpleWord = not (String.contains " " cleanAlbumName) && String.length cleanAlbumName >= 3
                            isOnlyAlbumWithThisLetter = List.length albumsWithSameLetter == 1
                            isExactPrefix = currentKb == String.left 2 cleanAlbumName
                            
                            -- Exclude words where 2-char is meaningfully better than single char
                            -- (e.g., "general" -> "ge" is better than "g", but "cool" -> "c" is better than "co")
                            meaningfulTwoChar = case cleanAlbumName of
                                "general" -> True  -- "ge" is meaningful
                                "garage" -> True   -- "ga" is meaningful  
                                "garden" -> True   -- "ga" is meaningful
                                _ -> False
                        in
                        isOnlyAlbumWithThisLetter && isExactPrefix && isSimpleWord && not meaningfulTwoChar
                    else
                        False
        
        -- Try to upgrade each keybinding to a single letter if clearly safe
        upgradeKeybinding : (ImmichAlbumId, String) -> (ImmichAlbumId, String)
        upgradeKeybinding (albumId, currentKb) =
            if shouldUpgradeToSingleLetter albumId currentKb then
                let
                    singleLetter = String.left 1 currentKb
                in
                (albumId, singleLetter)
            else
                (albumId, currentKb)
    in
    List.map upgradeKeybinding keybindings