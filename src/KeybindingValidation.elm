module KeybindingValidation exposing
    ( isValidNextCharacter
    , getNextAvailableCharacters
    , formatKeybindingWithHighlight
    , validateKeybindingInput
    , couldStartKeybinding
    , KeybindingValidationResult(..)
    )

import Dict exposing (Dict)
import Element exposing (Element, text)
import Element.Font as Font
import Immich exposing (ImmichAlbum, ImmichAlbumId)
import ViewAlbums exposing (getAlbumByExactKeybinding)

type KeybindingValidationResult
    = ValidKeybinding String
    | InvalidCharacter String
    | ExactMatch ImmichAlbum

-- Check if a character is valid as the next character in a partial keybinding
isValidNextCharacter : String -> Char -> Dict ImmichAlbumId String -> Bool
isValidNextCharacter partialKeybinding nextChar albumKeybindings =
    let
        targetString = partialKeybinding ++ String.fromChar nextChar
        keybindingList = Dict.values albumKeybindings
    in
    List.any (String.startsWith targetString) keybindingList

-- Get all valid next characters for a partial keybinding
getNextAvailableCharacters : String -> Dict ImmichAlbumId String -> List Char
getNextAvailableCharacters partialKeybinding albumKeybindings =
    let
        keybindingList = Dict.values albumKeybindings
        possibleNextChars = 
            keybindingList
                |> List.filter (String.startsWith partialKeybinding)
                |> List.map (String.dropLeft (String.length partialKeybinding))
                |> List.filterMap (String.toList >> List.head)
                |> List.sort
                |> removeDuplicates
    in
    possibleNextChars

-- Remove duplicates from a list
removeDuplicates : List comparable -> List comparable
removeDuplicates list =
    case list of
        [] -> []
        x :: xs ->
            x :: removeDuplicates (List.filter ((/=) x) xs)

-- Format a keybinding string with highlighting for next available characters
formatKeybindingWithHighlight : String -> Dict ImmichAlbumId String -> List (Element msg)
formatKeybindingWithHighlight keybinding albumKeybindings =
    let
        nextChars = getNextAvailableCharacters keybinding albumKeybindings
        keybindingList = Dict.values albumKeybindings
        
        -- Find all keybindings that start with our current keybinding
        matchingKeybindings = 
            keybindingList
                |> List.filter (String.startsWith keybinding)
                |> List.sort
        
        -- Format each matching keybinding with highlighting
        formatKeybinding : String -> List (Element msg)
        formatKeybinding fullKeybinding =
            let
                remainingPart = String.dropLeft (String.length keybinding) fullKeybinding
                nextChar = String.left 1 remainingPart
                restOfKeybinding = String.dropLeft 1 remainingPart
            in
            [ text keybinding
            , if String.isEmpty nextChar then
                text ""
              else
                Element.el [ Font.bold ] (text nextChar)
            , text restOfKeybinding
            ]
    in
    if List.isEmpty matchingKeybindings then
        [ text keybinding ]
    else
        matchingKeybindings
            |> List.head
            |> Maybe.map formatKeybinding
            |> Maybe.withDefault [ text keybinding ]

-- Check if a character could potentially start any keybinding
couldStartKeybinding : String -> Dict ImmichAlbumId String -> Bool
couldStartKeybinding inputChar albumKeybindings =
    let
        keybindingList = Dict.values albumKeybindings
    in
    List.any (String.startsWith inputChar) keybindingList

-- Main validation function that determines the result of adding a character to a partial keybinding
validateKeybindingInput : String -> String -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> KeybindingValidationResult
validateKeybindingInput partialKeybinding inputChar albumKeybindings knownAlbums =
    let
        newPartialKeybinding = partialKeybinding ++ inputChar
        keyChar = String.toList inputChar |> List.head |> Maybe.withDefault ' '
        maybeExactMatch = getAlbumByExactKeybinding newPartialKeybinding albumKeybindings knownAlbums
        hasKeybindings = not (Dict.isEmpty albumKeybindings)
    in
    case maybeExactMatch of
        Just album ->
            ExactMatch album
        Nothing ->
            if hasKeybindings then
                -- We have keybindings, so validate against them
                if isValidNextCharacter partialKeybinding keyChar albumKeybindings then
                    ValidKeybinding newPartialKeybinding
                else
                    InvalidCharacter inputChar
            else
                -- No keybindings available, treat as invalid for keybinding purposes
                -- This will be handled differently by the calling code
                InvalidCharacter inputChar