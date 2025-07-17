module KeybindingValidation exposing
    ( KeybindingValidationResult(..)
    , validateKeybindingInput
    )

import Dict exposing (Dict)
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
        targetString =
            partialKeybinding ++ String.fromChar nextChar

        keybindingList =
            Dict.values albumKeybindings
    in
    List.any (String.startsWith targetString) keybindingList



-- Get all valid next characters for a partial keybinding
-- Remove duplicates from a list
-- Format a keybinding string with highlighting for next available characters
-- Check if a character could potentially start any keybinding
-- Main validation function that determines the result of adding a character to a partial keybinding


validateKeybindingInput : String -> String -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> KeybindingValidationResult
validateKeybindingInput partialKeybinding inputChar albumKeybindings knownAlbums =
    let
        newPartialKeybinding =
            partialKeybinding ++ inputChar

        keyChar =
            String.toList inputChar |> List.head |> Maybe.withDefault ' '

        maybeExactMatch =
            getAlbumByExactKeybinding newPartialKeybinding albumKeybindings knownAlbums

        hasKeybindings =
            not (Dict.isEmpty albumKeybindings)
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
