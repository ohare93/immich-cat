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

        -- Check if a character is valid as the next character in a partial keybinding
        isValidNextCharacter : String -> Char -> Dict ImmichAlbumId String -> Bool
        isValidNextCharacter partialKb nextChar keybindings =
            let
                targetString =
                    partialKb ++ String.fromChar nextChar

                keybindingList =
                    Dict.values keybindings
            in
            List.any (String.startsWith targetString) keybindingList
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
