module Helpers exposing (..)

import Dict exposing (Dict)
import Regex exposing (Regex)
import Task


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


regexFromString : String -> Regex
regexFromString searchString =
    Regex.fromStringWith { caseInsensitive = True, multiline = False } searchString |> Maybe.withDefault Regex.never


listOverrideDict : List a -> (a -> ( comparable, a )) -> Dict comparable a -> Dict comparable a
listOverrideDict newList comparer currentDict =
    let
        newDict =
            List.map comparer newList |> Dict.fromList
    in
    Dict.union newDict currentDict


loopImageIndexOverArray : Int -> Int -> Int -> Int
loopImageIndexOverArray index step length =
    modBy length (index + step)


isSupportedSearchLetter : String -> Bool
isSupportedSearchLetter testString =
    let
        regex =
            regexFromString "^[a-zA-Z0-9 ]$"
    in
    Regex.contains regex testString


isKeybindingLetter : String -> Bool
isKeybindingLetter testString =
    let
        regex =
            Regex.fromStringWith { caseInsensitive = False, multiline = False } "^[a-z0-9]$" |> Maybe.withDefault Regex.never
    in
    Regex.contains regex testString
