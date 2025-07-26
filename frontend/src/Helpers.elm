module Helpers exposing (..)

import Regex exposing (Regex)
import Task


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


regexFromString : String -> Regex
regexFromString searchString =
    Regex.fromStringWith { caseInsensitive = True, multiline = False } searchString |> Maybe.withDefault Regex.never
