module Theme exposing
    ( DeviceClass(..)
    , Theme(..)
    , classifyDevice
    , getBackgroundColor
    , getHighlightColor
    , getKeybindTextColor
    , getMutedTextColor
    , getSecondaryColor
    , getTextColor
    , nextTheme
    )

import Element
import ViewAlbums


type DeviceClass
    = Mobile
    | Tablet
    | Desktop


type Theme
    = Light
    | Dark
    | System


classifyDevice : Int -> Int -> DeviceClass
classifyDevice width _ =
    if width < 768 then
        Mobile

    else if width < 1024 then
        Tablet

    else
        Desktop


nextTheme : Theme -> Theme
nextTheme currentTheme =
    case currentTheme of
        Light ->
            Dark

        Dark ->
            System

        System ->
            Light


getBackgroundColor : Theme -> Element.Color
getBackgroundColor theme =
    case theme of
        Light ->
            Element.rgb 0.98 0.98 0.98

        Dark ->
            Element.rgb 0.05 0.05 0.05

        System ->
            ViewAlbums.usefulColours "darkgrey"


getTextColor : Theme -> Element.Color
getTextColor theme =
    case theme of
        Light ->
            Element.rgb 0.1 0.1 0.1

        Dark ->
            Element.rgb 0.9 0.9 0.9

        System ->
            Element.rgb 0.1 0.1 0.1


getSecondaryColor : Theme -> Element.Color
getSecondaryColor theme =
    case theme of
        Light ->
            Element.rgb 0.6 0.6 0.6

        Dark ->
            Element.rgb 0.7 0.7 0.7

        System ->
            Element.rgb 0.6 0.6 0.6


getMutedTextColor : Theme -> Element.Color
getMutedTextColor theme =
    case theme of
        Light ->
            Element.rgb 0.5 0.5 0.5

        Dark ->
            Element.rgb 0.6 0.6 0.6

        System ->
            Element.rgb 0.5 0.5 0.5


getKeybindTextColor : Theme -> Element.Color
getKeybindTextColor theme =
    case theme of
        Light ->
            Element.rgb 0.1 0.1 0.1

        Dark ->
            Element.rgb 0.9 0.9 0.9

        System ->
            Element.rgb 0.1 0.1 0.1


getHighlightColor : Theme -> Element.Color
getHighlightColor theme =
    case theme of
        Light ->
            Element.fromRgb { red = 0.8, green = 0.2, blue = 0.2, alpha = 1 }

        Dark ->
            Element.fromRgb { red = 1.0, green = 0.4, blue = 0.4, alpha = 1 }

        System ->
            Element.fromRgb { red = 0.8, green = 0.2, blue = 0.2, alpha = 1 }
