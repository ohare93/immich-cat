module ViewInputMode exposing (computeInputMode, viewInputModeIndicator)

{-| View functions for input mode display.

Extracts input mode computation and rendering from Main.elm.

-}

import Element exposing (Element, el, row, text, width)
import Element.Background as Background
import Element.Font as Font
import Theme exposing (Theme(..))
import Types exposing (UserMode(..))
import UpdateAsset exposing (AssetState(..))
import UpdateMenus exposing (MenuState(..))
import ViewAlbums exposing (InputMode(..))


{-| Compute the current input mode from user mode.

Pure function that determines which input mode indicator to show
based on the current user mode and asset state.

-}
computeInputMode : UserMode -> InputMode
computeInputMode userMode =
    case userMode of
        MainMenu menuState ->
            case menuState of
                MainMenuHome ->
                    NormalMode

                TimelineView _ ->
                    NormalMode

                SearchView config ->
                    if config.inputFocused then
                        InsertMode

                    else
                        NormalMode

                AlbumBrowse _ ->
                    KeybindingMode

                AlbumView _ _ ->
                    NormalMode

                Settings ->
                    NormalMode

        ViewAssets assetState ->
            case assetState of
                SearchAssetInput _ ->
                    InsertMode

                SelectAlbumInput _ ->
                    InsertMode

                EditAsset editInputMode _ _ ->
                    editInputMode

                CreateAlbumConfirmation editInputMode _ _ _ ->
                    editInputMode

                ShowEditAssetHelp editInputMode _ _ ->
                    editInputMode

                GridView _ ->
                    NormalMode

        LoadingAssets _ ->
            NormalMode


{-| Render the input mode indicator bar.

Shows the current input mode with appropriate background color
and the theme indicator.

-}
viewInputModeIndicator : InputMode -> Theme -> Element msg
viewInputModeIndicator inputMode theme =
    let
        themeText =
            case theme of
                Light ->
                    "☀️"

                Dark ->
                    "🌙"

                System ->
                    "⚙️"

        modeElement =
            case inputMode of
                NormalMode ->
                    el [ width (Element.fillPortion 1), Background.color <| Element.fromRgb { red = 0.8, green = 0.8, blue = 0.8, alpha = 1 } ] <| text "Normal"

                InsertMode ->
                    el [ width (Element.fillPortion 1), Background.color <| Element.fromRgb { red = 0, green = 1, blue = 0, alpha = 1 } ] <| text "Input"

                KeybindingMode ->
                    el [ width (Element.fillPortion 1), Background.color <| Element.fromRgb { red = 1, green = 0.5, blue = 0, alpha = 1 } ] <| text "Keybind"

                ScrollViewMode _ ->
                    el [ width (Element.fillPortion 1), Background.color <| Element.fromRgb { red = 0.5, green = 0, blue = 1, alpha = 1 } ] <| text "Scroll"
    in
    row [ Element.width Element.fill ]
        [ modeElement
        , el [ Element.width (Element.px 40), Background.color <| Theme.getSecondaryColor theme, Font.color <| Theme.getTextColor theme, Element.centerX ] <| text themeText
        ]
