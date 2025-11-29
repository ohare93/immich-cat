module HelpText exposing
    ( AlbumBrowseState(..)
    , ViewContext(..)
    , viewContextHelp
    , viewKeybinding
    )

import Element exposing (Element, column, el, fill, px, row, text, width)
import Element.Background as Background
import Element.Font as Font
import ViewAlbums exposing (InputMode(..), usefulColours)



-- Context types for different help scenarios


type ViewContext
    = MainMenuContext
    | TimelineContext
    | SearchContext { inputFocused : Bool }
    | AlbumBrowseContext AlbumBrowseState
    | AlbumViewContext
    | AssetViewContext InputMode
    | SettingsContext


type AlbumBrowseState
    = SelectingAlbum
    | ConfiguringView



-- Main help content function


viewContextHelp : ViewContext -> Element msg
viewContextHelp context =
    case context of
        MainMenuContext ->
            viewMainMenuHelp

        TimelineContext ->
            viewTimelineHelp

        SearchContext { inputFocused } ->
            viewSearchHelp inputFocused

        AlbumBrowseContext albumState ->
            viewAlbumBrowseHelp albumState

        AlbumViewContext ->
            viewAlbumViewHelp

        AssetViewContext inputMode ->
            viewAssetViewHelp inputMode

        SettingsContext ->
            viewSettingsHelp



-- Main Menu Help


viewMainMenuHelp : Element msg
viewMainMenuHelp =
    column [ width fill, Element.spacingXY 0 15 ]
        [ el [ Font.size 18, Font.bold ] (text "Welcome to Image Categorizer")
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 14 ] (text "Choose a view mode from the left to get started:")
            , el [ Font.size 14 ] (text "• Timeline: Browse all your assets chronologically")
            , el [ Font.size 14 ] (text "• Search: Find specific assets with smart search")
            , el [ Font.size 14 ] (text "• Albums: Browse and organize your photo albums")
            , el [ Font.size 14 ] (text "• Reload Albums: Refresh album list from server")
            , el [ Font.size 14 ] (text "• Settings: Configure your preferences")
            ]
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 14, Font.bold ] (text "Navigation Tips")
            , el [ Font.size 13 ] (text "• Press the highlighted key or click to navigate")
            , el [ Font.size 13 ] (text "• Press ? while in any view to get contextual help")
            , el [ Font.size 13 ] (text "• Use Escape to return to previous views")
            ]
        ]



-- Timeline Help


viewTimelineHelp : Element msg
viewTimelineHelp =
    column [ width fill, Element.spacingXY 0 15 ]
        [ el [ Font.size 18, Font.bold ] (text "Timeline Help")
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] (text "Timeline Filters")
            , viewKeybinding "m" "Toggle media type (All/Images/Videos)"
            , viewKeybinding "c" "Toggle categorisation (All/Uncategorised)"
            , viewKeybinding "o" "Cycle order (Newest/Oldest/Random)"
            , viewKeybinding "s" "Toggle status (All/Favorites/Archived)"
            ]
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] (text "Actions")
            , viewKeybinding "Enter" "Load & view assets with current filters"
            , viewKeybinding "Space" "Load & view assets with current filters"
            , viewKeybinding "Escape" "Back to Main Menu"
            ]
        ]



-- Search Help


viewSearchHelp : Bool -> Element msg
viewSearchHelp inputFocused =
    column [ width fill, Element.spacingXY 0 15 ]
        [ el [ Font.size 18, Font.bold ] (text "Search Help")
        , if inputFocused then
            column [ Element.spacingXY 0 8 ]
                [ el [ Font.size 16, Font.bold ] (text "Input Mode")
                , viewKeybinding "Type" "Enter search terms"
                , viewKeybinding "Escape" "Exit input mode"
                , viewKeybinding "Enter" "Execute search"
                ]

          else
            column [ Element.spacingXY 0 8 ]
                [ el [ Font.size 16, Font.bold ] (text "Search Filters")
                , viewKeybinding "i" "Enter input mode to type search"
                , viewKeybinding "m" "Toggle media type (All/Images/Videos)"
                , viewKeybinding "c" "Toggle search context (Content/Filename/Description)"
                , viewKeybinding "s" "Toggle status (All/Favorites/Archived)"
                ]
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] (text "Actions")
            , viewKeybinding "Enter" "Execute search with current query"
            , viewKeybinding "Space" "Execute search with current query"
            , viewKeybinding "Escape" "Back to Main Menu"
            ]
        ]



-- Album Browse Help


viewAlbumBrowseHelp : AlbumBrowseState -> Element msg
viewAlbumBrowseHelp albumState =
    case albumState of
        SelectingAlbum ->
            column [ width fill, Element.spacingXY 0 15 ]
                [ el [ Font.size 18, Font.bold ] (text "Album Selection")
                , column [ Element.spacingXY 0 8 ]
                    [ el [ Font.size 16, Font.bold ] (text "Album Selection")
                    , viewKeybinding "Type" "Search albums by name"
                    , viewKeybinding "Keys" "Type album keybinding (shown in parentheses)"
                    , viewKeybinding "↑↓" "Navigate through album list"
                    , viewKeybinding "Page↑↓" "Page through album list"
                    , viewKeybinding "Click" "Click album to select"
                    , viewKeybinding "Enter" "Select highlighted album"
                    , viewKeybinding "Escape" "Back to Main Menu"
                    ]
                ]

        ConfiguringView ->
            column [ width fill, Element.spacingXY 0 15 ]
                [ el [ Font.size 18, Font.bold ] (text "Album View Configuration")
                , column [ Element.spacingXY 0 8 ]
                    [ el [ Font.size 16, Font.bold ] (text "Album Filters")
                    , viewKeybinding "m" "Toggle media type (All/Images/Videos)"
                    , viewKeybinding "o" "Cycle order (Newest/Oldest/Random)"
                    , viewKeybinding "s" "Toggle status (All/Favorites/Archived)"
                    , viewKeybinding "x" "Toggle move mode (move assets out of this album)"
                    ]
                , column [ Element.spacingXY 0 8 ]
                    [ el [ Font.size 16, Font.bold ] (text "Actions")
                    , viewKeybinding "Enter" "Load & view album assets"
                    , viewKeybinding "Space" "Load & view album assets"
                    , viewKeybinding "Escape" "Back to Album Selection"
                    ]
                ]



-- Album View Help


viewAlbumViewHelp : Element msg
viewAlbumViewHelp =
    column [ width fill, Element.spacingXY 0 15 ]
        [ el [ Font.size 18, Font.bold ] (text "Album View Help")
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] (text "Album Filters")
            , viewKeybinding "m" "Toggle media type (All/Images/Videos)"
            , viewKeybinding "o" "Cycle order (Newest/Oldest/Random)"
            , viewKeybinding "s" "Toggle status (All/Favorites/Archived)"
            ]
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] (text "Actions")
            , viewKeybinding "Enter" "Load & view album assets"
            , viewKeybinding "Space" "Load & view album assets"
            , viewKeybinding "Escape" "Back to Album Configuration"
            ]
        ]



-- Asset View Help


viewAssetViewHelp : InputMode -> Element msg
viewAssetViewHelp inputMode =
    column [ width fill, Element.spacingXY 0 15 ]
        [ el [ Font.size 18, Font.bold ] (text "Asset Navigation Help")
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] (text "Navigation")
            , viewKeybinding "←/→" "Previous/next asset"
            , viewKeybinding "Backspace" "Previous asset"
            , viewKeybinding "Space/Enter" "Next asset"
            , viewKeybinding "Home/End" "First/last asset (Grid view)"
            , viewKeybinding "Escape" "Return to main menu"
            ]
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] (text "Asset Actions")
            , viewKeybinding "D" "Toggle delete/archive"
            , viewKeybinding "F" "Toggle favorite"
            , viewKeybinding "K" "Open in Immich (new tab)"
            , viewKeybinding "Y" "Copy image to clipboard"
            , viewKeybinding "L" "Load/toggle video (for long videos)"
            , viewKeybinding "I" "Enter album search mode"
            , viewKeybinding "R" "Reload albums from server"
            , viewKeybinding "T" "Toggle time view (Absolute/Relative)"
            , viewKeybinding "G" "Switch to grid view"
            , viewKeybinding "S" "Toggle scroll view (for large images)"
            , viewKeybinding "X" "Toggle move mode (move assets out of source album)"
            , viewKeybinding "?" "Show/hide this help"
            ]
        , if inputMode == InsertMode then
            column [ Element.spacingXY 0 8 ]
                [ el [ Font.size 16, Font.bold ] (text "Album Search (Insert Mode)")
                , viewKeybinding "Type" "Search albums by name"
                , viewKeybinding "↑↓" "Navigate through results"
                , viewKeybinding "Enter" "Add to highlighted album"
                , viewKeybinding "PageUp/PageDown" "Page through results"
                , viewKeybinding "Click" "Click album to add"
                , viewKeybinding "Escape" "Exit search mode"
                ]

          else if inputMode == KeybindingMode then
            column [ Element.spacingXY 0 8 ]
                [ el [ Font.size 16, Font.bold ] (text "Album Keybinding Mode")
                , viewKeybinding "Type" "Type album keybinding"
                , viewKeybinding "Enter" "Add to selected album"
                , viewKeybinding "Escape" "Exit keybinding mode"
                ]

          else
            case inputMode of
                ScrollViewMode _ ->
                    column [ Element.spacingXY 0 8 ]
                        [ el [ Font.size 16, Font.bold ] (text "Scroll View Mode")
                        , column [ Element.spacingXY 0 5 ]
                            [ el [ Font.size 14, Font.bold ] (text "Basic Navigation")
                            , viewKeybinding "j" "Scroll down (small)"
                            , viewKeybinding "k" "Scroll up (small)"
                            , viewKeybinding "h" "Scroll left (small)"
                            , viewKeybinding "l" "Scroll right (small)"
                            ]
                        , column [ Element.spacingXY 0 5 ]
                            [ el [ Font.size 14, Font.bold ] (text "Page Navigation")
                            , viewKeybinding "PageUp" "Scroll up (full page)"
                            , viewKeybinding "PageDown" "Scroll down (full page)"
                            , viewKeybinding "Ctrl+U" "Scroll up (half page)"
                            , viewKeybinding "Ctrl+D" "Scroll down (half page)"
                            , viewKeybinding "Ctrl+B" "Scroll up (full page)"
                            , viewKeybinding "Ctrl+F" "Scroll down (full page)"
                            ]
                        , column [ Element.spacingXY 0 5 ]
                            [ el [ Font.size 14, Font.bold ] (text "Exit")
                            , viewKeybinding "S" "Exit scroll view"
                            , viewKeybinding "Escape" "Exit scroll view"
                            ]
                        , el [ Font.size 13, Element.paddingXY 0 5 ] (text "Use this mode for very large images that need scrolling")
                        ]

                _ ->
                    Element.none
        ]



-- Settings Help


viewSettingsHelp : Element msg
viewSettingsHelp =
    column [ width fill, Element.spacingXY 0 15 ]
        [ el [ Font.size 18, Font.bold ] (text "Settings Help")
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] (text "Immich Configuration")
            , el [ Font.size 14 ] (text "Set up your Immich server connection:")
            , viewKeybinding "URL" "Enter your Immich server URL (e.g., https://immich.example.com)"
            , viewKeybinding "API Key" "Enter your Immich API key from User Settings → API Keys"
            , viewKeybinding "Save" "Click to save configuration to browser storage"
            , viewKeybinding "Clear" "Click to remove saved configuration"
            ]
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] (text "Security Notes")
            , el [ Font.size 13 ] (text "• Configuration is stored in your browser's localStorage")
            , el [ Font.size 13 ] (text "• API keys are encrypted before storage")
            , el [ Font.size 13 ] (text "• Data persists across browser sessions")
            , el [ Font.size 13 ] (text "• Clear configuration to remove stored data")
            ]
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] (text "Navigation")
            , viewKeybinding "Escape" "Return to main menu"
            ]
        ]



-- Keybinding display helper (consistent with ViewAsset.elm)


viewKeybinding : String -> String -> Element msg
viewKeybinding key description =
    row [ width fill, Element.spacingXY 10 0, Element.alignTop ]
        [ el [ width (px 100), Font.family [ Font.monospace ], Background.color (usefulColours "grey"), Element.paddingXY 8 4, Element.alignTop ] (text key)
        , Element.paragraph [ width fill, Element.alignTop ] [ text description ]
        ]
