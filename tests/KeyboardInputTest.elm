module KeyboardInputTest exposing (..)

import Expect
import KeyboardInput
    exposing
        ( KeyAction(..)
        , KeyReleaseResult(..)
        , ModifierState
        , ModifierUpdate(..)
        , classifyKeyPress
        , processKeyRelease
        , resetModifiers
        )
import Test exposing (Test, describe, test)


{-| Default modifier state with nothing pressed.
-}
noModifiers : ModifierState
noModifiers =
    { controlPressed = False
    , altPressed = False
    }


{-| Modifier state with Control pressed.
-}
controlPressed : ModifierState
controlPressed =
    { controlPressed = True
    , altPressed = False
    }


{-| Modifier state with Alt pressed.
-}
altPressed : ModifierState
altPressed =
    { controlPressed = False
    , altPressed = True
    }


{-| Modifier state with both Control and Alt pressed.
-}
bothPressed : ModifierState
bothPressed =
    { controlPressed = True
    , altPressed = True
    }


suite : Test
suite =
    describe "KeyboardInput"
        [ classifyKeyPressTests
        , processKeyReleaseTests
        , resetModifiersTests
        ]


classifyKeyPressTests : Test
classifyKeyPressTests =
    describe "classifyKeyPress"
        [ describe "Escape key"
            [ test "returns EscapePressed for Escape key" <|
                \_ ->
                    classifyKeyPress "Escape" noModifiers
                        |> Expect.equal EscapePressed
            , test "returns EscapePressed even with Control pressed" <|
                \_ ->
                    classifyKeyPress "Escape" controlPressed
                        |> Expect.equal EscapePressed
            , test "returns EscapePressed even with Alt pressed" <|
                \_ ->
                    classifyKeyPress "Escape" altPressed
                        |> Expect.equal EscapePressed
            ]
        , describe "Modifier keys"
            [ test "returns ModifierPressed SetControlPressed for Control key" <|
                \_ ->
                    classifyKeyPress "Control" noModifiers
                        |> Expect.equal (ModifierPressed SetControlPressed)
            , test "returns ModifierPressed SetAltPressed for Alt key" <|
                \_ ->
                    classifyKeyPress "Alt" noModifiers
                        |> Expect.equal (ModifierPressed SetAltPressed)
            ]
        , describe "Navigation keys"
            [ test "Alt+o triggers NavigationBack" <|
                \_ ->
                    classifyKeyPress "o" altPressed
                        |> Expect.equal NavigationBack
            , test "Alt+i triggers NavigationForward" <|
                \_ ->
                    classifyKeyPress "i" altPressed
                        |> Expect.equal NavigationForward
            ]
        , describe "Regular keys without modifiers"
            [ test "regular letter returns RegularKeyPressed" <|
                \_ ->
                    classifyKeyPress "a" noModifiers
                        |> Expect.equal (RegularKeyPressed "a")
            , test "number key returns RegularKeyPressed" <|
                \_ ->
                    classifyKeyPress "1" noModifiers
                        |> Expect.equal (RegularKeyPressed "1")
            , test "special key returns RegularKeyPressed" <|
                \_ ->
                    classifyKeyPress "Enter" noModifiers
                        |> Expect.equal (RegularKeyPressed "Enter")
            ]
        , describe "Keys with Control modifier"
            [ test "Control+k returns prefixed key" <|
                \_ ->
                    classifyKeyPress "k" controlPressed
                        |> Expect.equal (RegularKeyPressed "Control+k")
            , test "Control+a returns prefixed key" <|
                \_ ->
                    classifyKeyPress "a" controlPressed
                        |> Expect.equal (RegularKeyPressed "Control+a")
            , test "Control+Enter returns prefixed key" <|
                \_ ->
                    classifyKeyPress "Enter" controlPressed
                        |> Expect.equal (RegularKeyPressed "Control+Enter")
            ]
        , describe "Keys with Alt modifier"
            [ test "Alt+a returns prefixed key (not navigation)" <|
                \_ ->
                    classifyKeyPress "a" altPressed
                        |> Expect.equal (RegularKeyPressed "Alt+a")
            , test "Alt+k returns prefixed key" <|
                \_ ->
                    classifyKeyPress "k" altPressed
                        |> Expect.equal (RegularKeyPressed "Alt+k")
            ]
        , describe "Control takes precedence over Alt"
            [ test "with both modifiers, Control prefix is used" <|
                \_ ->
                    classifyKeyPress "k" bothPressed
                        |> Expect.equal (RegularKeyPressed "Control+k")
            , test "Alt+o becomes Control+o when Control is also pressed" <|
                \_ ->
                    classifyKeyPress "o" bothPressed
                        |> Expect.equal (RegularKeyPressed "Control+o")
            ]
        ]


processKeyReleaseTests : Test
processKeyReleaseTests =
    describe "processKeyRelease"
        [ describe "Control key release"
            [ test "releases Control from control-pressed state" <|
                \_ ->
                    case processKeyRelease "Control" controlPressed of
                        ModifierReleased state ->
                            Expect.equal { controlPressed = False, altPressed = False } state

                        KeyReleaseIgnored ->
                            Expect.fail "Expected ModifierReleased"
            , test "releases Control while Alt remains pressed" <|
                \_ ->
                    case processKeyRelease "Control" bothPressed of
                        ModifierReleased state ->
                            Expect.equal { controlPressed = False, altPressed = True } state

                        KeyReleaseIgnored ->
                            Expect.fail "Expected ModifierReleased"
            ]
        , describe "Alt key release"
            [ test "releases Alt from alt-pressed state" <|
                \_ ->
                    case processKeyRelease "Alt" altPressed of
                        ModifierReleased state ->
                            Expect.equal { controlPressed = False, altPressed = False } state

                        KeyReleaseIgnored ->
                            Expect.fail "Expected ModifierReleased"
            , test "releases Alt while Control remains pressed" <|
                \_ ->
                    case processKeyRelease "Alt" bothPressed of
                        ModifierReleased state ->
                            Expect.equal { controlPressed = True, altPressed = False } state

                        KeyReleaseIgnored ->
                            Expect.fail "Expected ModifierReleased"
            ]
        , describe "Non-modifier key release"
            [ test "regular key release is ignored" <|
                \_ ->
                    processKeyRelease "a" noModifiers
                        |> Expect.equal KeyReleaseIgnored
            , test "Escape release is ignored" <|
                \_ ->
                    processKeyRelease "Escape" noModifiers
                        |> Expect.equal KeyReleaseIgnored
            , test "Enter release is ignored" <|
                \_ ->
                    processKeyRelease "Enter" controlPressed
                        |> Expect.equal KeyReleaseIgnored
            ]
        ]


resetModifiersTests : Test
resetModifiersTests =
    describe "resetModifiers"
        [ test "returns state with both modifiers false" <|
            \_ ->
                resetModifiers
                    |> Expect.equal { controlPressed = False, altPressed = False }
        ]
