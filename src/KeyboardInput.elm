module KeyboardInput exposing
    ( KeyAction(..)
    , KeyReleaseResult(..)
    , ModifierState
    , ModifierUpdate(..)
    , classifyKeyPress
    , processKeyRelease
    , resetModifiers
    )

{-| Pure functions for keyboard input processing.

Extracts modifier state management and key classification from Main.elm,
keeping the mode-specific routing in Main.elm.

-}


{-| State of modifier keys (Control and Alt).
-}
type alias ModifierState =
    { controlPressed : Bool
    , altPressed : Bool
    }


{-| Result of classifying a key press.
-}
type KeyAction
    = EscapePressed
      -- Escape key always resets modifiers and needs special handling
    | ModifierPressed ModifierUpdate
      -- Control or Alt key pressed - just update state
    | RegularKeyPressed String
      -- Regular key with effective key (includes modifier prefix like "Control+k")
    | NavigationBack
      -- Alt+o - navigate history back
    | NavigationForward



-- Alt+i - navigate history forward


{-| How to update modifier state after a modifier key press.
-}
type ModifierUpdate
    = SetControlPressed
    | SetAltPressed


{-| Result of processing a key release.
-}
type KeyReleaseResult
    = ModifierReleased ModifierState
      -- A modifier was released, here's the new state
    | KeyReleaseIgnored



-- Non-modifier key release, no state change needed


{-| Classify a key press into an action type.

This is the main entry point for keyboard input processing.
Takes the raw key string and current modifier state.
Returns a KeyAction that Main.elm can pattern match on.

-}
classifyKeyPress : String -> ModifierState -> KeyAction
classifyKeyPress key modifiers =
    if key == "Escape" then
        EscapePressed

    else if key == "Control" then
        ModifierPressed SetControlPressed

    else if key == "Alt" then
        ModifierPressed SetAltPressed

    else
        let
            effectiveKey =
                computeEffectiveKey key modifiers
        in
        -- Check for global navigation keys
        case effectiveKey of
            "Alt+o" ->
                NavigationBack

            "Alt+i" ->
                NavigationForward

            _ ->
                RegularKeyPressed effectiveKey


{-| Compute the effective key with modifier prefix.

If Control is pressed, prefixes with "Control+".
If Alt is pressed, prefixes with "Alt+".
Otherwise returns the key unchanged.

Control takes precedence over Alt if both are pressed.

-}
computeEffectiveKey : String -> ModifierState -> String
computeEffectiveKey key modifiers =
    if modifiers.controlPressed then
        "Control+" ++ key

    else if modifiers.altPressed then
        "Alt+" ++ key

    else
        key


{-| Process a key release event.

Only Control and Alt releases affect state.
Other key releases are ignored.

-}
processKeyRelease : String -> ModifierState -> KeyReleaseResult
processKeyRelease key currentState =
    if key == "Control" then
        ModifierReleased { currentState | controlPressed = False }

    else if key == "Alt" then
        ModifierReleased { currentState | altPressed = False }

    else
        KeyReleaseIgnored


{-| Reset all modifiers to unpressed state.

Used when Escape is pressed or window loses focus.

-}
resetModifiers : ModifierState
resetModifiers =
    { controlPressed = False
    , altPressed = False
    }
