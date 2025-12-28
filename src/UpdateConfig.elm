module UpdateConfig exposing
    ( ConfigLoadedResult
    , SaveConfigResult(..)
    , handleConfigLoaded
    , handleSaveConfig
    )

{-| Pure functions for config validation and state updates.

This module handles the pure logic for configuration management.
Main.elm uses these results to update Model and generate port commands.

-}

import Dict exposing (Dict)
import Helpers exposing (validateConfig)
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichApiPaths, ImmichLoadState(..), getImmichApiPaths)


{-| Result of SaveConfig validation.
-}
type SaveConfigResult
    = ValidationError String
    | ConfigValid
        { finalUrl : String
        , finalApiKey : String
        , immichApiPaths : ImmichApiPaths
        }


{-| Validate and prepare config for saving.
-}
handleSaveConfig :
    { url : String
    , apiKey : String
    , envBaseUrl : String
    , envApiKey : String
    }
    -> SaveConfigResult
handleSaveConfig config =
    let
        validation =
            validateConfig config.url config.apiKey config.envBaseUrl config.envApiKey

        finalUrl =
            if String.isEmpty (String.trim config.url) then
                config.envBaseUrl

            else
                config.url

        finalApiKey =
            if String.isEmpty (String.trim config.apiKey) then
                config.envApiKey

            else
                config.apiKey
    in
    case validation of
        Just errorMsg ->
            ValidationError errorMsg

        Nothing ->
            ConfigValid
                { finalUrl = finalUrl
                , finalApiKey = finalApiKey
                , immichApiPaths = getImmichApiPaths finalUrl finalApiKey
                }


{-| Result of processing ConfigLoaded.
-}
type alias ConfigLoadedResult =
    { configuredApiUrl : Maybe String
    , configuredApiKey : Maybe String
    , settingsApiUrl : String
    , settingsApiKey : String
    , configValidationMessage : Maybe String
    , baseUrl : String
    , apiKey : String
    , immichApiPaths : ImmichApiPaths
    , knownAlbums : Dict ImmichAlbumId ImmichAlbum
    , albumKeybindings : Dict ImmichAlbumId String
    , albumsLoadState : ImmichLoadState
    , shouldInitializeImmich : Bool
    , shouldAutoClear : Bool
    }


{-| Process a ConfigLoaded message.
Returns the new config state values.
-}
handleConfigLoaded :
    { key : String
    , maybeValue : Maybe String
    , currentConfiguredApiUrl : Maybe String
    , currentConfiguredApiKey : Maybe String
    , currentSettingsApiUrl : String
    , currentSettingsApiKey : String
    , currentBaseUrl : String
    , currentApiKey : String
    , knownAlbums : Dict ImmichAlbumId ImmichAlbum
    , albumKeybindings : Dict ImmichAlbumId String
    , albumsLoadState : ImmichLoadState
    }
    -> ConfigLoadedResult
handleConfigLoaded config =
    let
        -- First, update the specific field based on key
        intermediate =
            case config.key of
                "immichApiUrl" ->
                    { configuredApiUrl = config.maybeValue
                    , configuredApiKey = config.currentConfiguredApiKey
                    , settingsApiUrl = config.maybeValue |> Maybe.withDefault config.currentSettingsApiUrl
                    , settingsApiKey = config.currentSettingsApiKey
                    , validationMessage =
                        if config.maybeValue /= Nothing then
                            Just "✅ Configuration saved successfully!"

                        else
                            Nothing
                    }

                "immichApiKey" ->
                    { configuredApiUrl = config.currentConfiguredApiUrl
                    , configuredApiKey = config.maybeValue
                    , settingsApiUrl = config.currentSettingsApiUrl
                    , settingsApiKey = config.maybeValue |> Maybe.withDefault config.currentSettingsApiKey
                    , validationMessage =
                        if config.maybeValue /= Nothing then
                            Just "✅ Configuration loaded from storage"

                        else
                            Nothing
                    }

                "saveSuccess" ->
                    { configuredApiUrl = config.currentConfiguredApiUrl
                    , configuredApiKey = config.currentConfiguredApiKey
                    , settingsApiUrl = config.currentSettingsApiUrl
                    , settingsApiKey = config.currentSettingsApiKey
                    , validationMessage = config.maybeValue
                    }

                "clearSuccess" ->
                    { configuredApiUrl = config.currentConfiguredApiUrl
                    , configuredApiKey = config.currentConfiguredApiKey
                    , settingsApiUrl = config.currentSettingsApiUrl
                    , settingsApiKey = config.currentSettingsApiKey
                    , validationMessage = Nothing
                    }

                _ ->
                    { configuredApiUrl = config.currentConfiguredApiUrl
                    , configuredApiKey = config.currentConfiguredApiKey
                    , settingsApiUrl = config.currentSettingsApiUrl
                    , settingsApiKey = config.currentSettingsApiKey
                    , validationMessage = Nothing
                    }

        -- Use configured values if available, otherwise fall back to current values
        finalUrl =
            intermediate.configuredApiUrl
                |> Maybe.withDefault config.currentBaseUrl

        finalApiKey =
            intermediate.configuredApiKey
                |> Maybe.withDefault config.currentApiKey

        -- Determine if we should initialize Immich
        shouldInitializeImmich =
            case ( intermediate.configuredApiUrl, intermediate.configuredApiKey ) of
                ( Just _, Just _ ) ->
                    -- Both localStorage values loaded - use them
                    True

                ( Nothing, Nothing ) ->
                    -- Both localStorage values are empty - fall back to env variables if valid
                    not (String.isEmpty finalUrl) && not (String.isEmpty finalApiKey)

                _ ->
                    -- Only one localStorage value loaded - wait for the other one
                    False

        -- Check if credentials changed
        credentialsChanged =
            finalUrl /= config.currentBaseUrl || finalApiKey /= config.currentApiKey

        -- Determine if auto-clear should trigger
        shouldAutoClear =
            config.key == "saveSuccess" && config.maybeValue /= Nothing
    in
    { configuredApiUrl = intermediate.configuredApiUrl
    , configuredApiKey = intermediate.configuredApiKey
    , settingsApiUrl = intermediate.settingsApiUrl
    , settingsApiKey = intermediate.settingsApiKey
    , configValidationMessage = intermediate.validationMessage
    , baseUrl = finalUrl
    , apiKey = finalApiKey
    , immichApiPaths = getImmichApiPaths finalUrl finalApiKey
    , knownAlbums =
        if credentialsChanged then
            Dict.empty

        else
            config.knownAlbums
    , albumKeybindings =
        if credentialsChanged then
            Dict.empty

        else
            config.albumKeybindings
    , albumsLoadState =
        if credentialsChanged then
            ImmichLoading

        else
            config.albumsLoadState
    , shouldInitializeImmich = shouldInitializeImmich && config.albumsLoadState == ImmichLoading
    , shouldAutoClear = shouldAutoClear
    }
