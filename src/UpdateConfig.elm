module UpdateConfig exposing
    ( ConfigContext
    , ConfigLoadedResult
    , ConfigMsg(..)
    , ConfigResult
    , PortCmd(..)
    , SaveConfigResult(..)
    , handleConfigLoaded
    , handleConfigMsg
    , handleSaveConfig
    )

{-| Pure functions for config validation and state updates.

This module handles the pure logic for configuration management.
Main.elm uses these results to update Model and generate port commands.

-}

import ApiTypes exposing (ApiKey(..), ApiUrl(..))
import Dict exposing (Dict)
import Helpers exposing (validateConfig)
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichApiPaths, ImmichLoadState(..), getImmichApiPaths)
import Set exposing (Set)


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
                , immichApiPaths = getImmichApiPaths (ApiUrl finalUrl) (ApiKey finalApiKey)
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
    , storageKeysLoaded : Set String
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
        -- Only initialize when BOTH storage loads have completed AND we have valid credentials
        bothLoadsComplete =
            Set.member "immichApiUrl" config.storageKeysLoaded
                && Set.member "immichApiKey" config.storageKeysLoaded

        shouldInitializeImmich =
            bothLoadsComplete
                && not (String.isEmpty finalUrl)
                && not (String.isEmpty finalApiKey)

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
    , immichApiPaths = getImmichApiPaths (ApiUrl finalUrl) (ApiKey finalApiKey)
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


{-| Config-related messages that can be handled by this module.
-}
type ConfigMsg
    = SaveConfigMsg String String
    | LoadConfigMsg String
    | ConfigLoadedMsg String (Maybe String)
    | ClearConfigMsg
    | UpdateApiUrlMsg String
    | UpdateApiKeyMsg String


{-| Context containing all config-related state needed for updates.
-}
type alias ConfigContext =
    { envBaseUrl : String
    , envApiKey : String
    , configuredApiUrl : Maybe String
    , configuredApiKey : Maybe String
    , settingsApiUrl : String
    , settingsApiKey : String
    , baseUrl : String
    , apiKey : String
    , immichApiPaths : ImmichApiPaths
    , knownAlbums : Dict ImmichAlbumId ImmichAlbum
    , albumKeybindings : Dict ImmichAlbumId String
    , albumsLoadState : ImmichLoadState
    , configValidationMessage : Maybe String
    , storageKeysLoaded : Set String
    }


{-| Result of handling a config message.
Contains updated context, commands to execute, and flags for special actions.
-}
type alias ConfigResult msg =
    { context : ConfigContext
    , portCmds : List (PortCmd msg)
    , shouldInitializeImmich : Bool
    }


{-| Representation of port commands that Main.elm should execute.
-}
type PortCmd msg
    = SaveToStorage String String
    | LoadFromStorage String
    | ClearStorage
    | ScheduleConfigLoaded Int String (Maybe String) msg


{-| Unified handler for all config-related messages.
Returns updated context and commands to execute.
-}
handleConfigMsg : (ConfigMsg -> msg) -> ConfigMsg -> ConfigContext -> ConfigResult msg
handleConfigMsg toMsg msg context =
    case msg of
        SaveConfigMsg url apiKey ->
            handleSaveConfigMsg toMsg url apiKey context

        LoadConfigMsg key ->
            handleLoadConfigMsg key context

        ConfigLoadedMsg key maybeValue ->
            handleConfigLoadedMsg toMsg key maybeValue context

        ClearConfigMsg ->
            handleClearConfigMsg context

        UpdateApiUrlMsg url ->
            handleUpdateApiUrlMsg url context

        UpdateApiKeyMsg apiKey ->
            handleUpdateApiKeyMsg apiKey context


{-| Handle SaveConfig message.
-}
handleSaveConfigMsg : (ConfigMsg -> msg) -> String -> String -> ConfigContext -> ConfigResult msg
handleSaveConfigMsg toMsg url apiKey context =
    case handleSaveConfig { url = url, apiKey = apiKey, envBaseUrl = context.envBaseUrl, envApiKey = context.envApiKey } of
        ValidationError errorMsg ->
            { context = { context | configValidationMessage = Just errorMsg }
            , portCmds = []
            , shouldInitializeImmich = False
            }

        ConfigValid result ->
            { context =
                { context
                    | configValidationMessage = Just "Saving configuration..."
                    , configuredApiUrl = Just result.finalUrl
                    , configuredApiKey = Just result.finalApiKey
                    , baseUrl = result.finalUrl
                    , apiKey = result.finalApiKey
                    , immichApiPaths = result.immichApiPaths
                }
            , portCmds =
                [ SaveToStorage "immichApiUrl" result.finalUrl
                , SaveToStorage "immichApiKey" result.finalApiKey
                , ScheduleConfigLoaded 1000 "saveSuccess" (Just "✅ Configuration saved successfully!") (toMsg (ConfigLoadedMsg "saveSuccess" (Just "✅ Configuration saved successfully!")))
                ]
            , shouldInitializeImmich = False
            }


{-| Handle LoadConfig message.
-}
handleLoadConfigMsg : String -> ConfigContext -> ConfigResult msg
handleLoadConfigMsg key context =
    { context = context
    , portCmds = [ LoadFromStorage key ]
    , shouldInitializeImmich = False
    }


{-| Handle ConfigLoaded message.
-}
handleConfigLoadedMsg : (ConfigMsg -> msg) -> String -> Maybe String -> ConfigContext -> ConfigResult msg
handleConfigLoadedMsg toMsg key maybeValue context =
    let
        -- Track which storage keys have been loaded (only real storage keys, not synthetic ones)
        updatedStorageKeysLoaded =
            if key == "immichApiUrl" || key == "immichApiKey" then
                Set.insert key context.storageKeysLoaded

            else
                context.storageKeysLoaded

        result =
            handleConfigLoaded
                { key = key
                , maybeValue = maybeValue
                , currentConfiguredApiUrl = context.configuredApiUrl
                , currentConfiguredApiKey = context.configuredApiKey
                , currentSettingsApiUrl = context.settingsApiUrl
                , currentSettingsApiKey = context.settingsApiKey
                , currentBaseUrl = context.baseUrl
                , currentApiKey = context.apiKey
                , knownAlbums = context.knownAlbums
                , albumKeybindings = context.albumKeybindings
                , albumsLoadState = context.albumsLoadState
                , storageKeysLoaded = updatedStorageKeysLoaded
                }

        updatedContext =
            { context
                | configuredApiUrl = result.configuredApiUrl
                , configuredApiKey = result.configuredApiKey
                , settingsApiUrl = result.settingsApiUrl
                , settingsApiKey = result.settingsApiKey
                , configValidationMessage = result.configValidationMessage
                , baseUrl = result.baseUrl
                , apiKey = result.apiKey
                , immichApiPaths = result.immichApiPaths
                , knownAlbums = result.knownAlbums
                , albumKeybindings = result.albumKeybindings
                , albumsLoadState = result.albumsLoadState
                , storageKeysLoaded = updatedStorageKeysLoaded
            }

        autoClearCmd =
            if result.shouldAutoClear then
                [ ScheduleConfigLoaded 3000 "clearSuccess" Nothing (toMsg (ConfigLoadedMsg "clearSuccess" Nothing)) ]

            else
                []
    in
    { context = updatedContext
    , portCmds = autoClearCmd
    , shouldInitializeImmich = result.shouldInitializeImmich
    }


{-| Handle ClearConfig message.
-}
handleClearConfigMsg : ConfigContext -> ConfigResult msg
handleClearConfigMsg context =
    { context =
        { context
            | configuredApiUrl = Nothing
            , configuredApiKey = Nothing
            , settingsApiUrl = context.envBaseUrl
            , settingsApiKey = context.envApiKey
            , configValidationMessage = Nothing
            , knownAlbums = Dict.empty
            , albumKeybindings = Dict.empty
            , albumsLoadState = ImmichLoading
        }
    , portCmds = [ ClearStorage ]
    , shouldInitializeImmich = False
    }


{-| Handle UpdateSettingsApiUrl message.
-}
handleUpdateApiUrlMsg : String -> ConfigContext -> ConfigResult msg
handleUpdateApiUrlMsg url context =
    { context =
        { context
            | settingsApiUrl = url
            , configValidationMessage = Nothing
        }
    , portCmds = []
    , shouldInitializeImmich = False
    }


{-| Handle UpdateSettingsApiKey message.
-}
handleUpdateApiKeyMsg : String -> ConfigContext -> ConfigResult msg
handleUpdateApiKeyMsg apiKey context =
    { context =
        { context
            | settingsApiKey = apiKey
            , configValidationMessage = Nothing
        }
    , portCmds = []
    , shouldInitializeImmich = False
    }
