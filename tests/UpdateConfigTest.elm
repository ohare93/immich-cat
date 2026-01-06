module UpdateConfigTest exposing (..)

{-| Tests for UpdateConfig.handleConfigLoaded, specifically the shouldInitializeImmich logic.

These tests verify that Immich initialization happens correctly when:

1.  Both localStorage values are loaded
2.  Neither is in localStorage (fallback to env)
3.  URL from env, key from localStorage (the bug case that was fixed)
4.  URL from localStorage, key from env
5.  Only one load has completed (should not initialize)
6.  Empty credentials after both loads (should not initialize)

-}

import Dict
import Expect
import Immich exposing (ImmichLoadState(..))
import Set
import Test exposing (Test, describe, test)
import UpdateConfig exposing (handleConfigLoaded)


{-| Base config for testing - all fields have reasonable defaults
-}
baseConfig :
    { key : String
    , maybeValue : Maybe String
    , currentConfiguredApiUrl : Maybe String
    , currentConfiguredApiKey : Maybe String
    , currentSettingsApiUrl : String
    , currentSettingsApiKey : String
    , currentBaseUrl : String
    , currentApiKey : String
    , knownAlbums : Dict.Dict String Immich.ImmichAlbum
    , albumKeybindings : Dict.Dict String String
    , albumsLoadState : ImmichLoadState
    , storageKeysLoaded : Set.Set String
    }
baseConfig =
    { key = ""
    , maybeValue = Nothing
    , currentConfiguredApiUrl = Nothing
    , currentConfiguredApiKey = Nothing
    , currentSettingsApiUrl = ""
    , currentSettingsApiKey = ""
    , currentBaseUrl = "https://photos.example.com"
    , currentApiKey = ""
    , knownAlbums = Dict.empty
    , albumKeybindings = Dict.empty
    , albumsLoadState = ImmichLoading
    , storageKeysLoaded = Set.empty
    }


suite : Test
suite =
    describe "UpdateConfig.handleConfigLoaded"
        [ describe "shouldInitializeImmich"
            [ test "both URL and key in localStorage - should initialize" <|
                \_ ->
                    let
                        -- Both storage keys are marked as loaded
                        config =
                            { baseConfig
                                | key = "immichApiKey"
                                , maybeValue = Just "stored-api-key"
                                , currentConfiguredApiUrl = Just "https://stored.example.com"
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal True result.shouldInitializeImmich
            , test "neither in localStorage but both in env - should initialize" <|
                \_ ->
                    let
                        -- Both storage keys loaded, but returned Nothing
                        -- currentBaseUrl and env provide the credentials
                        config =
                            { baseConfig
                                | key = "immichApiKey"
                                , maybeValue = Nothing
                                , currentConfiguredApiUrl = Nothing
                                , currentConfiguredApiKey = Nothing
                                , currentBaseUrl = "https://env.example.com"
                                , currentApiKey = "env-api-key"
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal True result.shouldInitializeImmich
            , test "URL from env, key from localStorage - should initialize (the bug case)" <|
                \_ ->
                    let
                        -- This was the bug case: URL not in localStorage (comes from env),
                        -- API key in localStorage
                        config =
                            { baseConfig
                                | key = "immichApiKey"
                                , maybeValue = Just "stored-api-key"
                                , currentConfiguredApiUrl = Nothing -- Not in localStorage
                                , currentConfiguredApiKey = Nothing -- Will be set by this load
                                , currentBaseUrl = "https://env.example.com" -- From env
                                , currentApiKey = "" -- Empty env key
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal True result.shouldInitializeImmich
            , test "URL from localStorage, key from env - should initialize" <|
                \_ ->
                    let
                        -- URL in localStorage, key from env
                        config =
                            { baseConfig
                                | key = "immichApiUrl"
                                , maybeValue = Just "https://stored.example.com"
                                , currentConfiguredApiUrl = Nothing -- Will be set by this load
                                , currentConfiguredApiKey = Nothing -- Not in localStorage
                                , currentBaseUrl = "" -- Empty env URL
                                , currentApiKey = "env-api-key" -- From env
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal True result.shouldInitializeImmich
            , test "only URL loaded (key not yet loaded) - should NOT initialize" <|
                \_ ->
                    let
                        -- Only URL has been loaded so far
                        config =
                            { baseConfig
                                | key = "immichApiUrl"
                                , maybeValue = Just "https://stored.example.com"
                                , currentBaseUrl = "https://env.example.com"
                                , currentApiKey = "env-api-key"
                                , storageKeysLoaded = Set.singleton "immichApiUrl" -- Only URL loaded
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal False result.shouldInitializeImmich
            , test "only key loaded (URL not yet loaded) - should NOT initialize" <|
                \_ ->
                    let
                        -- Only key has been loaded so far
                        config =
                            { baseConfig
                                | key = "immichApiKey"
                                , maybeValue = Just "stored-api-key"
                                , currentBaseUrl = "https://env.example.com"
                                , currentApiKey = "env-api-key"
                                , storageKeysLoaded = Set.singleton "immichApiKey" -- Only key loaded
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal False result.shouldInitializeImmich
            , test "both loads complete but credentials empty - should NOT initialize" <|
                \_ ->
                    let
                        -- Both loaded but no valid credentials from either source
                        config =
                            { baseConfig
                                | key = "immichApiKey"
                                , maybeValue = Nothing
                                , currentConfiguredApiUrl = Nothing
                                , currentConfiguredApiKey = Nothing
                                , currentBaseUrl = "" -- Empty
                                , currentApiKey = "" -- Empty
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal False result.shouldInitializeImmich
            , test "both loads complete but URL empty - should NOT initialize" <|
                \_ ->
                    let
                        config =
                            { baseConfig
                                | key = "immichApiKey"
                                , maybeValue = Just "valid-key"
                                , currentConfiguredApiUrl = Nothing
                                , currentBaseUrl = "" -- Empty URL
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal False result.shouldInitializeImmich
            , test "both loads complete but key empty - should NOT initialize" <|
                \_ ->
                    let
                        config =
                            { baseConfig
                                | key = "immichApiUrl"
                                , maybeValue = Just "https://valid.example.com"
                                , currentConfiguredApiKey = Nothing
                                , currentApiKey = "" -- Empty key
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal False result.shouldInitializeImmich
            , test "synthetic keys (saveSuccess) should not trigger initialization by themselves" <|
                \_ ->
                    let
                        -- saveSuccess is not a real storage key
                        config =
                            { baseConfig
                                | key = "saveSuccess"
                                , maybeValue = Just "Configuration saved!"
                                , currentBaseUrl = "https://valid.example.com"
                                , currentApiKey = "valid-key"
                                , storageKeysLoaded = Set.empty -- No real storage keys loaded yet
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal False result.shouldInitializeImmich
            ]
        , describe "credential fallback logic"
            [ test "uses localStorage URL when available" <|
                \_ ->
                    let
                        config =
                            { baseConfig
                                | key = "immichApiUrl"
                                , maybeValue = Just "https://stored.example.com"
                                , currentBaseUrl = "https://env.example.com"
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal "https://stored.example.com" result.baseUrl
            , test "falls back to env URL when not in localStorage" <|
                \_ ->
                    let
                        config =
                            { baseConfig
                                | key = "immichApiUrl"
                                , maybeValue = Nothing
                                , currentBaseUrl = "https://env.example.com"
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal "https://env.example.com" result.baseUrl
            , test "uses localStorage API key when available" <|
                \_ ->
                    let
                        config =
                            { baseConfig
                                | key = "immichApiKey"
                                , maybeValue = Just "stored-api-key"
                                , currentApiKey = "env-api-key"
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal "stored-api-key" result.apiKey
            , test "falls back to env API key when not in localStorage" <|
                \_ ->
                    let
                        config =
                            { baseConfig
                                | key = "immichApiKey"
                                , maybeValue = Nothing
                                , currentApiKey = "env-api-key"
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.equal "env-api-key" result.apiKey
            ]
        , describe "localStorage priority over env (both sources have values)"
            [ test "localStorage URL overrides env URL when both are present" <|
                \_ ->
                    let
                        config =
                            { baseConfig
                                | key = "immichApiUrl"
                                , maybeValue = Just "https://stored.example.com"
                                , currentBaseUrl = "https://env.example.com"
                                , currentApiKey = "env-api-key"
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.all
                        [ \r -> Expect.equal "https://stored.example.com" r.baseUrl
                        , \r -> Expect.equal "env-api-key" r.apiKey
                        ]
                        result
            , test "localStorage Key overrides env Key when both are present" <|
                \_ ->
                    let
                        config =
                            { baseConfig
                                | key = "immichApiKey"
                                , maybeValue = Just "stored-api-key"
                                , currentBaseUrl = "https://env.example.com"
                                , currentApiKey = "env-api-key"
                                , currentConfiguredApiUrl = Nothing
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.all
                        [ \r -> Expect.equal "https://env.example.com" r.baseUrl
                        , \r -> Expect.equal "stored-api-key" r.apiKey
                        ]
                        result
            , test "localStorage values override env values when both sources have values" <|
                \_ ->
                    let
                        config =
                            { baseConfig
                                | key = "immichApiKey"
                                , maybeValue = Just "stored-api-key"
                                , currentBaseUrl = "https://env.example.com"
                                , currentApiKey = "env-api-key"
                                , currentConfiguredApiUrl = Just "https://stored.example.com"
                                , storageKeysLoaded = Set.fromList [ "immichApiUrl", "immichApiKey" ]
                            }

                        result =
                            handleConfigLoaded config
                    in
                    Expect.all
                        [ \r -> Expect.equal "https://stored.example.com" r.baseUrl
                        , \r -> Expect.equal "stored-api-key" r.apiKey
                        , \r -> Expect.equal True r.shouldInitializeImmich
                        ]
                        result
            ]
        ]
