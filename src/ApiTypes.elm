module ApiTypes exposing (ApiKey(..), ApiUrl(..), apiKeyToString, apiUrlToString)

{-| Basic types for API configuration.

This module contains only ApiUrl and ApiKey types with no dependencies,
allowing them to be imported by both Immich.elm and Types.elm without cycles.

-}


{-| API URL for Immich server
-}
type ApiUrl
    = ApiUrl String


{-| Convert ApiUrl to String
-}
apiUrlToString : ApiUrl -> String
apiUrlToString (ApiUrl url) =
    url


{-| API Key for Immich authentication
-}
type ApiKey
    = ApiKey String


{-| Convert ApiKey to String
-}
apiKeyToString : ApiKey -> String
apiKeyToString (ApiKey key) =
    key
