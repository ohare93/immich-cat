module AssetSourceTypes exposing (AlbumConfig, AssetSource(..), FeedbackMessage(..), defaultAlbumConfig, feedbackMessageToString)

{-| Shared type for asset sources to avoid circular dependencies.

This module contains only the AssetSource type and has minimal dependencies,
allowing both Types.elm and UpdateMenus.elm to import it without creating cycles.

-}

import Immich exposing (ImageOrder(..), ImageSearchConfig, ImmichAlbum, MediaTypeFilter(..), SearchContext, StatusFilter(..))


{-| Configuration for filtered album viewing
-}
type alias AlbumConfig =
    { mediaType : MediaTypeFilter
    , order : ImageOrder
    , status : StatusFilter
    , moveFromMode : Bool
    }


{-| Default album configuration
-}
defaultAlbumConfig : AlbumConfig
defaultAlbumConfig =
    { mediaType = AllMedia
    , order = CreatedDesc
    , status = AllStatuses
    , moveFromMode = False
    }


{-| Source of the current assets being viewed
-}
type AssetSource
    = NoAssets
    | ImageSearch ImageSearchConfig
    | TextSearch String SearchContext
    | Album ImmichAlbum
    | FilteredAlbum ImmichAlbum AlbumConfig


{-| Feedback messages for user notifications
-}
type FeedbackMessage
    = AlbumsLoaded Int
    | AlbumsReloaded Int
    | NoAlbumsFound
    | AlbumMembershipFetchFailed String


{-| Convert FeedbackMessage to a displayable string
-}
feedbackMessageToString : FeedbackMessage -> String
feedbackMessageToString msg =
    case msg of
        AlbumsLoaded count ->
            "Loaded " ++ String.fromInt count ++ " albums"

        AlbumsReloaded count ->
            "Reloaded " ++ String.fromInt count ++ " albums"

        NoAlbumsFound ->
            "No albums found"

        AlbumMembershipFetchFailed error ->
            "Album membership fetch failed: " ++ error
