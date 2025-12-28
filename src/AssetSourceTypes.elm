module AssetSourceTypes exposing (AssetSource(..))

{-| Shared type for asset sources to avoid circular dependencies.

This module contains only the AssetSource type and has minimal dependencies,
allowing both Types.elm and UpdateMenus.elm to import it without creating cycles.

-}

import Immich exposing (ImageSearchConfig, ImmichAlbum, SearchContext)
import Menus exposing (AlbumConfig)


{-| Source of the current assets being viewed
-}
type AssetSource
    = NoAssets
    | ImageSearch ImageSearchConfig
    | TextSearch String SearchContext
    | Album ImmichAlbum
    | FilteredAlbum ImmichAlbum AlbumConfig
