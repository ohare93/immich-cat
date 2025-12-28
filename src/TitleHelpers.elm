module TitleHelpers exposing (createDetailedViewTitle, getMoveFromInfo)

import AssetSourceTypes exposing (AssetSource(..))
import Immich exposing (CategorisationFilter(..), ImageOrder(..), MediaTypeFilter(..), StatusFilter(..))
import Menus exposing (AlbumConfig)


{-| Create a detailed title string for the current view based on asset source.
Shows the type of view (Timeline, Search, Album) with any active filters.
-}
createDetailedViewTitle : AssetSource -> String
createDetailedViewTitle assetSource =
    case assetSource of
        ImageSearch config ->
            let
                orderText =
                    case config.order of
                        CreatedDesc ->
                            "[created desc]"

                        CreatedAsc ->
                            "[created asc]"

                        ModifiedDesc ->
                            "[modified desc]"

                        ModifiedAsc ->
                            "[modified asc]"

                        Random ->
                            "[random]"

                        DurationAsc ->
                            "[duration asc]"

                        DurationDesc ->
                            "[duration desc]"

                mediaText =
                    case config.mediaType of
                        AllMedia ->
                            ""

                        ImagesOnly ->
                            " [images]"

                        VideosOnly ->
                            " [videos]"

                statusText =
                    case config.status of
                        AllStatuses ->
                            ""

                        FavoritesOnly ->
                            " [favourites]"

                        ArchivedOnly ->
                            " [archived]"

                categText =
                    case config.categorisation of
                        All ->
                            "Timeline"

                        Uncategorised ->
                            "Timeline [uncategorised]"
            in
            categText ++ statusText ++ mediaText ++ " " ++ orderText

        TextSearch searchText _ ->
            "Search \"" ++ searchText ++ "\""

        Album album ->
            "Album \"" ++ album.albumName ++ "\""

        FilteredAlbum album config ->
            let
                orderText =
                    case config.order of
                        CreatedDesc ->
                            "[created desc]"

                        CreatedAsc ->
                            "[created asc]"

                        ModifiedDesc ->
                            "[modified desc]"

                        ModifiedAsc ->
                            "[modified asc]"

                        Random ->
                            "[random]"

                        DurationAsc ->
                            "[duration asc]"

                        DurationDesc ->
                            "[duration desc]"

                mediaText =
                    case config.mediaType of
                        AllMedia ->
                            ""

                        ImagesOnly ->
                            " [images]"

                        VideosOnly ->
                            " [videos]"

                statusText =
                    case config.status of
                        AllStatuses ->
                            ""

                        FavoritesOnly ->
                            " [favourites]"

                        ArchivedOnly ->
                            " [archived]"

                hasFilters =
                    config.mediaType /= AllMedia || config.status /= AllStatuses || config.order /= CreatedDesc
            in
            if hasFilters then
                "Album \"" ++ album.albumName ++ "\"" ++ statusText ++ mediaText ++ " " ++ orderText

            else
                "Album \"" ++ album.albumName ++ "\""

        NoAssets ->
            ""


{-| Extract move-from info from asset source if in move-from mode.
Returns the album name and whether move-from mode is enabled.
-}
getMoveFromInfo : AssetSource -> Maybe ( String, Bool )
getMoveFromInfo assetSource =
    case assetSource of
        FilteredAlbum album config ->
            Just ( album.albumName, config.moveFromMode )

        _ ->
            Nothing
