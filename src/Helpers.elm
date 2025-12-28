module Helpers exposing
    ( applySortingToAssets
    , categorisationToString
    , filterByMediaType
    , filterByStatus
    , isKeybindingLetter
    , isSupportedSearchLetter
    , listOverrideDict
    , loopImageIndexOverArray
    , mediaTypeToString
    , orderToString
    , regexFromString
    , sortWithIdTiebreaker
    , statusToString
    , toggleCategorisation
    , toggleMediaType
    , toggleOrder
    , toggleOrderHandler
    , toggleStatus
    , validateConfig
    )

import Dict exposing (Dict)
import Immich exposing (CategorisationFilter(..), ImageOrder(..), ImmichAsset, MediaTypeFilter(..), StatusFilter(..))
import Regex exposing (Regex)


regexFromString : String -> Regex
regexFromString searchString =
    Regex.fromStringWith { caseInsensitive = True, multiline = False } searchString |> Maybe.withDefault Regex.never


listOverrideDict : List a -> (a -> ( comparable, a )) -> Dict comparable a -> Dict comparable a
listOverrideDict newList comparer currentDict =
    let
        newDict =
            List.map comparer newList |> Dict.fromList
    in
    Dict.union newDict currentDict


loopImageIndexOverArray : Int -> Int -> Int -> Int
loopImageIndexOverArray index step length =
    modBy length (index + step)


isSupportedSearchLetter : String -> Bool
isSupportedSearchLetter testString =
    let
        regex =
            regexFromString "^[a-zA-Z0-9 \\-_.(),;:!?@#&']$"
    in
    Regex.contains regex testString


isKeybindingLetter : String -> Bool
isKeybindingLetter testString =
    let
        regex =
            Regex.fromStringWith { caseInsensitive = False, multiline = False } "^[a-z0-9]$" |> Maybe.withDefault Regex.never
    in
    Regex.contains regex testString


mediaTypeToString : MediaTypeFilter -> String
mediaTypeToString mediaType =
    case mediaType of
        AllMedia ->
            "All"

        ImagesOnly ->
            "Images"

        VideosOnly ->
            "Videos"


categorisationToString : CategorisationFilter -> String
categorisationToString categorisation =
    case categorisation of
        All ->
            "All"

        Uncategorised ->
            "Uncategorised"


orderToString : ImageOrder -> String
orderToString order =
    case order of
        CreatedDesc ->
            "Newest Created"

        CreatedAsc ->
            "Oldest Created"

        ModifiedDesc ->
            "Newest Modified"

        ModifiedAsc ->
            "Oldest Modified"

        Random ->
            "Random"

        DurationAsc ->
            "Duration ↑"

        DurationDesc ->
            "Duration ↓"


statusToString : StatusFilter -> String
statusToString status =
    case status of
        AllStatuses ->
            "All"

        FavoritesOnly ->
            "Favorites"

        ArchivedOnly ->
            "Archived"


toggleMediaType : MediaTypeFilter -> MediaTypeFilter
toggleMediaType current =
    case current of
        AllMedia ->
            ImagesOnly

        ImagesOnly ->
            VideosOnly

        VideosOnly ->
            AllMedia


toggleOrder : ImageOrder -> ImageOrder
toggleOrder current =
    case current of
        CreatedDesc ->
            CreatedAsc

        CreatedAsc ->
            ModifiedDesc

        ModifiedDesc ->
            ModifiedAsc

        ModifiedAsc ->
            Random

        Random ->
            CreatedDesc

        DurationAsc ->
            DurationDesc

        DurationDesc ->
            CreatedDesc


toggleOrderHandler : MediaTypeFilter -> ImageOrder -> ImageOrder
toggleOrderHandler mediaType currentOrder =
    case mediaType of
        VideosOnly ->
            -- When viewing videos, include duration sorting options
            case currentOrder of
                CreatedDesc ->
                    CreatedAsc

                CreatedAsc ->
                    ModifiedDesc

                ModifiedDesc ->
                    ModifiedAsc

                ModifiedAsc ->
                    DurationAsc

                DurationAsc ->
                    DurationDesc

                DurationDesc ->
                    Random

                Random ->
                    CreatedDesc

        _ ->
            -- For all media or images only, use regular toggle
            toggleOrder currentOrder


toggleStatus : StatusFilter -> StatusFilter
toggleStatus current =
    case current of
        AllStatuses ->
            FavoritesOnly

        FavoritesOnly ->
            ArchivedOnly

        ArchivedOnly ->
            AllStatuses


toggleCategorisation : CategorisationFilter -> CategorisationFilter
toggleCategorisation current =
    case current of
        All ->
            Uncategorised

        Uncategorised ->
            All


filterByMediaType : MediaTypeFilter -> List ImmichAsset -> List ImmichAsset
filterByMediaType mediaFilter assets =
    case mediaFilter of
        AllMedia ->
            assets

        ImagesOnly ->
            List.filter (\asset -> String.startsWith "image/" asset.mimeType) assets

        VideosOnly ->
            List.filter (\asset -> String.startsWith "video/" asset.mimeType) assets


filterByStatus : StatusFilter -> List ImmichAsset -> List ImmichAsset
filterByStatus statusFilter assets =
    case statusFilter of
        AllStatuses ->
            assets

        FavoritesOnly ->
            List.filter (\asset -> asset.isFavourite) assets

        ArchivedOnly ->
            List.filter (\asset -> asset.isArchived) assets


{-| Sort a list with a primary comparator and ID-based tiebreaker.
When the primary comparison results in equality, falls back to comparing IDs for deterministic ordering.
-}
sortWithIdTiebreaker : (a -> comparable) -> Bool -> (a -> String) -> List a -> List a
sortWithIdTiebreaker getPrimary descending getId items =
    List.sortWith
        (\a b ->
            let
                primaryResult =
                    if descending then
                        compare (getPrimary b) (getPrimary a)

                    else
                        compare (getPrimary a) (getPrimary b)
            in
            case primaryResult of
                EQ ->
                    compare (getId a) (getId b)

                other ->
                    other
        )
        items


{-| Sort assets by the specified order.
Uses secondary sort by ID for deterministic ordering when primary sort values are equal.
-}
applySortingToAssets : ImageOrder -> List ImmichAsset -> List ImmichAsset
applySortingToAssets order assets =
    case order of
        CreatedAsc ->
            sortWithIdTiebreaker .fileCreatedAtString False .id assets

        CreatedDesc ->
            sortWithIdTiebreaker .fileCreatedAtString True .id assets

        ModifiedAsc ->
            sortWithIdTiebreaker .fileModifiedAtString False .id assets

        ModifiedDesc ->
            sortWithIdTiebreaker .fileModifiedAtString True .id assets

        DurationAsc ->
            let
                getDuration asset =
                    asset.duration
                        |> Maybe.andThen Immich.parseDurationToSeconds
                        |> Maybe.withDefault 999999
            in
            sortWithIdTiebreaker getDuration False .id assets

        DurationDesc ->
            let
                getDuration asset =
                    asset.duration
                        |> Maybe.andThen Immich.parseDurationToSeconds
                        |> Maybe.withDefault -1
            in
            sortWithIdTiebreaker getDuration True .id assets

        Random ->
            assets


{-| Validate Immich configuration settings.
Returns Nothing if valid, Just error message if invalid.
-}
validateConfig : String -> String -> String -> String -> Maybe String
validateConfig url apiKey envUrl envApiKey =
    let
        finalUrl =
            if String.isEmpty (String.trim url) then
                envUrl

            else
                url

        finalApiKey =
            if String.isEmpty (String.trim apiKey) then
                envApiKey

            else
                apiKey
    in
    if String.isEmpty (String.trim finalUrl) then
        Just "URL cannot be empty (no environment default available)"

    else if String.isEmpty (String.trim finalApiKey) then
        Just "API key cannot be empty (no environment default available)"

    else if not (String.startsWith "http" finalUrl) then
        Just "URL must start with http:// or https://"

    else if String.length finalApiKey < 10 then
        Just "API key appears to be too short"

    else
        Nothing
