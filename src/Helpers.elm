module Helpers exposing
    ( categorisationToString
    , filterByMediaType
    , filterByStatus
    , isKeybindingLetter
    , isSupportedSearchLetter
    , listOverrideDict
    , loopImageIndexOverArray
    , mediaTypeToString
    , orderToString
    , regexFromString
    , statusToString
    , toggleCategorisation
    , toggleMediaType
    , toggleOrder
    , toggleStatus
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
