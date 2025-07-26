module ViewAlbums exposing
    ( PropertyChange(..)
    , AssetWithActions
    , AlbumSearch
    , AlbumPagination
    , InputMode(..)
    , viewWithSidebar
    , viewSidebar
    , viewSidebarAlbums
    , viewSidebarAlbumsForCurrentAsset
    , filterToOnlySearchedForAlbums
    , filterAlbumsByKeybinding
    , shouldFilterAlbum
    , getFilteredAlbumsList
    , getFilteredAlbumsListForAsset
    , getSelectedAlbum
    , getSelectedAlbumForAsset
    , getAlbumByExactKeybinding
    , moveSelectionUp
    , moveSelectionDown
    , moveSelectionUpForAsset
    , moveSelectionDownForAsset
    , updateAlbumSearchString
    , getAlbumSearch
    , getAlbumSearchWithHeight
    , getAlbumSearchWithIndex
    , toggleAssetAlbum
    , isAddingToAlbum
    , isCurrentlyInAlbum
    , getAssetWithActions
    , calculateItemsPerPage
    , calculateTotalPages
    , updatePagination
    , resetPagination
    , pageUp
    , pageDown
    , halfPageUp
    , halfPageDown
    , usefulColours
    , shittyFuzzyAlgorithmTest
    , flipPropertyChange
    )

import Dict exposing (Dict)
import Element exposing (Element, alignTop, alignRight, clipY, column, el, fill, fillPortion, height, paddingXY, row, text, width)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Helpers exposing (regexFromString)
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichAsset, ImmichAssetId)
import Regex

-- Types that need to be imported from Main
type PropertyChange
    = RemainTrue
    | RemainFalse
    | ChangeToTrue
    | ChangeToFalse

type alias AssetWithActions =
    { asset : ImmichAsset
    , isFavourite : PropertyChange
    , isArchived : PropertyChange
    , albumMembership : Dict ImmichAlbumId PropertyChange
    }

type alias AlbumSearch =
    { searchString : String
    , albumScores : Dict ImmichAlbumId Int
    , selectedIndex : Int
    , partialKeybinding : String
    , pagination : AlbumPagination
    }

type alias AlbumPagination =
    { currentPage : Int
    , itemsPerPage : Int
    , totalItems : Int
    }

type InputMode
    = NormalMode
    | InsertMode
    | KeybindingMode


-- Helper function for property changes
flipPropertyChange : PropertyChange -> PropertyChange
flipPropertyChange propertyChange =
    case propertyChange of
        RemainTrue ->
            ChangeToFalse
        RemainFalse ->
            ChangeToTrue
        ChangeToTrue ->
            RemainFalse
        ChangeToFalse ->
            RemainTrue

-- Color utility
usefulColours : String -> Element.Color
usefulColours name =
    case name of
        "red" ->
            Element.fromRgb { red = 1, green = 0, blue = 0, alpha = 1 }
        "green" ->
            Element.fromRgb { red = 0, green = 1, blue = 0, alpha = 1 }
        "blue" ->
            Element.fromRgb { red = 0, green = 0, blue = 1, alpha = 1 }
        "grey" ->
            Element.fromRgb { red = 0.8, green = 0.8, blue = 0.8, alpha = 0.6 }
        "darkgrey" ->
            Element.fromRgb { red = 0.2, green = 0.2, blue = 0.2, alpha = 0.4 }
        "black" ->
            Element.fromRgb { red = 0, green = 0, blue = 0, alpha = 1 }
        "white" ->
            Element.fromRgb { red = 1, green = 1, blue = 1, alpha = 1 }
        _ ->
            Element.fromRgb { red = 0, green = 0, blue = 0, alpha = 1 }

-- Layout helper
viewWithSidebar : Element msg -> Element msg -> Element msg
viewWithSidebar sidebarView viewToBeNextToSidebar =
    row [ width fill, height fill ]
        [ el [ width <| fillPortion 4, height fill ] <| viewToBeNextToSidebar
        , el [ width <| fillPortion 1, height fill, alignRight, clipY ] <| sidebarView
        ]

-- Main sidebar view
viewSidebar : AssetWithActions -> AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> Maybe InputMode -> (ImmichAlbum -> msg) -> Element msg
viewSidebar asset search albumKeybindings albums maybeInputMode selectAlbumMsg =
    column [ alignTop, height fill ]
        [ el [ alignTop ] <| text "Asset Changes"
        , if search.searchString /= "" then
            el [ alignTop, Font.color <| usefulColours "blue" ] <| text ("Search: \"" ++ search.searchString ++ "\"")
          else
            Element.none
        , if search.partialKeybinding /= "" then
            el [ alignTop, Font.color <| Element.fromRgb { red = 1, green = 0.5, blue = 0, alpha = 1 } ] <| text ("Keybind: \"" ++ search.partialKeybinding ++ "\"")
          else
            Element.none
        , row [ alignTop ]
            [ case asset.isFavourite of
                ChangeToTrue ->
                    el [ Font.color <| usefulColours "green" ] <| text "Fav"
                ChangeToFalse ->
                    el [ Font.color <| usefulColours "red" ] <| text "!Fav"
                RemainTrue ->
                    el [ Font.color <| usefulColours "grey" ] <| text "Fav"
                RemainFalse ->
                    el [ Font.color <| usefulColours "grey" ] <| text "!Fav"
            , case asset.isArchived of
                ChangeToTrue ->
                    el [ Font.color <| usefulColours "red" ] <| text "Delete"
                ChangeToFalse ->
                    el [ Font.color <| usefulColours "green" ] <| text "Undelete"
                RemainTrue ->
                    el [ Font.color <| usefulColours "grey" ] <| text ""
                RemainFalse ->
                    el [ Font.color <| usefulColours "grey" ] <| text ""
            ]
        , el [ height fill ] <| viewSidebarAlbumsForCurrentAsset asset search albumKeybindings albums maybeInputMode selectAlbumMsg
        ]

-- Sidebar albums view for general album browsing
viewSidebarAlbums : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> (ImmichAlbum -> msg) -> Element msg
viewSidebarAlbums search albumKeybindings albums selectAlbumMsg =
    let
        allFilteredAlbums = Dict.values <| filterToOnlySearchedForAlbums search albumKeybindings albums
        totalItems = List.length allFilteredAlbums
        
        -- Calculate pagination
        startIndex = search.pagination.currentPage * search.pagination.itemsPerPage
        endIndex = startIndex + search.pagination.itemsPerPage
        paginatedAlbums = allFilteredAlbums |> List.drop startIndex |> List.take search.pagination.itemsPerPage
        
        -- Calculate remaining items info
        totalPages = calculateTotalPages totalItems search.pagination.itemsPerPage
        itemsAfter = max 0 (totalItems - endIndex)
        itemsBefore = startIndex
        
        -- Create pagination status row
        paginationStatus = 
            if totalPages > 1 then
                let
                    pageInfo = "Page " ++ String.fromInt (search.pagination.currentPage + 1) ++ " of " ++ String.fromInt totalPages
                    remainingInfo = 
                        if itemsAfter > 0 && itemsBefore > 0 then
                            " (" ++ String.fromInt itemsBefore ++ " above, " ++ String.fromInt itemsAfter ++ " below)"
                        else if itemsAfter > 0 then
                            " (" ++ String.fromInt itemsAfter ++ " more below)"
                        else if itemsBefore > 0 then
                            " (" ++ String.fromInt itemsBefore ++ " above)"
                        else
                            ""
                in
                [ el [ Font.size 12, Font.color <| Element.fromRgb { red = 0.5, green = 0.5, blue = 0.5, alpha = 1 } ] <| 
                    text (pageInfo ++ remainingInfo) ]
            else
                []
        
        albumRows = List.map
            (\album ->
                let
                    keybinding = Dict.get album.id albumKeybindings |> Maybe.withDefault ""
                    isKeybindingMatch = 
                        search.partialKeybinding /= "" && String.startsWith search.partialKeybinding keybinding
                    albumDisplayName = 
                        if keybinding == "" then
                            album.albumName
                        else
                            album.albumName ++ " (" ++ keybinding ++ ")"
                    
                    attrs = 
                        if isKeybindingMatch then
                            [ Background.color <| Element.fromRgb { red = 1, green = 0.8, blue = 0.4, alpha = 0.8 } ]
                        else
                            []
                in
                row [ onClick (selectAlbumMsg album) ]
                    [ el [ paddingXY 5 0 ] <| text (String.fromInt album.assetCount)
                    , el attrs <| text albumDisplayName
                    ]
            ) paginatedAlbums
    in
    column [ height fill ] (paginationStatus ++ albumRows)

-- Sidebar albums view for current asset editing
viewSidebarAlbumsForCurrentAsset : AssetWithActions -> AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> Maybe InputMode -> (ImmichAlbum -> msg) -> Element msg
viewSidebarAlbumsForCurrentAsset asset search albumKeybindings albums maybeInputMode selectAlbumMsg =
    let
        allFilteredAlbums = getFilteredAlbumsListForAsset search albumKeybindings albums asset
        totalItems = List.length allFilteredAlbums
        
        -- Calculate pagination
        startIndex = search.pagination.currentPage * search.pagination.itemsPerPage
        endIndex = startIndex + search.pagination.itemsPerPage
        paginatedAlbums = allFilteredAlbums |> List.drop startIndex |> List.take search.pagination.itemsPerPage
        
        -- Calculate remaining items info
        totalPages = calculateTotalPages totalItems search.pagination.itemsPerPage
        itemsAfter = max 0 (totalItems - endIndex)
        itemsBefore = startIndex
        
        -- Create pagination status row
        paginationStatus = 
            if totalPages > 1 then
                let
                    pageInfo = "Page " ++ String.fromInt (search.pagination.currentPage + 1) ++ " of " ++ String.fromInt totalPages
                    remainingInfo = 
                        if itemsAfter > 0 && itemsBefore > 0 then
                            " (" ++ String.fromInt itemsBefore ++ " above, " ++ String.fromInt itemsAfter ++ " below)"
                        else if itemsAfter > 0 then
                            " (" ++ String.fromInt itemsAfter ++ " more below)"
                        else if itemsBefore > 0 then
                            " (" ++ String.fromInt itemsBefore ++ " above)"
                        else
                            ""
                in
                [ el [ Font.size 12, Font.color <| Element.fromRgb { red = 0.5, green = 0.5, blue = 0.5, alpha = 1 } ] <| 
                    text (pageInfo ++ remainingInfo) ]
            else
                []
        
        albumRows = List.indexedMap
            (\index album ->
                let
                    assetInAlbum =
                        Maybe.withDefault RemainFalse <| Dict.get album.id asset.albumMembership
                    isSelected = (index + startIndex) == search.selectedIndex && maybeInputMode == Just InsertMode
                    baseAttrs =
                        case assetInAlbum of
                            RemainTrue ->
                                [ Background.color <| usefulColours "green" ]
                            RemainFalse ->
                                [ Background.color <| usefulColours "grey" ]
                            ChangeToTrue ->
                                [ Background.color <| usefulColours "blue" ]
                            ChangeToFalse ->
                                [ Background.color <| usefulColours "red" ]
                    attrs = if isSelected then
                                Font.color (usefulColours "white") :: Font.bold :: baseAttrs
                            else
                                baseAttrs
                in
                let
                    keybinding = Dict.get album.id albumKeybindings |> Maybe.withDefault ""
                    isKeybindingMatch = 
                        search.partialKeybinding /= "" && String.startsWith search.partialKeybinding keybinding
                    albumDisplayName = 
                        if keybinding == "" then
                            album.albumName
                        else
                            album.albumName ++ " (" ++ keybinding ++ ")"
                    
                    finalAttrs = 
                        if isKeybindingMatch then
                            (Background.color <| Element.fromRgb { red = 1, green = 0.8, blue = 0.4, alpha = 0.8 }) :: attrs
                        else
                            attrs
                in
                row [ onClick (selectAlbumMsg album) ]
                    [ el [ paddingXY 5 0 ] <| text (String.fromInt album.assetCount)
                    , el finalAttrs <| text (if isSelected then "► " ++ albumDisplayName else albumDisplayName)
                    ]
            ) paginatedAlbums
    in
    column [ height fill ] (paginationStatus ++ albumRows)

-- Filtering functions
filterToOnlySearchedForAlbums : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> Dict ImmichAlbumId ImmichAlbum
filterToOnlySearchedForAlbums search albumKeybindings albums =
    let
        textFiltered = 
            if search.searchString == "" then
                albums
            else
                Dict.filter (\id _ -> shouldFilterAlbum search.albumScores id) albums
    in
    if search.partialKeybinding == "" then
        textFiltered
    else
        filterAlbumsByKeybinding search.partialKeybinding albumKeybindings textFiltered

filterAlbumsByKeybinding : String -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> Dict ImmichAlbumId ImmichAlbum
filterAlbumsByKeybinding partialKeybinding albumKeybindings albums =
    Dict.filter 
        (\albumId _ -> 
            case Dict.get albumId albumKeybindings of
                Just keybinding ->
                    String.startsWith partialKeybinding keybinding
                Nothing ->
                    False
        ) 
        albums

shouldFilterAlbum : Dict ImmichAlbumId Int -> ImmichAlbumId -> Bool
shouldFilterAlbum albumScores albumId =
    case Dict.get albumId albumScores of
        Just score ->
            0 < score
        Nothing ->
            False

-- Album list functions
getFilteredAlbumsList : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> List ImmichAlbum
getFilteredAlbumsList search albumKeybindings albums =
    let
        matchesDict =
            filterToOnlySearchedForAlbums search albumKeybindings albums
    in
    if search.searchString == "" then
        -- When no search, just sort by asset count
        matchesDict
            |> Dict.values
            |> List.sortBy (\album -> -album.assetCount)
    else
        -- When searching, sort by: score > 0 first, then by asset count within each group
        matchesDict
            |> Dict.toList
            |> List.map (\(id, album) -> (Dict.get id search.albumScores |> Maybe.withDefault 0, album))
            |> List.sortBy (\(score, album) -> (if score > 0 then 0 else 1, -album.assetCount))
            |> List.map (\(_, album) -> album)

getFilteredAlbumsListForAsset : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AssetWithActions -> List ImmichAlbum
getFilteredAlbumsListForAsset search albumKeybindings albums asset =
    let
        matchesDict =
            filterToOnlySearchedForAlbums search albumKeybindings albums
    in
    if search.searchString == "" then
        -- When no search, sort by: asset membership first, then by asset count
        matchesDict
            |> Dict.values
            |> List.sortBy (\album -> (if Dict.member album.id asset.albumMembership then 0 else 1, -album.assetCount))
    else
        -- When searching, sort by: score > 0 first, then asset membership, then by asset count
        matchesDict
            |> Dict.toList
            |> List.map (\(id, album) -> (Dict.get id search.albumScores |> Maybe.withDefault 0, album))
            |> List.sortBy (\(score, album) -> 
                ( if score > 0 then 0 else 1
                , if Dict.member album.id asset.albumMembership then 0 else 1
                , -album.assetCount
                ))
            |> List.map (\(_, album) -> album)

-- Selection functions
getSelectedAlbum : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> Maybe ImmichAlbum
getSelectedAlbum search albumKeybindings albums =
    let
        filteredAlbums = getFilteredAlbumsList search albumKeybindings albums
    in
    List.drop search.selectedIndex filteredAlbums
        |> List.head

getSelectedAlbumForAsset : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AssetWithActions -> Maybe ImmichAlbum
getSelectedAlbumForAsset search albumKeybindings albums asset =
    let
        filteredAlbums = getFilteredAlbumsListForAsset search albumKeybindings albums asset
    in
    List.drop search.selectedIndex filteredAlbums
        |> List.head

getAlbumByExactKeybinding : String -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> Maybe ImmichAlbum
getAlbumByExactKeybinding keybinding albumKeybindings albums =
    albumKeybindings
        |> Dict.toList
        |> List.filter (\(_, albumKeybinding) -> albumKeybinding == keybinding)
        |> List.head
        |> Maybe.andThen (\(albumId, _) -> Dict.get albumId albums)

-- Movement functions
moveSelectionUp : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AlbumSearch
moveSelectionUp search albumKeybindings albums =
    { search | selectedIndex = max 0 (search.selectedIndex - 1) }

moveSelectionDown : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AlbumSearch
moveSelectionDown search albumKeybindings albums =
    let
        filteredCount = List.length (getFilteredAlbumsList search albumKeybindings albums)
        maxIndex = max 0 (filteredCount - 1)
    in
    { search | selectedIndex = min maxIndex (search.selectedIndex + 1) }

moveSelectionUpForAsset : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AssetWithActions -> AlbumSearch
moveSelectionUpForAsset search albumKeybindings albums asset =
    { search | selectedIndex = max 0 (search.selectedIndex - 1) }

moveSelectionDownForAsset : AlbumSearch -> Dict ImmichAlbumId String -> Dict ImmichAlbumId ImmichAlbum -> AssetWithActions -> AlbumSearch
moveSelectionDownForAsset search albumKeybindings albums asset =
    let
        filteredCount = List.length (getFilteredAlbumsListForAsset search albumKeybindings albums asset)
        maxIndex = max 0 (filteredCount - 1)
    in
    { search | selectedIndex = min maxIndex (search.selectedIndex + 1) }

-- Album search functions
updateAlbumSearchString : String -> AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> AlbumSearch
updateAlbumSearchString newSearchString oldSearch albums =
    getAlbumSearchWithIndex newSearchString 0 albums

getAlbumSearch : String -> Dict ImmichAssetId ImmichAlbum -> AlbumSearch
getAlbumSearch searchString albums =
    let
        totalItems = Dict.size albums
        itemsPerPage = calculateItemsPerPage 800  -- Default screen height
    in
    { searchString = searchString
    , albumScores =
        Dict.map (\id album -> shittyFuzzyAlgorithmTest searchString album.albumName) albums
    , selectedIndex = 0
    , partialKeybinding = ""
    , pagination = 
        { currentPage = 0
        , itemsPerPage = itemsPerPage
        , totalItems = totalItems
        }
    }

getAlbumSearchWithHeight : String -> Dict ImmichAssetId ImmichAlbum -> Int -> AlbumSearch
getAlbumSearchWithHeight searchString albums screenHeight =
    let
        totalItems = Dict.size albums
        itemsPerPage = calculateItemsPerPage screenHeight
    in
    { searchString = searchString
    , albumScores =
        Dict.map (\id album -> shittyFuzzyAlgorithmTest searchString album.albumName) albums
    , selectedIndex = 0
    , partialKeybinding = ""
    , pagination = 
        { currentPage = 0
        , itemsPerPage = itemsPerPage
        , totalItems = totalItems
        }
    }

getAlbumSearchWithIndex : String -> Int -> Dict ImmichAssetId ImmichAlbum -> AlbumSearch
getAlbumSearchWithIndex searchString selectedIndex albums =
    let
        totalItems = Dict.size albums
        itemsPerPage = calculateItemsPerPage 800  -- Default screen height
    in
    { searchString = searchString
    , albumScores =
        Dict.map (\id album -> shittyFuzzyAlgorithmTest searchString album.albumName) albums
    , selectedIndex = selectedIndex
    , partialKeybinding = ""
    , pagination = 
        { currentPage = 0
        , itemsPerPage = itemsPerPage
        , totalItems = totalItems
        }
    }

-- Asset functions
toggleAssetAlbum : AssetWithActions -> ImmichAlbum -> AssetWithActions
toggleAssetAlbum asset album =
    { asset | albumMembership = Dict.insert album.id (flipPropertyChange <| Maybe.withDefault RemainFalse <| Dict.get album.id asset.albumMembership) asset.albumMembership }

isAddingToAlbum : PropertyChange -> Bool
isAddingToAlbum propertyChange =
    case propertyChange of
        ChangeToTrue -> True
        RemainTrue -> False  -- already in album, not adding
        ChangeToFalse -> False
        RemainFalse -> False

isCurrentlyInAlbum : PropertyChange -> Bool
isCurrentlyInAlbum propertyChange =
    case propertyChange of
        RemainTrue -> True      -- currently in, staying in
        ChangeToFalse -> True   -- currently in, changing to not in  
        RemainFalse -> False    -- currently not in, staying not in
        ChangeToTrue -> False   -- currently not in, changing to in

getAssetWithActions : ImmichAsset -> AssetWithActions
getAssetWithActions asset =
    { asset = asset
    , isFavourite =
        if asset.isFavourite then
            RemainTrue
        else
            RemainFalse
    , isArchived =
        if asset.isArchived then
            RemainTrue
        else
            RemainFalse
    , albumMembership = Dict.fromList <| List.map (\a -> ( a, RemainTrue )) asset.albumMembership
    }

-- Pagination functions
calculateItemsPerPage : Int -> Int
calculateItemsPerPage screenHeight =
    -- Assuming each album item is about 25px tall, with some padding for header/footer
    max 5 ((screenHeight - 100) // 25)

calculateTotalPages : Int -> Int -> Int
calculateTotalPages totalItems itemsPerPage =
    if itemsPerPage == 0 then
        1
    else
        (totalItems + itemsPerPage - 1) // itemsPerPage

updatePagination : Int -> AlbumPagination -> AlbumPagination
updatePagination screenHeight pagination =
    let
        newItemsPerPage = calculateItemsPerPage screenHeight
    in
    { pagination 
    | itemsPerPage = newItemsPerPage
    }

resetPagination : AlbumPagination -> AlbumPagination
resetPagination pagination =
    { pagination | currentPage = 0 }

pageUp : AlbumPagination -> AlbumPagination
pageUp pagination =
    { pagination | currentPage = max 0 (pagination.currentPage - 1) }

pageDown : AlbumPagination -> AlbumPagination
pageDown pagination =
    let
        maxPage = calculateTotalPages pagination.totalItems pagination.itemsPerPage - 1
    in
    { pagination | currentPage = min maxPage (pagination.currentPage + 1) }

halfPageUp : AlbumPagination -> AlbumPagination
halfPageUp pagination =
    let
        halfPage = pagination.itemsPerPage // 2
        newCurrentItem = max 0 (pagination.currentPage * pagination.itemsPerPage - halfPage)
        newPage = newCurrentItem // pagination.itemsPerPage
    in
    { pagination | currentPage = newPage }

halfPageDown : AlbumPagination -> AlbumPagination
halfPageDown pagination =
    let
        halfPage = pagination.itemsPerPage // 2
        maxItems = pagination.totalItems
        newCurrentItem = min (maxItems - 1) (pagination.currentPage * pagination.itemsPerPage + pagination.itemsPerPage + halfPage)
        newPage = newCurrentItem // pagination.itemsPerPage
        maxPage = calculateTotalPages pagination.totalItems pagination.itemsPerPage - 1
    in
    { pagination | currentPage = min maxPage newPage }

-- Fuzzy search algorithm
shittyFuzzyAlgorithmTest : String -> String -> Int
shittyFuzzyAlgorithmTest searchString textToBeSearched =
    let
        patternFzfFuzzy =
            List.foldr (++) ".*" <| List.map (\a -> String.fromChar a ++ ".*") <| String.toList searchString
        regexes =
            [ { score = 10, regex = regexFromString ("$" ++ searchString ++ ".*") }
            , { score = 7, regex = regexFromString ("[^a-z]" ++ searchString ++ ".*") }
            , { score = 5, regex = regexFromString patternFzfFuzzy }
            ]
        
        scoreForRegex regex =
            if Regex.contains regex.regex textToBeSearched then
                regex.score
            else
                0
        
        totalScore = List.sum <| List.map scoreForRegex regexes
    in
    totalScore