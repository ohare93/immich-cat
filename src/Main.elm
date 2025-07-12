module Main exposing (main)

import Browser exposing (element)
import Browser.Events exposing (onKeyDown)
import Date
import Dict exposing (Dict)
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, centerY, clipY, column, el, fill, fillPortion, height, minimum, paddingXY, px, row, text, width)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button)
import Helpers exposing (regexFromString)
import Html exposing (Html, node)
import Html.Attributes
import Immich exposing (ImmichAlbum, ImmichAlbumId, ImmichApiPaths, ImmichAsset, ImmichAssetId, ImmichLoadState(..), getAllAlbums, getImmichApiPaths)
import Json.Decode as Decode
import Regex


type Msg
    = KeyPress String
    | ImmichMsg Immich.Msg
    | LoadDataAgain
    | SelectAlbum ImmichAlbum

type alias Flags =
    { test : Int
    , immichApiKey : String
    , immichApiUrl : String
    }

type alias Model =
    { key : String
    , currentAssetsSource : AssetSource
    , userMode : UserMode
    , test : Int
    , imageIndex : ImageIndex
    , debugging : Bool
    , imageSearchConfig : ImageSearchConfig
    -- Immich fields
    , currentAssets : List ImmichAssetId
    , knownAssets : Dict ImmichAssetId ImmichAsset
    , imagesLoadState : ImmichLoadState
    , knownAlbums : Dict ImmichAlbumId ImmichAlbum
    , albumsLoadState : ImmichLoadState
    , apiUrl : String
    , apiKey : String
    , immichApiPaths : ImmichApiPaths
    }

type ImageOrder
    = Desc
    | Asc
    | Random

type CategorisationFilter
    = All
    | Uncategorised

type alias ImageSearchConfig =
    { order : ImageOrder
    , categorisation : CategorisationFilter
    }

type AssetSource
    = NoAssets
    | ImageSearch ImageSearchConfig
    | TextSearch String
    | Album ImmichAlbum

type alias SourceLoadState =
    { fetchedAssetList : Maybe Bool
    , fetchedAssetMembership : Maybe Bool
    }

type AssetSourceUpdate
    = FetchedAssetList



-- | FetchedAlbums

type UserMode
    = MainMenu
    | SearchAssetInput SearchString
    | SelectAlbumInput AlbumSearch
    | LoadingAssets SourceLoadState
    | EditAsset InputMode AssetWithActions AlbumSearch
    | CreateAlbumConfirmation InputMode AssetWithActions AlbumSearch String
    | ShowEditAssetHelp InputMode AssetWithActions AlbumSearch

type alias SearchString =
    String

type alias ImageIndex =
    Int

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
    }

type InputMode
    = NormalMode
    | InsertMode

type AssetChange
    = ToggleAlbum ImmichAlbum
    | ToggleDelete
    | ToggleFavourite

type TextInputUpdate
    = TextInputAddition String
    | TextInputBackspace

type UserActionGeneral
    = ChangeUserModeToEditAsset
    | ChangeUserModeToLoading AssetSource
    | ChangeUserModeToMainMenu
    | ChangeUserModeToSearchAsset
    | ChangeUserModeToSelectAlbum
    | ReloadData
    | UnknownAction

type UserActionSearchMode
    = TextInputUpdate TextInputUpdate
    | UserActionGeneralSearch UserActionGeneral

type UserActionAlbumSelectMode
    = TextSelectInputUpdate TextInputUpdate
    | SelectAlbumIfMatching
    | MoveSelectionUp
    | MoveSelectionDown
    | UserActionGeneralAlbumSelect UserActionGeneral

type UserActionEditMode
    = ChangeInputMode InputMode
    | ChangeImageIndex Int
    | TextEditModeInputUpdate TextInputUpdate
    | ApplyAlbumIfMatching
    | CreateNewAlbum
    | MoveAlbumSelectionUp
    | MoveAlbumSelectionDown
    | ShowHelp
    | AssetChange AssetChange
    | UserActionGeneralEdit UserActionGeneral

type PropertyChange
    = RemainTrue
    | RemainFalse
    | ChangeToTrue
    | ChangeToFalse

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

propertyChangeToNumber : PropertyChange -> Int
propertyChangeToNumber prop =
    case prop of
        ChangeToTrue ->
            0
        ChangeToFalse ->
            1
        RemainTrue ->
            2
        RemainFalse ->
            3


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { key = ""
      , userMode = MainMenu
      , currentAssetsSource = NoAssets
      , test = flags.test
      , imageIndex = 0
      , debugging = False
      , imageSearchConfig = { order = Desc, categorisation = Uncategorised }
      -- Immich fields
      , currentAssets = []
      , knownAssets = Dict.empty
      , imagesLoadState = ImmichLoading
      , knownAlbums = Dict.empty
      , albumsLoadState = ImmichLoading
      , apiUrl = flags.immichApiUrl
      , apiKey = flags.immichApiKey
      , immichApiPaths = getImmichApiPaths flags.immichApiUrl flags.immichApiKey
      }
      -- , Cmd.none
    , getAllAlbums flags.immichApiUrl flags.immichApiKey |> Cmd.map ImmichMsg
    )

getDebugAssets : ( List ImmichAsset, List ImmichAlbum )
getDebugAssets =
    ( [ ImmichAsset "0001" "images/imafight.jpg" "Imafight" "image/jpg" False False []
      , ImmichAsset "0002" "images/dcemployees.jpg" "dcemployees" "image/jpg" False False []
      , ImmichAsset "0003" "images/jordan.jpg" "Jordan" "image/jpg" False False []
      , ImmichAsset "0004" "images/router-password.mp4" "router password" "video/mp4" False False []
      ]
    , [ ImmichAlbum "a" "J" 200 [] (Date.fromOrdinalDate 2025 1)
      , ImmichAlbum "b" "ToBeSorted" 3000 [] (Date.fromOrdinalDate 2025 1)
      , ImmichAlbum "c" "The World" 50 [] (Date.fromOrdinalDate 2025 1)
      , ImmichAlbum "d" "The Other One" 75 [] (Date.fromOrdinalDate 2025 1)
      , ImmichAlbum "e" "Comics" 50 [] (Date.fromOrdinalDate 2025 1)
      ]
    )


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
        _ ->
            Element.fromRgb { red = 0, green = 0, blue = 0, alpha = 1 }


-- VIEW --
-- showAlbumsForImage : List ImmichAlbum -> ImageWithMetadata -> Html msg
-- showAlbumsForImage albums image =
--     let
--         inAlbumns =
--             List.filter (\album -> List.member album.id image.inAlbumns) albums
--
--         notInAlbumns =
--             List.filter (\album -> not (List.member album.id image.inAlbumns)) albums
--     in
--     div []
--         [ div []
--             [ text ("In Albumns: " ++ String.join ", " (List.map (\album -> album.name) inAlbumns)) ]
--         , div []
--             [ text ("Not In Albumns: " ++ String.join ", " (List.map (\album -> album.name) notInAlbumns)) ]
--         ]


viewAsset : ImmichApiPaths -> ImmichAsset -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> Int -> Element msg
viewAsset apiPaths asset currentAssets knownAssets imageIndex =
    if asset.path == "" then
        el [] (text "No Asset")

    else if assetIsImage asset then
        viewImage asset apiPaths currentAssets knownAssets imageIndex
    else if assetIsVideo asset then
        viewVideo asset apiPaths currentAssets knownAssets imageIndex

    else
        text (String.join " " [ "Error with", asset.title, "Unknown mimetype:", asset.mimeType ])

assetIsImage : ImmichAsset -> Bool
assetIsImage asset =
    List.member asset.mimeType
        [ "image/jpeg"
        , "image/jpg"
        , "image/pjpeg"
        , "image/png"
        , "image/x-png"
        , "image/gif"
        , "image/bmp"
        , "image/x-windows-bmp"
        , "image/webp"
        , "image/heif"
        , "image/heic"
        , "image/tiff"
        , "image/x-tiff"
        , "image/svg+xml"
        , "image/vnd.adobe.photoshop"
        , "image/vnd.microsoft.icon"
        , "image/x-icon"
        , "image/ico"
        , "image/avif"
        , "image/x-canon-cr2"
        , "image/x-canon-crw"
        , "image/x-nikon-nef"
        , "image/x-nikon-nrw"
        , "image/x-sony-arw"
        , "image/x-panasonic-rw2"
        , "image/x-pentax-pef"
        , "image/x-olympus-orf"
        , "image/x-fuji-raf"
        ]

viewImage : ImmichAsset -> ImmichApiPaths -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> Int -> Element msg
viewImage asset apiPaths currentAssets knownAssets imageIndex =
    el [ width fill, height fill ] <|
        Element.html
            (Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "align-items" "center", Html.Attributes.style "justify-content" "center", Html.Attributes.style "height" "calc(100vh - 40px)", Html.Attributes.style "width" "100%", Html.Attributes.style "overflow" "hidden" ]
                [ Html.node "style" [] [ Html.text "image-from-api img { max-width: 100% !important; max-height: calc(100vh - 40px) !important; object-fit: contain !important; width: auto !important; height: auto !important; }" ]
                , node "image-from-api"
                    [ Html.Attributes.attribute "asset-url" (apiPaths.downloadAsset asset.id)
                    , Html.Attributes.attribute "api-key" apiPaths.apiKey
                    , Html.Attributes.attribute "preload-urls" (generatePreloadUrls currentAssets knownAssets apiPaths imageIndex 3)
                    , Html.Attributes.class "center"
                    , Html.Attributes.style "max-width" "100%"
                    , Html.Attributes.style "max-height" "100%"
                    , Html.Attributes.style "object-fit" "contain"
                    , Html.Attributes.style "display" "block"
                    ]
                    []
                ]
            )

generatePreloadUrls : List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> ImmichApiPaths -> Int -> Int -> String
generatePreloadUrls currentAssets knownAssets apiPaths imageIndex count =
    let
        -- Generate indices for next few images and previous few images
        forwardIndices =
            List.range (imageIndex + 1) (imageIndex + count)
        backwardIndices =
            List.range (max 0 (imageIndex - count)) (imageIndex - 1)
        adjacentIndices =
            forwardIndices ++ backwardIndices

        getAssetUrl index =
            if index >= 0 && index < List.length currentAssets then
                currentAssets
                    |> List.drop index
                    |> List.head
                    |> Maybe.andThen (\id -> Dict.get id knownAssets)
                    |> Maybe.map (\asset -> apiPaths.downloadAsset asset.id)
            else
                Nothing

        preloadUrls =
            adjacentIndices
                |> List.filterMap getAssetUrl
                |> List.take (count * 2)

        -- Allow both forward and backward
    in
    String.join "," preloadUrls

assetIsVideo : ImmichAsset -> Bool
assetIsVideo asset =
    List.member asset.mimeType
        [ "video/mp4"
        , "video/quicktime"
        , "video/x-msvideo"
        , "video/msvideo"
        , "video/x-flv"
        , "video/webm"
        , "video/x-matroska"
        , "video/mpeg"
        , "video/x-mpeg"
        , "video/x-mpeq2a"
        , "video/mp4v"
        , "video/ogg"
        , "video/3gpp"
        , "video/x-sgi-movie"
        , "video/avi"
        , "video/mpv2"
        , "video/x-ms-wmv"
        , "video/x-ms-asf"
        ]

viewVideo : ImmichAsset -> ImmichApiPaths -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> Int -> Element msg
viewVideo asset apiPaths currentAssets knownAssets imageIndex =
    el [ width fill, height fill, Html.Attributes.style "position" "relative" |> Element.htmlAttribute ] <|
        Element.html
            (Html.div []
                [ Html.node "style" [] [ Html.text "video-from-api video { width: 100% !important; height: 100% !important; object-fit: contain !important; }" ]
                , node "video-from-api"
                    [ Html.Attributes.attribute "asset-url" (apiPaths.downloadAsset asset.id)
                    , Html.Attributes.attribute "api-key" apiPaths.apiKey
                    , Html.Attributes.attribute "preload-urls" (generatePreloadUrls currentAssets knownAssets apiPaths imageIndex 2)
                    , Html.Attributes.class "center"
                    , Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "height" "calc(100vh - 40px)"
                    , Html.Attributes.style "max-height" "calc(100vh - 40px)"
                    , Html.Attributes.style "max-width" "100%"
                    , Html.Attributes.style "display" "block"
                    , Html.Attributes.style "overflow" "hidden"
                    ]
                    []
                ]
            )


-- viewVideo : List (Element.Attribute msg) -> { poster : String, source : String } -> Element msg
-- viewVideo attrs { poster, source } =
--     el attrs <|
--         Element.html <|
--             Html.video
--                 [ Html.Attributes.attribute "controls" "controls"
--                 , Html.Attributes.preload "none"
--                 -- , Html.Attributes.poster poster
--                 , Html.Attributes.autoplay True
--                 , Html.Attributes.loop True
--                 ]
--                 [ Html.source
--                     [ Html.Attributes.id "mp4"
--                     , Html.Attributes.src source
--                     , Html.Attributes.type_ "video/mp4"
--                     ]
--                     []
--                 ]

viewEditAsset : ImmichApiPaths -> ImageIndex -> Int -> String -> AssetWithActions -> List ImmichAssetId -> Dict ImmichAssetId ImmichAsset -> Element Msg
viewEditAsset apiPaths imageIndex totalAssets viewTitle currentAsset currentAssets knownAssets =
    column [ width fill, height fill ]
        [ el [ alignTop, height (px 20) ]
            (text (String.fromInt (imageIndex + 1) ++ "/" ++ String.fromInt totalAssets ++ "    " ++ viewTitle))
        , el [ width fill, height fill ] <| viewAsset apiPaths currentAsset.asset currentAssets knownAssets imageIndex
        ]

viewLoadingAssets : ImmichLoadState -> Element Msg
viewLoadingAssets imagesLoadState =
    case imagesLoadState of
        ImmichLoading ->
            text "Loading images"
        ImmichLoadSuccess ->
            text "Loaded. Should move states...."
        ImmichLoadError error ->
            let
                errorMessage =
                    Immich.errorToString error
            in
            text errorMessage


view : Model -> Html Msg
view model =
    Element.layout [ width fill, height (fill |> minimum 1), Background.color <| usefulColours "darkgrey" ] <|
        viewWithInputBottomBar model.userMode <|
            viewMainWindow model


viewWithInputBottomBar : UserMode -> Element Msg -> Element Msg
viewWithInputBottomBar userMode viewMain =
    column [ width fill, height fill ]
        [ el [ width fill, height (fill |> minimum 1), clipY ] viewMain
        , el [ width fill, height (px 20) ] <| viewInputMode userMode
        ]


viewMainWindow : Model -> Element Msg
viewMainWindow model =
    case model.userMode of
        MainMenu ->
            row [ width fill, height fill ]
                [ column [ width <| fillPortion 1, height fill, Element.spacingXY 0 20 ]
                    [ text "Select Asset Source"
                    , button [] { onPress = Just LoadDataAgain, label = text "Load albums" }
                    , viewCurrentConfig model.imageSearchConfig
                    ]
                , el [ width <| fillPortion 1, height fill ] <| viewInstructions
                ]
        SearchAssetInput searchString ->
            column []
                [ text "Input Search String"
                , text searchString
                ]
        SelectAlbumInput search ->
            viewWithSidebar
                (viewSidebarAlbums search model.knownAlbums)
                (column []
                    [ text "Select Album"
                    , text search.searchString
                    ]
                )
        LoadingAssets _ ->
            viewLoadingAssets model.imagesLoadState
        EditAsset inputMode asset search ->
            let
                viewTitle =
                    case model.currentAssetsSource of
                        ImageSearch config ->
                            case config.categorisation of
                                Uncategorised ->
                                    "Uncategorised"
                                All ->
                                    "All Images"
                        TextSearch searchText ->
                            "Search : '" ++ searchText ++ "'"
                        Album album ->
                            album.albumName
                        NoAssets ->
                            ""
            in
            viewWithSidebar (viewSidebar asset search model.knownAlbums (Just inputMode)) (viewEditAsset model.immichApiPaths model.imageIndex (Dict.size model.knownAssets) viewTitle asset model.currentAssets model.knownAssets)
        CreateAlbumConfirmation _ asset search albumName ->
            viewWithSidebar (viewSidebar asset search model.knownAlbums Nothing) (viewCreateAlbumConfirmation albumName)
        ShowEditAssetHelp inputMode asset search ->
            viewWithSidebar (viewSidebar asset search model.knownAlbums (Just inputMode)) (viewEditAssetHelp inputMode)

viewCurrentConfig : ImageSearchConfig -> Element msg
viewCurrentConfig config =
    let
        orderText =
            case config.order of
                Desc ->
                    "Descending"
                Asc ->
                    "Ascending"
                Random ->
                    "Random"

        categorisationText =
            case config.categorisation of
                All ->
                    "All images"
                Uncategorised ->
                    "Uncategorised"
    in
    column [ Element.spacingXY 0 8 ]
        [ el [ Font.size 16, Font.bold ] (text "Current Settings:")
        , el [ Font.size 14 ] (text ("Order: " ++ orderText))
        , el [ Font.size 14 ] (text ("Filter: " ++ categorisationText))
        ]

viewCreateAlbumConfirmation : String -> Element msg
viewCreateAlbumConfirmation albumName =
    column [ width fill, height fill, paddingXY 20 20, Element.spacingXY 0 20, centerX, centerY ]
        [ el [ Font.size 18, Font.bold, centerX ] (text "Create New Album")
        , el [ centerX ] (text ("Album name: \"" ++ albumName ++ "\""))
        , el [ Font.size 14, centerX ] (text "Press Enter to create, Escape to cancel")
        ]

viewEditAssetHelp : InputMode -> Element msg
viewEditAssetHelp inputMode =
    column [ width fill, height fill, paddingXY 20 20, Element.spacingXY 0 20, centerX, centerY ]
        [ el [ Font.size 18, Font.bold, centerX ] (text "Asset Navigation Help")
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] <| text "Navigation"
            , viewKeybinding "←" "Previous image"
            , viewKeybinding "→" "Next image" 
            , viewKeybinding "Escape" "Return to main menu"
            ]
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] <| text "Asset Actions"
            , viewKeybinding "d" "Toggle delete/archive"
            , viewKeybinding "f" "Toggle favorite"
            , viewKeybinding "i" "Enter album search mode"
            ]
        , if inputMode == InsertMode then
            column [ Element.spacingXY 0 8 ]
                [ el [ Font.size 16, Font.bold ] <| text "Album Search (Insert Mode)"
                , viewKeybinding "Type" "Search albums by name"
                , viewKeybinding "↑↓" "Navigate through results"
                , viewKeybinding "Enter" "Add to highlighted album"
                , viewKeybinding "Tab" "Create new album"
                , viewKeybinding "Click" "Click album to add"
                , viewKeybinding "Escape" "Exit search mode"
                ]
          else
            Element.none
        , el [ Font.size 14, centerX ] (text "Press ? or Escape to close help")
        ]

viewInstructions : Element msg
viewInstructions =
    column [ width fill, height fill, paddingXY 20 20, Element.spacingXY 0 10 ]
        [ el [ Font.size 18, Font.bold ] <| text "Keybindings"
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] <| text "Main Menu"
            , viewKeybinding "o" "Cycle order (desc/asc/random)"
            , viewKeybinding "c" "Cycle categorisation (all/uncategorised)"
            , viewKeybinding "l" "Load with current settings"
            , viewKeybinding "a" "Browse and select albums"
            , viewKeybinding "s" "Search assets"
            ]
        , column [ Element.spacingXY 0 8 ]
            [ el [ Font.size 16, Font.bold ] <| text "Asset Navigation (Normal Mode)"
            , viewKeybinding "←" "Previous image"
            , viewKeybinding "→" "Next image"
            , viewKeybinding "Escape" "Return to main menu"
            , viewKeybinding "i" "Enter insert mode (album search)"
            , viewKeybinding "d" "Toggle delete/archive"
            , viewKeybinding "f" "Toggle favorite"
            , viewKeybinding "?" "Show help"
            ]
        ]

viewKeybinding : String -> String -> Element msg
viewKeybinding key description =
    row [ width fill, Element.spacingXY 10 0 ]
        [ el [ width <| Element.px 120, Font.family [ Font.monospace ], Background.color <| usefulColours "grey", paddingXY 8 4 ] <| text key
        , el [ width fill ] <| text description
        ]

viewInputMode : UserMode -> Element msg
viewInputMode userMode =
    let
        inputMode =
            case userMode of
                MainMenu ->
                    NormalMode
                SearchAssetInput _ ->
                    InsertMode
                SelectAlbumInput _ ->
                    InsertMode
                LoadingAssets _ ->
                    NormalMode
                EditAsset editInputMode _ _ ->
                    editInputMode
                CreateAlbumConfirmation editInputMode _ _ _ ->
                    editInputMode
                ShowEditAssetHelp editInputMode _ _ ->
                    editInputMode
    in
    case inputMode of
        NormalMode ->
            el [ width fill, Background.color <| Element.fromRgb { red = 0.8, green = 0.8, blue = 0.8, alpha = 1 } ] <| text "Normal"
        InsertMode ->
            el [ width fill, Background.color <| Element.fromRgb { red = 0, green = 1, blue = 0, alpha = 1 } ] <| text "Input"

viewWithSidebar : Element Msg -> Element Msg -> Element Msg
viewWithSidebar sidebarView viewToBeNextToSidebar =
    row [ width fill, height fill ]
        [ el [ width <| fillPortion 4, height fill ] <| viewToBeNextToSidebar
        , el [ width <| fillPortion 1, height fill, alignRight, clipY ] <| sidebarView
        ]

viewSidebar : AssetWithActions -> AlbumSearch -> Dict ImmichAssetId ImmichAlbum -> Maybe InputMode -> Element Msg
viewSidebar asset search albums maybeInputMode =
    column [ alignTop, height fill ]
        [ el [ alignTop ] <| text "Asset Changes"
        , if search.searchString /= "" then
            el [ alignTop, Font.color <| usefulColours "blue" ] <| text ("Search: \"" ++ search.searchString ++ "\"")
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
        , el [ height fill ] <| viewSidebarAlbumsForCurrentAsset asset search albums maybeInputMode
        ]


viewSidebarAlbums : AlbumSearch -> Dict ImmichAssetId ImmichAlbum -> Element Msg
viewSidebarAlbums search albums =
    column [ height fill ]
        (List.map
            (\album ->
                row [ onClick (SelectAlbum album) ]
                    [ el [ paddingXY 5 0 ] <| text (String.fromInt album.assetCount)
                    , el [] <| text album.albumName
                    ]
            )
         <|
            List.take 40 <|
                Dict.values <|
                    filterToOnlySearchedForAlbums search albums
        )

viewSidebarAlbumsForCurrentAsset : AssetWithActions -> AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> Maybe InputMode -> Element Msg
viewSidebarAlbumsForCurrentAsset asset search albums maybeInputMode =
    let
        filteredAlbums = List.take 40 (getFilteredAlbumsListForAsset search albums asset)
    in
    column [ height fill ]
        (List.indexedMap
            (\index album ->
                let
                    assetInAlbum =
                        Maybe.withDefault RemainFalse <| Dict.get album.id asset.albumMembership
                    isSelected = index == search.selectedIndex && maybeInputMode == Just InsertMode
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
                row [ onClick (SelectAlbum album) ]
                    [ el [ paddingXY 5 0 ] <| text (String.fromInt album.assetCount)
                    , el attrs <| text (if isSelected then "► " ++ album.albumName else album.albumName)
                    ]
            )
            filteredAlbums
        )

filterToOnlySearchedForAlbums : AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> Dict ImmichAlbumId ImmichAlbum
filterToOnlySearchedForAlbums search albums =
    if search.searchString == "" then
        albums
    else
        Dict.filter (\id _ -> shouldFilterAlbum search.albumScores id) albums

shouldFilterAlbum : Dict ImmichAlbumId Int -> ImmichAlbumId -> Bool
shouldFilterAlbum albumScores albumId =
    case Dict.get albumId albumScores of
        Just score ->
            0 < score

        Nothing ->
            False


-- UPDATE --


loopImageIndexOverArray : ImageIndex -> Int -> Int -> ImageIndex
loopImageIndexOverArray index step length =
    modBy length (index + step)


isSupportedSearchLetter : String -> Bool
isSupportedSearchLetter testString =
    let
        regex =
            Regex.fromString "^[a-zA-Z0-9 ]$" |> Maybe.withDefault Regex.never
    in
    Regex.contains regex testString


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadDataAgain ->
            ( model, Immich.getAllAlbums model.apiUrl model.apiKey |> Cmd.map ImmichMsg )
        SelectAlbum album ->
            case model.userMode of
                SelectAlbumInput _ ->
                    ( createLoadStateForCurrentAssetSource (Album album) model, Immich.getAlbum model.immichApiPaths album.id |> Cmd.map ImmichMsg )
                EditAsset inputMode asset search ->
                    let
                        isNotInAlbum =
                            case Dict.get album.id asset.albumMembership of
                                Nothing ->
                                    True
                                Just _ ->
                                    False
                    in
                    ( { model | userMode = EditAsset inputMode (toggleAssetAlbum asset album) (getAlbumSearch "" model.knownAlbums) }, Immich.albumChangeAssetMembership model.immichApiPaths album.id [ asset.asset.id ] isNotInAlbum |> Cmd.map ImmichMsg )
                _ ->
                    ( model, Cmd.none )
        KeyPress key ->
            handleUserInput model key
        ImmichMsg imsg ->
            let
                newModel =
                    case imsg of
                        Immich.SingleAlbumFetched (Ok album) ->
                            model
                                |> handleFetchAlbums [ album ]
                                |> handleFetchAssets album.assets
                                -- |> handleProgressLoadingState FetchedAlbums
                                |> handleUpdateLoadingState FetchedAssetList
                        Immich.AlbumsFetched (Ok albums) ->
                            model
                                |> handleFetchAlbums albums
                        Immich.AlbumCreated (Ok album) ->
                            let
                                updatedModel =
                                    model
                                        |> handleFetchAlbums [ album ]
                            in
                            updatedModel

                        -- |> handleProgressLoadingState FetchedAlbums
                        Immich.ImagesFetched (Ok assets) ->
                            model
                                |> handleFetchAssets assets
                                |> handleUpdateLoadingState FetchedAssetList

                        Immich.AssetMembershipFetched (Ok assetWithMembership) ->
                            model
                                |> handleFetchAssetMembership assetWithMembership

                        Immich.AlbumsFetched (Err error) ->
                            { model | albumsLoadState = ImmichLoadError error }
                        Immich.AlbumCreated (Err error) ->
                            case model.userMode of
                                LoadingAssets _ ->
                                    getCurrentAssetWithActions model
                                        |> Maybe.map (\( assetWithActions, search ) -> { model | userMode = EditAsset InsertMode assetWithActions search })
                                        |> Maybe.withDefault model
                                _ ->
                                    model
                        Immich.ImagesFetched (Err error) ->
                            { model | imagesLoadState = ImmichLoadError error }
                        _ ->
                            model
            in
            case imsg of
                Immich.AlbumAssetsChanged (Ok _) ->
                    -- Album membership change succeeded, keep current UI state
                    ( newModel, Cmd.none )
                Immich.AlbumAssetsChanged (Err _) ->
                    -- Album membership change failed, re-fetch to get correct state
                    switchToEditIfAssetFound model model.imageIndex
                Immich.AlbumCreated (Ok album) ->
                    case model.userMode of
                        LoadingAssets _ ->
                            getCurrentAssetWithActions newModel
                                |> Maybe.map
                                    (\( assetWithActions, _ ) ->
                                        let
                                            updatedAsset =
                                                toggleAssetAlbum assetWithActions album
                                            updatedModel =
                                                { newModel | userMode = EditAsset InsertMode updatedAsset (getAlbumSearch "" newModel.knownAlbums) }
                                        in
                                        ( updatedModel, Immich.albumChangeAssetMembership newModel.immichApiPaths album.id [ assetWithActions.asset.id ] True |> Cmd.map ImmichMsg )
                                    )
                                |> Maybe.withDefault ( newModel, Cmd.none )
                        _ ->
                            ( newModel, Cmd.none )
                _ ->
                    checkIfLoadingComplete newModel

handleFetchAssetMembership : Immich.AssetWithMembership -> Model -> Model
handleFetchAssetMembership assetWithMembership model =
    case Dict.get assetWithMembership.assetId model.knownAssets of
        Nothing ->
            model

        Just asset ->
            let
                newAsset =
                    { asset | albumMembership = assetWithMembership.albumIds }
            in
            Tuple.first <| switchToEditIfAssetFound { model | knownAssets = Dict.insert assetWithMembership.assetId newAsset model.knownAssets } model.imageIndex


handleFetchAssets : List ImmichAsset -> Model -> Model
handleFetchAssets assets model =
    { model | knownAssets = Helpers.listOverrideDict assets (\a -> ( a.id, a )) model.knownAssets, currentAssets = List.map .id assets, imagesLoadState = ImmichLoadSuccess }

handleFetchAlbums : List ImmichAlbum -> Model -> Model
handleFetchAlbums albums model =
    { model | knownAlbums = Helpers.listOverrideDict albums (\a -> ( a.id, a )) model.knownAlbums, albumsLoadState = ImmichLoadSuccess }

handleUpdateLoadingState : AssetSourceUpdate -> Model -> Model
handleUpdateLoadingState updateType model =
    -- Use the event to update the Loading AssetLoadState
    -- Check if all the flags are now good, if so call progressToEditMode
    case model.userMode of
        LoadingAssets loadState ->
            let
                updatedLoadState =
                    case updateType of
                        FetchedAssetList ->
                            { loadState | fetchedAssetList = Just True }
                -- FetchedAlbums ->
                --     { loadState | fetchedAssetList = Just True }
                updatedModel =
                    { model | userMode = LoadingAssets updatedLoadState }
            in
            updatedModel
        _ ->
            model

checkIfLoadingComplete : Model -> ( Model, Cmd Msg )
checkIfLoadingComplete model =
    case model.userMode of
        LoadingAssets loadState ->
            if isLoadStateCompleted loadState then
                switchToEditIfAssetFound model 0
            else
                ( model, Cmd.none )
        _ ->
            ( model, Cmd.none )

isLoadStateCompleted : SourceLoadState -> Bool
isLoadStateCompleted loadState =
    isLoadCompletedForProp loadState.fetchedAssetMembership
        && isLoadCompletedForProp loadState.fetchedAssetList

isLoadCompletedForProp : Maybe Bool -> Bool
isLoadCompletedForProp maybeBool =
    maybeBool == Nothing || maybeBool == Just True

createLoadStateForCurrentAssetSource : AssetSource -> Model -> Model
createLoadStateForCurrentAssetSource assetSource model =
    case assetSource of
        NoAssets ->
            model
        ImageSearch _ ->
            { model | currentAssetsSource = assetSource, userMode = LoadingAssets { fetchedAssetList = Just False, fetchedAssetMembership = Nothing } }
        Album _ ->
            { model | currentAssetsSource = assetSource, userMode = LoadingAssets { fetchedAssetList = Just False, fetchedAssetMembership = Nothing } }
        TextSearch _ ->
            { model | currentAssetsSource = assetSource, userMode = LoadingAssets { fetchedAssetList = Just False, fetchedAssetMembership = Nothing } }


handleUserInput : Model -> String -> ( Model, Cmd Msg )
handleUserInput model key =
    case model.userMode of
        MainMenu ->
            case key of
                "l" ->
                    let
                        immichOrder =
                            case model.imageSearchConfig.order of
                                Desc ->
                                    Immich.Desc
                                Asc ->
                                    Immich.Asc
                                Random ->
                                    Immich.Random
                        immichCategorisation =
                            case model.imageSearchConfig.categorisation of
                                All ->
                                    Immich.All
                                Uncategorised ->
                                    Immich.Uncategorised
                        config =
                            { order = immichOrder, categorisation = immichCategorisation }
                    in
                    ( applyGeneralAction model (ChangeUserModeToLoading (ImageSearch model.imageSearchConfig)), Immich.fetchImages model.immichApiPaths config |> Cmd.map ImmichMsg )
                "o" ->
                    let
                        newOrder =
                            case model.imageSearchConfig.order of
                                Desc ->
                                    Asc
                                Asc ->
                                    Random
                                Random ->
                                    Desc
                        newConfig =
                            { order = newOrder, categorisation = model.imageSearchConfig.categorisation }
                    in
                    ( { model | imageSearchConfig = newConfig }, Cmd.none )
                "c" ->
                    let
                        newCategorisation =
                            case model.imageSearchConfig.categorisation of
                                All ->
                                    Uncategorised
                                Uncategorised ->
                                    All
                        newConfig =
                            { order = model.imageSearchConfig.order, categorisation = newCategorisation }
                    in
                    ( { model | imageSearchConfig = newConfig }, Cmd.none )
                "a" ->
                    ( applyGeneralAction model ChangeUserModeToSelectAlbum, Cmd.none )
                "s" ->
                    ( applyGeneralAction model ChangeUserModeToSearchAsset, Cmd.none )
                _ ->
                    ( model, Cmd.none )
        SearchAssetInput searchString ->
            let
                userAction =
                    if isSupportedSearchLetter key then
                        TextInputUpdate (TextInputAddition key)
                    else
                        case key of
                            "Backspace" ->
                                TextInputUpdate TextInputBackspace
                            "Escape" ->
                                UserActionGeneralSearch <| ChangeUserModeToMainMenu
                            "Enter" ->
                                UserActionGeneralSearch <| ChangeUserModeToLoading (TextSearch searchString)
                            _ ->
                                UserActionGeneralSearch UnknownAction
            in
            case userAction of
                TextInputUpdate (TextInputAddition newKey) ->
                    let
                        newSearchString =
                            searchString ++ newKey
                    in
                    ( { model | userMode = SearchAssetInput newSearchString }, Cmd.none )
                TextInputUpdate TextInputBackspace ->
                    let
                        newSearchString =
                            String.slice 0 (String.length searchString - 1) searchString
                    in
                    ( { model | userMode = SearchAssetInput newSearchString }, Cmd.none )
                UserActionGeneralSearch action ->
                    ( applyGeneralAction model action, Cmd.none )
        SelectAlbumInput searchResults ->
            let
                userAction =
                    if isSupportedSearchLetter key then
                        TextSelectInputUpdate (TextInputAddition key)
                    else
                        case key of
                            "Backspace" ->
                                TextSelectInputUpdate TextInputBackspace
                            "Escape" ->
                                UserActionGeneralAlbumSelect ChangeUserModeToMainMenu
                            "Enter" ->
                                SelectAlbumIfMatching
                            "ArrowUp" ->
                                MoveSelectionUp
                            "ArrowDown" ->
                                MoveSelectionDown
                            _ ->
                                UserActionGeneralAlbumSelect UnknownAction
            in
            case userAction of
                TextSelectInputUpdate (TextInputAddition newKey) ->
                    let
                        newSearchString =
                            searchResults.searchString ++ newKey
                    in
                    ( { model | userMode = SelectAlbumInput <| updateAlbumSearchString newSearchString searchResults model.knownAlbums }, Cmd.none )
                TextSelectInputUpdate TextInputBackspace ->
                    let
                        newSearchString =
                            String.slice 0 (String.length searchResults.searchString - 1) searchResults.searchString
                    in
                    ( { model | userMode = SelectAlbumInput <| updateAlbumSearchString newSearchString searchResults model.knownAlbums }, Cmd.none )
                SelectAlbumIfMatching ->
                    let
                        maybeMatch =
                            getSelectedAlbum searchResults model.knownAlbums
                    in
                    case maybeMatch of
                        Just album ->
                            ( createLoadStateForCurrentAssetSource (Album album) model, Immich.getAlbum model.immichApiPaths album.id |> Cmd.map ImmichMsg )
                        Nothing ->
                            ( model, Cmd.none )
                MoveSelectionUp ->
                    ( { model | userMode = SelectAlbumInput <| moveSelectionUp searchResults model.knownAlbums }, Cmd.none )
                MoveSelectionDown ->
                    ( { model | userMode = SelectAlbumInput <| moveSelectionDown searchResults model.knownAlbums }, Cmd.none )
                UserActionGeneralAlbumSelect action ->
                    ( applyGeneralAction model action, Cmd.none )
        LoadingAssets _ ->
            ( model, Cmd.none )
        EditAsset inputMode asset search ->
            let
                userAction =
                    if inputMode == InsertMode then
                        if isSupportedSearchLetter key then
                            TextEditModeInputUpdate (TextInputAddition key)
                        else
                            case key of
                                "Escape" ->
                                    ChangeInputMode NormalMode
                                "Backspace" ->
                                    TextEditModeInputUpdate TextInputBackspace
                                "Enter" ->
                                    ApplyAlbumIfMatching
                                "Tab" ->
                                    CreateNewAlbum
                                "ArrowUp" ->
                                    MoveAlbumSelectionUp
                                "ArrowDown" ->
                                    MoveAlbumSelectionDown
                                "?" ->
                                    ShowHelp
                                _ ->
                                    UserActionGeneralEdit UnknownAction
                    else
                        case key of
                            "ArrowLeft" ->
                                ChangeImageIndex -1
                            "ArrowRight" ->
                                ChangeImageIndex 1
                            "Escape" ->
                                UserActionGeneralEdit <| ChangeUserModeToMainMenu
                            -- "Backspace" ->
                            --     RemoveFromAssetChangeList
                            "i" ->
                                ChangeInputMode InsertMode
                            "d" ->
                                AssetChange ToggleDelete
                            "f" ->
                                AssetChange ToggleFavourite
                            "?" ->
                                ShowHelp
                            _ ->
                                UserActionGeneralEdit UnknownAction
            in
            case userAction of
                AssetChange ToggleFavourite ->
                    let
                        newAsset =
                            { asset | isFavourite = flipPropertyChange asset.isFavourite }
                    in
                    ( { model | userMode = EditAsset inputMode newAsset search }, Cmd.none )
                AssetChange ToggleDelete ->
                    let
                        newAsset =
                            { asset | isArchived = flipPropertyChange asset.isArchived }
                    in
                    ( { model | userMode = EditAsset inputMode newAsset search }, Cmd.none )
                AssetChange (ToggleAlbum album) ->
                    ( { model | userMode = EditAsset inputMode (toggleAssetAlbum asset album) search }, Cmd.none )
                ApplyAlbumIfMatching ->
                    let
                        maybeMatch =
                            getSelectedAlbumForAsset search model.knownAlbums asset
                    in
                    case maybeMatch of
                        Just album ->
                            let
                                isNotInAlbum =
                                    case Dict.get album.id asset.albumMembership of
                                        Nothing ->
                                            True
                                        Just _ ->
                                            False
                            in
                            ( { model | userMode = EditAsset inputMode (toggleAssetAlbum asset album) (getAlbumSearch "" model.knownAlbums) }, Immich.albumChangeAssetMembership model.immichApiPaths album.id [ asset.asset.id ] isNotInAlbum |> Cmd.map ImmichMsg )
                        Nothing ->
                            if String.trim search.searchString /= "" then
                                ( { model | userMode = CreateAlbumConfirmation inputMode asset search (String.trim search.searchString) }, Cmd.none )
                            else
                                ( model, Cmd.none )
                CreateNewAlbum ->
                    if String.trim search.searchString /= "" then
                        ( { model | userMode = CreateAlbumConfirmation inputMode asset search (String.trim search.searchString) }, Cmd.none )
                    else
                        ( model, Cmd.none )
                ChangeInputMode newInputMode ->
                    ( { model | userMode = EditAsset newInputMode asset <| getAlbumSearch "" model.knownAlbums }, Cmd.none )
                ChangeImageIndex indexChange ->
                    let
                        newIndex =
                            loopImageIndexOverArray model.imageIndex indexChange (List.length model.currentAssets)
                    in
                    switchToEditIfAssetFound model newIndex
                TextEditModeInputUpdate (TextInputAddition newKey) ->
                    let
                        newSearchString =
                            search.searchString ++ newKey
                    in
                    ( { model | userMode = EditAsset inputMode asset <| updateAlbumSearchString newSearchString search model.knownAlbums }, Cmd.none )
                TextEditModeInputUpdate TextInputBackspace ->
                    let
                        newSearchString =
                            String.slice 0 (String.length search.searchString - 1) search.searchString
                    in
                    ( { model | userMode = EditAsset inputMode asset <| updateAlbumSearchString newSearchString search model.knownAlbums }, Cmd.none )

                MoveAlbumSelectionUp ->
                    ( { model | userMode = EditAsset inputMode asset <| moveSelectionUpForAsset search model.knownAlbums asset }, Cmd.none )
                MoveAlbumSelectionDown ->
                    ( { model | userMode = EditAsset inputMode asset <| moveSelectionDownForAsset search model.knownAlbums asset }, Cmd.none )
                ShowHelp ->
                    ( { model | userMode = ShowEditAssetHelp inputMode asset search }, Cmd.none )
                UserActionGeneralEdit generalAction ->
                    ( applyGeneralAction model generalAction, Cmd.none )

        CreateAlbumConfirmation inputMode asset search albumName ->
            case key of
                "Enter" ->
                    ( { model | userMode = LoadingAssets { fetchedAssetList = Nothing, fetchedAssetMembership = Nothing } }, Immich.createAlbum model.immichApiPaths albumName |> Cmd.map ImmichMsg )
                "Escape" ->
                    ( { model | userMode = EditAsset inputMode asset search }, Cmd.none )
                _ ->
                    ( model, Cmd.none )

        ShowEditAssetHelp inputMode asset search ->
            case key of
                "Escape" ->
                    ( { model | userMode = EditAsset inputMode asset search }, Cmd.none )
                "?" ->
                    ( { model | userMode = EditAsset inputMode asset search }, Cmd.none )
                _ ->
                    ( model, Cmd.none )

getTopMatchToSearch : AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> Maybe ImmichAlbum
getTopMatchToSearch search albums =
    let
        matchesDict =
            filterToOnlySearchedForAlbums search albums
    in
    if Dict.isEmpty matchesDict then
        Nothing
    else
        -- Find the album with the highest score
        matchesDict
            |> Dict.toList
            |> List.map (\(id, album) -> (Dict.get id search.albumScores |> Maybe.withDefault 0, album))
            |> List.sortBy (\(score, _) -> -score)  -- Sort by score descending
            |> List.head
            |> Maybe.map (\(_, album) -> album)

-- Helper functions for album selection navigation
getFilteredAlbumsList : AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> List ImmichAlbum
getFilteredAlbumsList search albums =
    let
        matchesDict =
            filterToOnlySearchedForAlbums search albums
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

getFilteredAlbumsListForAsset : AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> AssetWithActions -> List ImmichAlbum
getFilteredAlbumsListForAsset search albums asset =
    let
        matchesDict =
            filterToOnlySearchedForAlbums search albums
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

getSelectedAlbum : AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> Maybe ImmichAlbum
getSelectedAlbum search albums =
    let
        filteredAlbums = getFilteredAlbumsList search albums
    in
    List.drop search.selectedIndex filteredAlbums
        |> List.head

moveSelectionUp : AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> AlbumSearch
moveSelectionUp search albums =
    { search | selectedIndex = max 0 (search.selectedIndex - 1) }

moveSelectionDown : AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> AlbumSearch
moveSelectionDown search albums =
    let
        filteredCount = List.length (getFilteredAlbumsList search albums)
        maxIndex = max 0 (filteredCount - 1)
    in
    { search | selectedIndex = min maxIndex (search.selectedIndex + 1) }

moveSelectionDownForAsset : AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> AssetWithActions -> AlbumSearch
moveSelectionDownForAsset search albums asset =
    let
        filteredCount = List.length (getFilteredAlbumsListForAsset search albums asset)
        maxIndex = max 0 (filteredCount - 1)
    in
    { search | selectedIndex = min maxIndex (search.selectedIndex + 1) }

moveSelectionUpForAsset : AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> AssetWithActions -> AlbumSearch
moveSelectionUpForAsset search albums asset =
    { search | selectedIndex = max 0 (search.selectedIndex - 1) }

getSelectedAlbumForAsset : AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> AssetWithActions -> Maybe ImmichAlbum
getSelectedAlbumForAsset search albums asset =
    let
        filteredAlbums = getFilteredAlbumsListForAsset search albums asset
    in
    List.drop search.selectedIndex filteredAlbums
        |> List.head

updateAlbumSearchString : String -> AlbumSearch -> Dict ImmichAlbumId ImmichAlbum -> AlbumSearch
updateAlbumSearchString newSearchString oldSearch albums =
    getAlbumSearchWithIndex newSearchString 0 albums

toggleAssetAlbum : AssetWithActions -> ImmichAlbum -> AssetWithActions
toggleAssetAlbum asset album =
    { asset | albumMembership = Dict.insert album.id (flipPropertyChange <| Maybe.withDefault RemainFalse <| Dict.get album.id asset.albumMembership) asset.albumMembership }


applyGeneralAction : Model -> UserActionGeneral -> Model
applyGeneralAction model action =
    case action of
        ChangeUserModeToMainMenu ->
            { model | userMode = MainMenu }
        ChangeUserModeToSearchAsset ->
            { model | userMode = SearchAssetInput "" }
        ChangeUserModeToSelectAlbum ->
            { model | userMode = SelectAlbumInput <| getAlbumSearch "" model.knownAlbums }
        ChangeUserModeToEditAsset ->
            Tuple.first <| switchToEditIfAssetFound model 0
        ChangeUserModeToLoading assetSource ->
            createLoadStateForCurrentAssetSource assetSource model

        ReloadData ->
            { model | imagesLoadState = ImmichLoading, albumsLoadState = ImmichLoading }

        UnknownAction ->
            model

switchToEditIfAssetFound : Model -> ImageIndex -> ( Model, Cmd Msg )
switchToEditIfAssetFound model index =
    model.currentAssets
        |> List.drop index
        |> List.head
        |> Maybe.andThen (\id -> Dict.get id model.knownAssets)
        |> Maybe.map
            (\asset ->
                let
                    cmdToSend =
                        -- if List.isEmpty asset.albumMembership then
                        Immich.fetchMembershipForAsset model.immichApiPaths asset.id |> Cmd.map ImmichMsg

                    -- else
                    --     Cmd.none
                in
                ( { model | imageIndex = index, userMode = EditAsset NormalMode (getAssetWithActions asset) (getAlbumSearch "" model.knownAlbums) }, cmdToSend )
            )
        |> Maybe.withDefault ( createLoadStateForCurrentAssetSource model.currentAssetsSource model, Cmd.none )


getCurrentAssetWithActions : Model -> Maybe ( AssetWithActions, AlbumSearch )
getCurrentAssetWithActions model =
    model.currentAssets
        |> List.drop model.imageIndex
        |> List.head
        |> Maybe.andThen (\id -> Dict.get id model.knownAssets)
        |> Maybe.map (\asset -> ( getAssetWithActions asset, getAlbumSearch "" model.knownAlbums ))

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

getAlbumSearch : String -> Dict ImmichAssetId ImmichAlbum -> AlbumSearch
getAlbumSearch searchString albums =
    { searchString = searchString
    , albumScores =
        Dict.map (\id album -> shittyFuzzyAlgorithmTest searchString album.albumName) albums
    , selectedIndex = 0
    }

getAlbumSearchWithIndex : String -> Int -> Dict ImmichAssetId ImmichAlbum -> AlbumSearch
getAlbumSearchWithIndex searchString selectedIndex albums =
    { searchString = searchString
    , albumScores =
        Dict.map (\id album -> shittyFuzzyAlgorithmTest searchString album.albumName) albums
    , selectedIndex = selectedIndex
    }



-- { searchString = searchString
-- , albumsWithScore =
--     albumsWithScores
--         |> List.filter (\a -> a.score > 0 || searchString == "")
--         |> List.sortBy (\a -> ( a.score, a.album.assetCount ))
--         |> List.reverse
-- }

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
    in
    if searchString == "" then
        0
    else
        List.filter (\a -> Regex.contains a.regex textToBeSearched) regexes |> List.map .score |> List.foldr (+) 0



-- SUBSCRIPTIONS --

subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown (Decode.map KeyPress (Decode.field "key" Decode.string))


main : Program Flags Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
