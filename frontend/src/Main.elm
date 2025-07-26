module Main exposing (main)
import Array exposing (Array)
import Browser exposing (element)
import Browser.Events exposing (onKeyDown)
import Date
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Immich exposing (ImageWithMetadata, ImmichAlbum)
import Json.Decode as Decode


type Msg
    = Increment
    | KeyPress String


type alias Model =
    { count : Int
    , key : String
    , imagePrepend : String
    , albums : List ImmichAlbum
    , images : Array ImageWithMetadata
    , state : UserState
    }

type UserState
    = ViewingState Int (Maybe ImageWithMetadata)
    | ErrorState


init : () -> ( Model, Cmd Msg )
init _ =
    let
        testImages =
            Array.fromList
                [ ImageWithMetadata "0001"
                    "images/imafight.jpg"
                    "Imafight"
                    "jpg"
                , ImageWithMetadata
                    "0002"
                    "images/dcemployees.jpg"
                    "dcemployees"
                    "jpg"
                , ImageWithMetadata
                    "0003"
                    "images/jordan.jpg"
                    "Jordan"
                    "jpg"
                ]

        -- , { id = "0003", url = "", title = "Image C", inAlbumns = [ "a", "b" ] }
        -- ]
        testAlbums =
            [ ImmichAlbum "a" "Test" 50 [] "000001" (Date.fromOrdinalDate 2025 1)
            ]
    in
    ( { count = 7
      , key = ""
      , albums = testAlbums
      , imagePrepend = "http://localhost:3333/"
      -- , albums =
      --       [ { id = "a", name = "Album A" }
      --       , { id = "b", name = "Album B" }
      --       , { id = "c", name = "Album C" }
      --       , { id = "d", name = "Album D" }
      --       , { id = "e", name = "Album E" }
      --       ]
      , images = testImages
      , state = ViewingState 0 Nothing
      }
    , Cmd.none
    )



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


imageOrBlank : String -> Html msg
imageOrBlank path =
    if path == "" then
        text ""

    else
        img [ src path, class "img-fluid" ] []


viewImage : String -> Maybe ImageWithMetadata -> Html msg
viewImage imagePrepend maybeImage =
    case maybeImage of
        Just image ->
            div []
                [ imageOrBlank (imagePrepend ++ image.path)
                , div [] [ text image.title ]
                -- , showAlbumsForImage model.albums image
                ]
        Nothing ->
            div [] [ text "No Image Found" ]


view : Model -> Html Msg
view model =
    case model.state of
        ViewingState index maybeImage ->
            viewViewingState model.imagePrepend model.count model.key index maybeImage
        ErrorState ->
            div [] [ text "Error!" ]

viewViewingState : String -> Int -> String -> Int -> Maybe ImageWithMetadata -> Html Msg
viewViewingState imagePrepend count key index maybeImage =
    div [ class "text-center" ]
        [ div [] [ text ("Count: " ++ String.fromInt count) ]
        , div [] [ text ("Key: " ++ key) ]
        , button
            [ class "btn btn-primary", onClick Increment ]
            [ text "+" ]
        , viewImage imagePrepend maybeImage
        , text (String.fromInt index)
        ]



-- UPDATE --


moveImagePointer : Model -> Int -> Model
moveImagePointer model step =
    case model.state of
        ViewingState index _ ->
            let
                newIndex =
                    modBy (Array.length model.images) (index + step)
                newImage =
                    Array.get newIndex model.images
            in
            { model | state = ViewingState newIndex newImage }
        ErrorState ->
            { model | state = ViewingState 0 Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        KeyPress key ->
            case key of
                "ArrowLeft" ->
                    ( moveImagePointer model -1, Cmd.none )

                "ArrowRight" ->
                    ( moveImagePointer model 1, Cmd.none )

                " " ->
                    ( { model | key = "" }, Cmd.none )

                _ ->
                    ( { model | key = key }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown (Decode.map KeyPress (Decode.field "key" Decode.string))


main : Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
