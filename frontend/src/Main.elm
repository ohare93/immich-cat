module Main exposing (main)

import Browser exposing (element)
import Browser.Events exposing (onKeyDown)
import Dict exposing (Dict)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Json.Decode as Decode


type Msg
    = Increment
    | KeyPress String


type alias Model =
    { count : Int
    , key : String
    , currentImage : Maybe ImageWithMetadata
    , albums : List Album
    , images : Dict String ImageWithMetadata
    , imageNumToId : Dict Int String
    , currentImageIndex : Int
    }


type alias ImageWithMetadata =
    { id : String
    , url : String
    , title : String
    , inAlbumns : List String
    }


type alias Album =
    { id : String
    , name : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        testImages =
            [ { id = "0001", url = "http://localhost:3333/images/imafight.jpg", title = "Image A", inAlbumns = [ "a" ] }
            , { id = "0002", url = "http://localhost:3333/images/dcemployees.jpg", title = "Image B", inAlbumns = [ "b" ] }
            , { id = "0003", url = "http://localhost:3333/images/jordan.jpg", title = "Image C", inAlbumns = [ "a", "b" ] }
            ]
    in
    ( { count = 7
      , key = ""
      , albums =
            [ { id = "a", name = "Album A" }
            , { id = "b", name = "Album B" }
            , { id = "c", name = "Album C" }
            , { id = "d", name = "Album D" }
            , { id = "e", name = "Album E" }
            ]
      , images =
            Dict.fromList
                (List.map (\image -> ( image.id, image )) testImages)
      , imageNumToId = Dict.fromList (List.indexedMap (\index image -> ( index, image.id )) testImages)
      , currentImage = List.head testImages
      , currentImageIndex = 0
      }
    , Cmd.none
    )


showAlbumsForImage : List Album -> ImageWithMetadata -> Html msg
showAlbumsForImage albums image =
    let
        inAlbumns =
            List.filter (\album -> List.member album.id image.inAlbumns) albums

        notInAlbumns =
            List.filter (\album -> not (List.member album.id image.inAlbumns)) albums
    in
    div []
        [ div []
            [ text ("In Albumns: " ++ String.join ", " (List.map (\album -> album.name) inAlbumns)) ]
        , div []
            [ text ("Not In Albumns: " ++ String.join ", " (List.map (\album -> album.name) notInAlbumns)) ]
        ]


imageOrBlank : String -> Html msg
imageOrBlank key =
    if key == "" then
        text ""

    else
        img [ src key, class "img-fluid" ] []


showCurrentImage : Model -> Html msg
showCurrentImage model =
    case model.currentImage of
        Just currentImage ->
            div []
                [ imageOrBlank currentImage.url
                , div [] [ text currentImage.title ]
                , showAlbumsForImage model.albums currentImage
                ]

        Nothing ->
            div [] [ text "No image found" ]


moveImagePointer : Model -> Int -> Model
moveImagePointer model step =
    let
        newIndex =
            modBy (Dict.size model.imageNumToId) (model.currentImageIndex + step)

        newImageId =
            Dict.get newIndex model.imageNumToId

        newImage =
            newImageId
                |> Maybe.andThen (\number -> Dict.get number model.images)
    in
    { model | currentImageIndex = newIndex, currentImage = newImage }


view : Model -> Html Msg
view model =
    div [ class "text-center" ]
        [ div [] [ text ("Count: " ++ String.fromInt model.count) ]
        , div [] [ text ("Key: " ++ model.key) ]
        , button
            [ class "btn btn-primary", onClick Increment ]
            [ text "+" ]
        , showCurrentImage model
        , text (String.fromInt model.currentImageIndex)
        ]


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
