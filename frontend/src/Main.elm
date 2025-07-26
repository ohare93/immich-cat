module Main exposing (main)

import Browser exposing (element)
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, button, div, text, img)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Json.Decode as Decode


type Msg
    = Increment
    | KeyPress String


type alias Model =
    { count : Int
    , key : String
    }
    

getImageFromKey : String -> String
getImageFromKey key =
    case key of
        "a" -> "http://localhost:3333/images/imafight.jpg"
        "b" -> "http://localhost:3333/images/dcemployees.jpg"
        "c" -> "http://localhost:3333/images/c.jpg"
        _ -> ""
        
imageOrBlank : String -> Html msg
imageOrBlank key =
    if key == "" then
        text ""
    else
        img [ src (getImageFromKey key), class "img-fluid" ] []


view model =
    div [ class "text-center" ]
        [ div [] [ text ("Count: " ++ String.fromInt model.count) ]
        , div [] [ text ("Key: " ++ model.key) ]
        , button
            [ class "btn btn-primary", onClick Increment ]
            [ text "+" ]
        , imageOrBlank model.key
        ]


update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        KeyPress key ->
            if key == " " then
                ( { model | key = "" }, Cmd.none )
            else
                ( { model | key = key }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown (Decode.map KeyPress (Decode.field "key" Decode.string))


main : Program () Model Msg
main =
    element
        { init = \_ -> ( { count = 0, key = "" }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }