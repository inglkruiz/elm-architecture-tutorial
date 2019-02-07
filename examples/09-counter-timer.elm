module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random
import Time exposing (every)

type Status
    = Playing
    | Stopped

type alias Model =
    { count : Int, status : Status }


initialModel : () -> (Model, Cmd Msg)
initialModel _ =
    (
        { count = 0, status = Stopped }
        , Cmd.none
    )


type Msg
    = Start
    | Increment Time.Posix
    | Stop


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Start ->
            ({model | status = Playing}, Cmd.none)
        
        Increment _ ->
            ({ model | count = model.count + 1 }, Cmd.none)
            
        Stop ->
            ({model | status = Stopped}, Cmd.none)
        

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Playing ->  
            Time.every 1000 Increment
        Stopped ->
            Sub.none

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Start ] [ text "Start" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Stop ] [ text "Stop" ]
        ]


main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
