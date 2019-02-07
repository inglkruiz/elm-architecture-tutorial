import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Task
import Time
import Svg exposing (Svg, svg, rect, circle, line)
import Svg.Attributes exposing (..)



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type Status = Running | Stopped

type alias Clock =
  {
    hour : Int
    , minute : Int
    , second : Int
  }

type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , status : Status
  , clock : Clock
  }


init : () -> (Model, Cmd Msg)
init _ =
  (
    {
      zone = Time.utc
      , time = (Time.millisToPosix 0)
      , status = Running
      , clock = Clock 0 0 0
    }
    , Task.perform AdjustTimeZone Time.here
  )

-- HELPERS

humanClock : Time.Zone -> Time.Posix -> Clock
humanClock zone time =
  let
    hour   = (Time.toHour zone time)
    minute = (Time.toMinute zone time)
    second = (Time.toSecond zone time)
  in
    Clock hour minute second

smallRotation value =
  String.fromInt (value * 6)

-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | Stop



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime, clock = (humanClock model.zone newTime) }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

    Stop ->
      ( { model | status = Stopped }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.status of
    Running ->
      Time.every 1000 Tick        

    Stopped ->
      Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  let
    hour = String.fromInt (model.clock.hour)
    hourRotation = String.fromInt (model.clock.hour * 30)
    minute = String.fromInt (model.clock.minute)
    minuteRotation = (smallRotation model.clock.minute)
    second = String.fromInt (model.clock.second)
    secondRotation = (smallRotation model.clock.second)
  in
  div []
    [
      h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
      , div []
      [ svg
        [ 
          Svg.Attributes.width "120"
          , Svg.Attributes.height "120"
          , viewBox "0 0 120 120"
        ]
        [
          rect
            [ x "0"
            , y "0"
            , Svg.Attributes.width "120"
            , Svg.Attributes.height "120"
            , rx "15"
            , ry "15"
            ] []
          , circle
            [ cx "60"
            , cy "60"
            , r "55"
            , fill "white"
            ] []
          , circle
            [ cx "60"
            , cy "60"
            , r "3"
            , fill "red"
            ] []
          , line
            [ x1 "60"
            , y1 "60"
            , x2 "60"
            , y2 "30"
            , stroke "black"
            , strokeWidth "3"
            , transform ("rotate(" ++ hourRotation ++ ", 60, 60)")
            ] []
          , line
            [ x1 "60"
            , y1 "60"
            , x2 "60"
            , y2 "20"
            , stroke "black"
            , strokeWidth "2"
            , transform ("rotate(" ++ minuteRotation ++ ", 60, 60)")
            ] []
          , line
            [ x1 "60"
            , y1 "60"
            , x2 "60"
            , y2 "10"
            , stroke "red"
            , strokeWidth "1"
            , transform ("rotate(" ++ secondRotation ++ ", 60, 60)")
            ] []
        ]
      ]
      , button [ onClick Stop ] [ text "Stop"]
    ]
