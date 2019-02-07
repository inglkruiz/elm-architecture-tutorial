import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type Face = One | Two | Three | Four | Five | Six

type alias Model = Face


init : () -> (Model, Cmd Msg)
init _ =
  ( One
  , Cmd.none
  )


-- HELPERS

rollDice : Random.Generator Face
rollDice =
  Random.uniform One [ Two, Three, Four, Five, Six ]

diceFaceImage : Face -> String
diceFaceImage face =
  case face of
    One ->
      "https://game-icons.net/icons/delapouite/originals/svg/dice-six-faces-one.svg"
    Two ->
      "https://game-icons.net/icons/delapouite/originals/svg/dice-six-faces-two.svg"
    Three ->
      "https://game-icons.net/icons/delapouite/originals/svg/dice-six-faces-three.svg"
    Four ->
      "https://game-icons.net/icons/delapouite/originals/svg/dice-six-faces-four.svg"
    Five ->
      "https://game-icons.net/icons/delapouite/originals/svg/dice-six-faces-five.svg"
    Six ->
      "https://game-icons.net/icons/delapouite/originals/svg/dice-six-faces-six.svg"

-- UPDATE


type Msg
  = Roll
  | NewFace Face


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewFace rollDice
      )

    NewFace newFace ->
      ( newFace
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [
      img [ src (diceFaceImage model), width 150 ] []
    , br [] []
    , button [ onClick Roll ] [ text "Roll" ]
    ]
