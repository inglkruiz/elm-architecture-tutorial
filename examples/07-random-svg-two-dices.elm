import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (Svg, svg, rect, circle)
import Svg.Attributes exposing (..)
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

type alias Model = 
  {
    dice1 : Face
    , dice2 : Face
  }


init : () -> (Model, Cmd Msg)
init _ =
  (
    { 
      dice1 = One
      , dice2 = One
    }
    , Cmd.none
  )


-- HELPERS



rollDice : Random.Generator Face
rollDice =
  Random.uniform One [ Two, Three, Four, Five, Six ]

rollDices : Random.Generator Model
rollDices =
  Random.map2
    (\x y -> Model x y)
    (rollDice)
    (rollDice)


-- UPDATE


type Msg
  = Roll
  | NewFace Model


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewFace rollDices
      )

    NewFace newFaces ->
      ( newFaces
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
    viewDice model.dice1
    , viewDice model.dice2
    , br [] []
    , button [ onClick Roll ] [ text "Roll" ]
    ]

viewDice : Face -> Svg Msg
viewDice value = 
  svg
    [ Svg.Attributes.width "120"
    , Svg.Attributes.height "120"
    , viewBox "0 0 120 120"
    ]
    (List.append 
      [
        rect
        [ x "10"
        , y "10"
        , Svg.Attributes.width "100"
        , Svg.Attributes.height "100"
        , rx "15"
        , ry "15"
        ]
        []
      ]
      (viewFace value)
    )

viewFace : Face -> List (Svg Msg)
viewFace face = 
  case face of
    One ->
      [ viewCenterDot ]
    Two ->
      viewTopRightBottomLeftDots
    Three ->
      List.append viewTopRightBottomLeftDots [ viewCenterDot ]
    Four ->
      viewAllCornerDots
    Five ->
      List.append viewAllCornerDots [ viewCenterDot ]
    Six ->
      List.append viewAllCornerDots
      [
        viewCenterRightDot
        , viewCenterLeftDot
      ]

viewTopRightBottomLeftDots : List (Svg Msg)
viewTopRightBottomLeftDots = 
  [
    viewTopRightDot
    , viewBottomLeftDot
  ]

viewAllCornerDots : List (Svg Msg)
viewAllCornerDots = 
  List.append viewTopRightBottomLeftDots
  [
    viewTopLeftDot
    , viewBottomRightDot
  ]

viewCenterDot : Svg Msg
viewCenterDot =
  circle
    [ cx "60"
    , cy "60"
    , r "10"
    , fill "white"
    ] []

viewCenterRightDot : Svg Msg
viewCenterRightDot =
  circle
    [ cx "90"
    , cy "60"
    , r "10"
    , fill "white"
    ] []

viewCenterLeftDot : Svg Msg
viewCenterLeftDot =
  circle
    [ cx "30"
    , cy "60"
    , r "10"
    , fill "white"
    ] []

viewTopLeftDot : Svg Msg
viewTopLeftDot =
  circle
  [ cx "30"
  , cy "30"
  , r "10"
  , fill "white"
  ] []

viewTopRightDot : Svg Msg
viewTopRightDot =
  circle
  [ cx "90"
  , cy "30"
  , r "10"
  , fill "white"
  ] []

viewBottomLeftDot : Svg Msg
viewBottomLeftDot =
  circle
  [ cx "30"
  , cy "90"
  , r "10"
  , fill "white"
  ] []

viewBottomRightDot : Svg Msg
viewBottomRightDot =
  circle
  [ cx "90"
  , cy "90"
  , r "10"
  , fill "white"
  ] []
