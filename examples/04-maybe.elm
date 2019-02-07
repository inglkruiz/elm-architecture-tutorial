import Browser
import Html exposing (Html, Attribute, div, span, input, text, h4)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { input1 : String
  , input2 : String
  }


init : Model
init =
  { input1 = ""
  , input2 = ""
  }



-- UPDATE


type Msg
  = ChangeInput1 String
  | ChangeInput2 String


update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeInput1 newInput ->
      { model | input1 = newInput }
    ChangeInput2 newInput ->
      { model | input2 = newInput }


-- HELPERS

convertFahrenheitToCelsius : Float -> String
convertFahrenheitToCelsius value = String.fromFloat (value * 1.8 + 32)

convertInchesToCentimeters : Float -> String
convertInchesToCentimeters value = (String.fromFloat (value * 2.54))

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [
    viewConverter "Fahrenheit to Celsius" model.input1 "°C" ChangeInput1 "°F" convertFahrenheitToCelsius
    , viewConverter "Inches to Centimeters" model.input2 "in" ChangeInput2 "cm" convertInchesToCentimeters
    ]

viewConverter : String -> String -> String -> (String -> Msg) -> String -> (Float -> String) -> Html Msg
viewConverter title model fromUnitSymbol msg toUnitSymbol equivalent = 
  div []
    [
    h4 [] [ text title ]
    , viewInput model fromUnitSymbol msg
    , case String.toFloat model of
        Just value ->
          viewResult toUnitSymbol (equivalent value)

        Nothing ->
          viewUnknownResult toUnitSymbol
    ]
  
viewInput : String -> String -> (String -> Msg) -> Html Msg
viewInput userInput unitSymbol msg =
  span []
    [ 
      let
        color =
          if String.toFloat userInput == Nothing then
            "red"
          else
            "green"
      in
        input [ value userInput, onInput msg, style "width" "40px", style "border-color" color  ] []
    , text (unitSymbol ++ " = ")
    ]

viewResult : String -> String -> Html Msg
viewResult unitSymbol equivalent =
  span []
    [ span [ style "color" "blue" ] [ text equivalent ]
    , text unitSymbol
    ]

viewUnknownResult : String -> Html Msg
viewUnknownResult unitSymbol  =
  span []
    [ span [ style "color" "red" ] [ text "???" ]
    , text unitSymbol
    ]