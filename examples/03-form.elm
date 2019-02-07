import Browser
import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Char exposing (isDigit, isUpper, isLower)

-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type Validation
  = None
  | Ok
  | Error

type alias ErrorModel = { status: Validation, msg: String }

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , valid : ErrorModel
  }


init : Model
init = 
  { name = ""
  , password = ""
  , passwordAgain = ""
  , age = ""
  , valid = { status = None, msg = "" }
  }



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String
  | Submit


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
      { model | age = age }
    
    Submit ->
      { model | valid = validate model }


validatePassword : String -> String -> ErrorModel
validatePassword password passwordAgain =
  if String.length password >= 8 then
    if not (String.any isDigit password) then
      { status = Error, msg = "Password must have one digit character at least!" }
    else if not (String.any isUpper password) then
      { status = Error, msg = "Password must have one uppercase character at least!" }
    else if not (String.any isLower password) then
      { status = Error, msg = "Password must have one lowercase character at least!" }
    else if password == passwordAgain then
      { status = Ok, msg = "OK"}
    else
      { status = Error, msg = "Passwords do not match!" }
  else
    { status = Error, msg = "Password must have 8 characters at least!" }

validateAge : String -> ErrorModel
validateAge age =
  if not (String.all isDigit age) then 
    { status = Error, msg = "Age must be an integer number!" }
  else 
    { status = Ok, msg = "OK" }

validate : Model -> ErrorModel
validate model =
  let
    passwordValidation = validatePassword model.password model.passwordAgain
    ageValidation = validateAge model.age
  in
    -- Only return password validation only if is not Ok
    if passwordValidation.status == Error then
      passwordValidation
    else 
      ageValidation
  


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewInput "text" "Age" model.age Age
    , button [ onClick Submit ] [ text "Submit" ]
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      case model.valid.status of
        Ok -> ("green", model.valid.msg)

        Error -> ("red", model.valid.msg)

        None -> ("black", "")
  in
    viewError color message


viewError : String -> String -> Html msg
viewError c t =
  div [ style "color" c ] [ text t ]