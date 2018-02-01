-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/time.html

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = { counter: Int, flag: Bool }


init : (Model, Cmd Msg)
init =
  ({counter=0, flag=True}, Cmd.none)



-- UPDATE


type Msg
  = Tick Time | TimerSwitch | TimerRestart


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime  ->
      (if model.flag then 
         ({counter=model.counter+1, flag=model.flag}, Cmd.none)
      else
         (model, Cmd.none))

    TimerSwitch ->
      ({counter=model.counter, flag = not model.flag}, Cmd.none)

    TimerRestart ->
      ({counter=0, flag = model.flag}, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    angle = degrees (toFloat(model.counter)*6)
    seconds = model.counter % 60
    minutes = model.counter // 60
    hourses = model.counter // 3600

    handX =
      toString (50 + 40 * cos (angle-0.5*pi))

    handY =
      toString (50 + 40 * sin (angle-0.5*pi))

    handX2 =
      toString (50 + 30 * cos (angle / 60 - 0.5*pi))

    handY2 =
      toString (50 + 30 * sin (angle / 60 - 0.5*pi))

    handX3 =
      toString (50 + 25 * cos (angle / 3600 - 0.5*pi))

    handY3 =
      toString (50 + 25 * sin (angle / 3600 - 0.5*pi))
      
    btnCaption = if model.flag then "Stop" else "Go"
  in
    div []
    [
    svg [ viewBox "0 0 100 100", width "300px" ]
      [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
      , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
      , line [ x1 "50", y1 "50", x2 handX2, y2 handY2, stroke "#023963" ] []
      , line [ x1 "50", y1 "50", x2 handX3, y2 handY3, stroke "#000963" ] []
      ]
    , div [] [ Svg.text (toString hourses), Svg.text ":", Svg.text(toString minutes), Svg.text ":", Svg.text(toString seconds) ]
    , button [ onClick TimerSwitch] [Html.text btnCaption]
    , button [ onClick TimerRestart] [Html.text "Restart"]
    ]