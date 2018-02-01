-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/time.html

import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import String exposing (toInt)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = { counter: Int
    , flag: TimerStatus 
    ,hourses: Int
    , minutes: Int
    , seconds: Int}

type TimerStatus = Stop | Run | Complete

init : (Model, Cmd Msg)
init =
  ({counter=0, flag=Run, hourses=0, minutes=0, seconds=10}, Cmd.none)



-- UPDATE


type Msg
  = Tick Time 
  | TimerSwitch 
  | TimerRestart 
  | InHourses String 
  | InMinutes String
  | InSeconds String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
 let 
  (seconds, minutes, hourses) = timepattern model.counter
 in
  case msg of
    Tick newTime  ->
      if model.flag == Run then
      
        if (seconds == model.seconds) && 
           (minutes == model.minutes) && 
           (hourses == model.hourses) then
         ({model | flag=Complete}, Cmd.none) 
        else        
         ({model | counter=model.counter+1}, Cmd.none)
         
      else
         (model, Cmd.none)

    TimerSwitch ->
      ({model | 
         flag = if model.flag==Stop then 
           Run 
         else if model.flag == Run then 
           Stop 
         else Complete
        }, Cmd.none)

    TimerRestart ->
      ({model | counter=0, flag=Run}, Cmd.none)
      
    InHourses h ->
      ({model | hourses = Result.withDefault -1 (String.toInt h)}, Cmd.none)
    
    InMinutes m ->
--      ({model | minutes = String.toInt m}, Cmd.none)
      ({model | minutes = Result.withDefault -1 (String.toInt m)}, Cmd.none)
    
    InSeconds s ->
--      ({model | seconds = String.toInt s}, Cmd.none)
      ({model | seconds = Result.withDefault -1 (String.toInt s)}, Cmd.none)
    


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick



-- VIEW

timepattern: Int -> ( Int, Int, Int)
timepattern i = 
  let
    seconds = i % 60
    minutes = i // 60
    hourses = i // 3600
  in
    (seconds, minutes, hourses)

view : Model -> Html Msg
view model =
  let
    angle = degrees (toFloat(model.counter)*6)
    --seconds = model.counter % 60
    --minutes = model.counter // 60
    --hourses = model.counter // 3600
    (seconds, minutes, hourses) = timepattern model.counter

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
      
    btnCaption = 
      case model.flag of
      Run -> "Stop" 
      Stop -> "Go"
      _ -> "???"
      
  in
    div []
    [
    svg [ viewBox "0 0 100 100", Svg.Attributes.width "300px" ]
      [ circle [ cx "50", cy "50", r "45", fill (if model.flag==Stop then "#0B79CE" else if model.flag==Run then "#00FF00" else "#FF0000") ] []
      , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
      , line [ x1 "50", y1 "50", x2 handX2, y2 handY2, stroke "#023963" ] []
      , line [ x1 "50", y1 "50", x2 handX3, y2 handY3, stroke "#000963" ] []
      ]
      , div []
        [ input [ Html.Attributes.type_ "text", placeholder "Часы", onInput InHourses ] []
        , input [ Html.Attributes.type_ "text", placeholder "Минуты", onInput InMinutes ] []
        , input [ Html.Attributes.type_ "text", placeholder "Секунды", onInput InSeconds ] []
        --, viewValidation model
        ]
    , div [] [ Svg.text (toString hourses), Svg.text ":", Svg.text(toString minutes), Svg.text ":", Svg.text(toString seconds) ]
    , div [] [ Svg.text (toString model.hourses), Svg.text ":", Svg.text(toString model.minutes), Svg.text ":", Svg.text(toString model.seconds) ]
    , button [ onClick TimerSwitch] [Html.text btnCaption]
    , button [ onClick TimerRestart] [Html.text "Restart"]
    ]
    
--viewValidation : Model -> Html msg
--viewValidation model =
--  let
--    (color, message) =
--      if model.password == model.passwordAgain then
--        ("green", "OK")
--      else
--        ("red", "Passwords do not match!")
--  in
--    div [ style [("color", color)] ] [ text message ]
    