module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

main = Browser.sandbox { init = init, update = update, view = view }

type SharkType
  = UnitType
  | SumType SharkType SharkType

type SharkValue
  = UnitValue
  | LeftValue SharkValue
  | RightValue SharkValue

type alias Model = 
  { sharkType: SharkType
  }

init : Model
init =
  { sharkType =
      SumType
        (SumType (SumType UnitType UnitType) UnitType)
        UnitType
  }

type Msg
  = Noop

update : Msg -> Model -> Model
update msg model =
  case msg of
    Noop ->
      model

spacedSpan txt = span [style "margin-left" "2px", style "margin-right" "2px"] [text txt]

enumerateValues : SharkType -> List SharkValue
enumerateValues sharkType =
  case sharkType of
    UnitType -> [UnitValue]
    SumType left right
      -> List.map LeftValue (enumerateValues left)
      ++ List.map RightValue (enumerateValues right)

innerViewSharkValue : SharkValue -> List (Html Msg)
innerViewSharkValue sharkValue =
  case sharkValue of
    UnitValue -> [spacedSpan "V"]
    LeftValue value -> [spacedSpan "L"] ++ innerViewSharkValue value
    RightValue value -> [spacedSpan "R"] ++ innerViewSharkValue value

viewIndexedSharkValue : Int -> SharkValue -> Html Msg
viewIndexedSharkValue index sharkValue = div [] ([spacedSpan (String.fromInt index ++ ":")] ++ innerViewSharkValue sharkValue)

viewSharkType : SharkType -> List (Html Msg)
viewSharkType sharkType =
  case sharkType of
    UnitType -> [spacedSpan "U"]
    SumType left right -> [spacedSpan "("] ++ viewSharkType left ++ [spacedSpan "+"] ++ viewSharkType right ++ [spacedSpan ")"]

view : Model -> Html Msg
view model =
  div [ style "margin" "8px" ] 
    ( [ div [ style "margin-bottom" "8px" ] ([spacedSpan "Type:"] ++ viewSharkType model.sharkType)
      , div [ style "margin-bottom" "8px" ] [text "Values:"]
      ]
    ++ List.indexedMap viewIndexedSharkValue (enumerateValues model.sharkType)
    )
