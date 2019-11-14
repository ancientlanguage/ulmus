module Main exposing (..)

import Browser
import Html
import Html exposing (Html, button, div, text, input, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

main = Browser.sandbox { init = init, update = update, view = view }

type SharkType
  = UnitType
  | SumType (List SharkType)

type SharkTypePath
  = Here
  | Deeper { choiceIndex : Int, path : SharkTypePath }

type SharkValue
  = UnitValue
  | SumValue { choiceIndex : Int, value : SharkValue }

type alias Model = 
  { sharkType: SharkType
  }

init : Model
init =
  { sharkType =
      SumType
        [ SumType [UnitType, UnitType, UnitType, UnitType]
        , SumType
          [ SumType [UnitType, UnitType]
          , SumType [UnitType, UnitType, UnitType]
          ]
        ]
  }

type Msg
  = Noop

update : Msg -> Model -> Model
update msg model =
  case msg of
    Noop ->
      model

spacedSpanStyles = [style "margin-left" "2px", style "margin-right" "2px"]
spacedSpan txt = span spacedSpanStyles [text txt]

enumerateValues : SharkType -> List SharkValue
enumerateValues sharkType =
  case sharkType of
    UnitType -> [UnitValue]
    SumType types ->
      let allValuesForTypes = List.map enumerateValues types
          indexedValuesForTypes = List.indexedMap (\index values -> List.map (\value -> SumValue { choiceIndex = index, value = value }) values) allValuesForTypes
      in List.concat indexedValuesForTypes

innerViewSharkValue : SharkValue -> List (Html Msg)
innerViewSharkValue sharkValue =
  case sharkValue of
    UnitValue -> [spacedSpan unitTypeValue]
    SumValue { choiceIndex, value } -> [spacedSpan (String.fromInt choiceIndex ++ ".")] ++ innerViewSharkValue value

viewIndexedSharkValue : Int -> SharkValue -> Html Msg
viewIndexedSharkValue index sharkValue =
  div []
    (  [span (spacedSpanStyles ++ [style "font-weight" "bold"]) [text (String.fromInt index ++ ":")]]
    ++ innerViewSharkValue sharkValue
    )

unitTypeString = "\u{1D7D9}" -- MATHEMATICAL DOUBLE-STRUCK DIGIT ONE (U+1D7D9)
unitTypeValue = "\u{25C7}" -- 'WHITE DIAMOND' (U+25C7)

sharkTypeToStrings : SharkType -> List String
sharkTypeToStrings sharkType =
  case sharkType of
    UnitType -> [unitTypeString]
    SumType types ->
      let typeStrings : List (List String)
          typeStrings = List.map sharkTypeToStrings types
      in ["("] ++ List.concat (List.intersperse ["+"] typeStrings) ++ [")"]

sharkTypeVerticalView : SharkType -> Html Msg
sharkTypeVerticalView sharkType =
  case sharkType of
    UnitType -> text unitTypeString
    SumType types ->
      let
        makeItem index t =
          div [style "margin-left" "20px"]
            [ span [style "margin-right" "8px"] [text (String.fromInt index ++ ".")]
            , sharkTypeVerticalView t
            ]
      in
      ( div []
          ( List.indexedMap makeItem types
          )
      )

view : Model -> Html Msg
view model =
  let
    typeHtml = List.map spacedSpan (sharkTypeToStrings model.sharkType)
    allValues = enumerateValues model.sharkType
    lengthAllValues = List.length allValues
    allValuesHtml = List.indexedMap viewIndexedSharkValue allValues
  in
    div [ style "margin" "8px" ] 
      ( [ div [ style "margin-bottom" "8px" ] ([spacedSpan "Type:"] ++ typeHtml)
        , sharkTypeVerticalView model.sharkType
        , div [ style "margin-bottom" "8px" ] ([spacedSpan "Values:", spacedSpan (String.fromInt lengthAllValues)])
        ]
        ++ allValuesHtml
      )
