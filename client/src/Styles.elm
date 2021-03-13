module Styles exposing (..)

import Css exposing (..)
import Css.Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)

wrapper : List (Attribute msg) -> List (Html msg) -> Html msg
wrapper = 
  styled Html.Styled.div 
  [
    backgroundColor (hex "FFFFFF")
  , Css.height (vh 100)
  , Css.width (vw 100)
  ]

wrapped : List (Attribute msg) -> List (Html msg) -> Html msg
wrapped = 
  styled Html.Styled.div 
  [
    displayFlex
  , paddingTop (vh 5)
  , margin auto
  , justifyContent center
  , Css.width (vw 90)
  , Css.height (vh 90)
  -- , backgroundColor (hex "EBDD86")
  ]

actionWrapper : List (Attribute msg) -> List (Html msg) -> Html msg
actionWrapper = 
  styled Html.Styled.div 
  [
    displayFlex
  , flex3 (int 0) (int 0) (pct 10)
  , flexDirection column
  , marginRight (px 10)
  ]


styledTable : List (Attribute msg) -> List (Html msg) -> Html msg
styledTable =
  styled Html.Styled.table
  [
    borderCollapse collapse
  , flex3 (int 0) (int 0) (pct 40)
  , tableLayout fixed
  , border3 (px 1) solid (rgb 255 0 0)
  , Css.width (pct 50)
  ]

styleT : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Attribute msg) -> List (Html msg) -> Html msg
styleT elem = 
  styled elem
  [
    border3 (px 1) solid (hex "854D27")
  , padding (rem 0.5)
  , textAlign center
  ]

styledTHead : List (Attribute msg) -> List (Html msg) -> Html msg
styledTHead = 
  styled thead
  [
    backgroundColor (hex "CD7F3A")
  ]

styledTR : List (Attribute msg) -> List (Html msg) -> Html msg
styledTR = 
  styled tr
  [
    backgroundColor (hex "F6D379")
  , nthOfType "odd" [
      backgroundColor (hex "FCF2D9")
     ]
  ]

styledButton : List (Attribute msg) -> List (Html msg) -> Html msg
styledButton = 
  styled button
  [
    backgroundColor (hex "4D6A6D")
  , textAlign center
  , display inlineBlock
  , marginBottom (Css.em 0.5)
  , padding2 (Css.em 0.3) (Css.em 1.2)
  , fontWeight bold
  , borderRadius (Css.em 0.5)
  , boxSizing borderBox
  , textDecoration none
  , color (hex "FFFFFF")
  , outline none
  , fontFamily sansSerif
  , hover [
       backgroundColor (hex "5D8083")
     ]
  ]

textWrapper : List (Attribute msg) -> List (Html msg) -> Html msg
textWrapper =
   styled div
   [
      flex3 (int 0) (int 0) (pct 20)
   ,  marginLeft (Css.em 1)
   ]

theme : { secondary : Color, primary : Color }
theme =
    { primary = hex "E7E393"
    , secondary = rgb 250 240 230
    }

