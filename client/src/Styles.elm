module Styles exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)

page : List (Attribute msg) -> List (Html msg) -> Html msg
page = 
  styled Html.Styled.div 
  [
    backgroundColor (hex "FFFFFF")
  , Css.height (vh 100)
  , Css.width (vw 100)
  ]

wrapper : List (Attribute msg) -> List (Html msg) -> Html msg
wrapper = 
  styled Html.Styled.div 
  [
    displayFlex
  , paddingTop (vh 5)
  , margin auto
  , Css.width (vw 95)
  , Css.height (vh 90)
  ]

actionWrapper : List (Attribute msg) -> List (Html msg) -> Html msg
actionWrapper = 
  styled Html.Styled.div 
  [
    displayFlex
  , flex3 (int 0) (int 0) (pct 25)
  , border3 (px 1) solid (hex "668C8F")
  , paddingLeft (px 20)
  , paddingBottom (px 20)
  , flexDirection column
  , justifyContent spaceBetween
  , backgroundColor (hex "E7EEEE")
  ]

tableWrapper : List (Attribute msg) -> List (Html msg) -> Html msg
tableWrapper =
  styled Html.Styled.div
  [
    flex3 (int 0) (int 0) (pct 50)
  ]

styledTable : List (Attribute msg) -> List (Html msg) -> Html msg
styledTable =
  styled Html.Styled.table
  [
    borderCollapse collapse
  , tableLayout fixed
  , flexDirection column
  , Css.width (pct 100)
  , border3 (px 1) solid (rgb 255 0 0)
  ]

styleT : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Attribute msg) -> List (Html msg) -> Html msg
styleT elem = 
  styled elem
  [
    border3 (px 1) solid (hex "111818")
  , padding (rem 0.5)
  , textAlign center
  ]

styledTHead : List (Attribute msg) -> List (Html msg) -> Html msg
styledTHead = 
  styled thead
  [
    backgroundColor (hex "668C8F")
  ]

styledTR : List (Attribute msg) -> List (Html msg) -> Html msg
styledTR = 
  styled tr
  [
    backgroundColor (hex "ABC4C4")
  , nthOfType "odd" [
      backgroundColor (hex "E7EEEE")
     ]
  ]

styledButton : List (Attribute msg) -> List (Html msg) -> Html msg
styledButton = 
  styled button
  [
    backgroundColor (hex "668C8F")
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
  , Css.width (pct 60)
  , hover [
       backgroundColor (hex "7CA0A2")
     ]
  ]

textWrapper : List (Attribute msg) -> List (Html msg) -> Html msg
textWrapper =
   styled div
   [
      flex3 (int 0) (int 0) (pct 20)
   ]

queryWrapper : List (Attribute msg) -> List (Html msg) -> Html msg
queryWrapper =
   styled div
   [
      displayFlex
   , flexDirection column
   , alignItems Css.start
   , marginBottom (Css.em 1)
   ]

loader : List (Html msg) -> Html msg
loader =
  styled div 
  [ border3 (px 10) solid (hex "f3f3f3")
  , borderTop3 (px 10) solid (hex "668C8F")
  , borderRadius (pct 50)
  , Css.width (px 30)
  , Css.height (px 30)
  ] 
  [ style "animation" "spin 2s linear infinite" ]
