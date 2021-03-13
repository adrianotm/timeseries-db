module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import File exposing (File)
import Either exposing (Either(..), unpack)
import Api exposing (..)
import Http
import Json.Decode as D
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = toUnstyled << view
    }

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- MODEL


type alias Model
  = List TS


init : () -> (Model, Cmd Msg)
init _ =
  ( []
  , Cmd.none  )

-- UPDATE

type Msg
  = UploadTS (List File)
  | UploadMsg String
  | GetTS
  | GotTS (List TS)

uploadMsg : Result Http.Error (()) -> Msg
uploadMsg res = 
  case res of
    Err e -> UploadMsg (Debug.toString e)
    Ok _ -> UploadMsg "Success upload."

handleTS : Result Http.Error ((List TS)) -> Msg
handleTS res = 
  case res of
    Err e -> UploadMsg (Debug.toString e)
    Ok tss -> GotTS tss

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    UploadTS files -> 
      case List.head files of
        Just file -> (model, postTimeseries file uploadMsg)
        Nothing -> (model, Cmd.none)
    UploadMsg resMsg -> let _ = Debug.log "Upload Message" resMsg in 
      (model, Cmd.none)
    GetTS -> (model, getAllTimeseries handleTS)
    GotTS tss -> (tss, Cmd.none)

-- VIEW

wrapper : List (Attribute msg) -> List (Html msg) -> Html msg
wrapper = 
  styled Html.Styled.div 
  [
    backgroundColor theme.primary
  , Css.height (vh 100)
  , Css.width (vw 100)
  ]

wrapped : List (Attribute msg) -> List (Html msg) -> Html msg
wrapped = 
  styled Html.Styled.div 
  [
    paddingLeft (vw 10)
  , paddingRight (vw 10)
  , paddingTop (vh 5)
  ]

actionWrapper : List (Attribute msg) -> List (Html msg) -> Html msg
actionWrapper = 
  styled Html.Styled.div 
  [
    displayFlex
  , justifyContent spaceAround
  , marginBottom (px 10)
  ]


styledTable : List (Attribute msg) -> List (Html msg) -> Html msg
styledTable =
  styled Html.Styled.table
  [
    margin auto
  , borderCollapse collapse
  , tableLayout fixed
  , border3 (px 1) solid (rgb 255 0 0)
  , Css.width (pct 50)
  ]

styleT : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Attribute msg) -> List (Html msg) -> Html msg
styleT elem = 
  styled elem
  [
    border3 (px 1) solid (rgb 255 0 0)
  , padding (rem 0.5)
  , textAlign center
  ]

theme : { secondary : Color, primary : Color }
theme =
    { primary = hex "FFA269"
    , secondary = rgb 250 240 230
    }

view : Model -> Html Msg
view model = 
  wrapper [] 
  [
    wrapped []
    [
      actionWrapper []
        [
          input
            [ type_ "file"
            , placeholder "Upload timeseries"
            , multiple False
            , on "change" (D.map UploadTS filesDecoder)
            ]
            []
        , button [ onClick GetTS ] [ text "Get TS" ] 
        ]
        , styledTable
          []
          (( thead []
              [ styleT th [] [ text "Timestamp" ]
              , styleT th [] [ text "Tag" ]
              , styleT th [] [ text "Value" ]
              ]
           ) :: List.map toTableRow model
        )
      ]
  ]

toTableRow : TS -> Html Msg
toTableRow ts = 
  tr []
    [ styleT td [] [ text (Debug.toString ts.timestamp)]
    , styleT td [] [ text (unpack identity Debug.toString ts.tag)]
    , styleT td [] [ text (Debug.toString ts.value)]
    ]


filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)
