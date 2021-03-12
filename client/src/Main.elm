module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Either exposing (Either(..))
import Api exposing (..)
import Http
import Json.Decode as D
import Task

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
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
  = ProcessUploadTS (List File)
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
    ProcessUploadTS files -> 
      case List.head files of
        Just file -> (model, postTimeseries file uploadMsg)
        Nothing -> (model, Cmd.none)
    UploadMsg resMsg -> let _ = Debug.log "Upload Message" resMsg in 
      (model, Cmd.none)
    GetTS -> (model, getAllTimeseries handleTS)
    GotTS tss -> (tss, Cmd.none)

-- VIEW

tagToString : Tag -> String
tagToString tag =
  case tag of
    Left s -> s
    Right s -> Debug.toString s

toTableRow : TS -> Html Msg
toTableRow ts = 
  tr []
    [ td [] [ text (Debug.toString ts.timestamp)]
    , td [] [ text (tagToString ts.tag)]
    , td [] [ text (Debug.toString ts.value)]
    ]

view : Model -> Html Msg
view model = 
  div []
    [
      input
        [ type_ "file"
        , multiple False
        , on "change" (D.map ProcessUploadTS filesDecoder)
        ]
        []
    , button [ onClick GetTS ] [ text "Get TS" ]
    , table
      []
      (( thead []
          [ th [] [ text "Timestamp" ]
          , th [] [ text "Tag" ]
          , th [] [ text "Value" ]
          ]
       ) :: List.map toTableRow model
      )
    ]

filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)
