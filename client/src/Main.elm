module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Either exposing (Either(..), unpack)
import Api exposing (..)
import Http exposing (Error(..))
import Date exposing (..)
import Time exposing (..)
import Css exposing (..)
import Iso8601 exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Styles exposing (..)
import Json.Decode as D

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = always Sub.none
    , view = toUnstyled << view
    }

-- MODEL

type alias Model
  = { tss : List TS, serverMsg : String }

init : () -> (Model, Cmd Msg)
init _ =
  ( {tss = [], serverMsg = ""}
  , Cmd.none  )

-- UPDATE

type Msg
  = RequestFile (File -> Msg)
  | UploadTS File
  | UpdateTS File
  | DeleteTS File
  | ClearAllTS
  | ApiMsg String
  | GetTS
  | GotTS (List TS)
  | NoOp

parseError : String -> Maybe String
parseError = Result.toMaybe << D.decodeString D.string

errToString : ErrorDetailed -> String
errToString err = 
  case err of
    Api.Timeout ->
      "Timeout exceeded."
    Api.NetworkError ->
      "Network error"
    Api.BadStatus metadata body ->
       (
         String.fromInt metadata.statusCode 
         ++ " " 
         ++ metadata.statusText 
         ++ "\n\n"
         ++ body
        )
    Api.BadUrl url ->
      "Bad url " ++ url
    Api.BadBody body ->
      "Bad body " ++ body

responseToError : Http.Response String -> Result ErrorDetailed ()
responseToError httpResponse =
  case httpResponse of
        Http.BadUrl_ url ->
            Err (Api.BadUrl url)

        Http.Timeout_ ->
            Err Api.Timeout

        Http.NetworkError_ ->
            Err Api.NetworkError

        Http.BadStatus_ metadata body ->
            Err (Api.BadStatus metadata body)

        Http.GoodStatus_ _ _ ->
            Ok ()


apiMsg : Result ErrorDetailed (()) -> Msg
apiMsg res = 
  case res of
    Err e -> ApiMsg <| errToString e
    Ok _ -> ApiMsg "Success."

basicApiMsg : Result Http.Error (()) -> Msg
basicApiMsg res = 
  case res of
    Err e -> ApiMsg (Debug.toString e)
    Ok _ -> ApiMsg "Success."

handleTS : Result Http.Error ((List TS)) -> Msg
handleTS res = 
  case res of
    Err e -> ApiMsg (Debug.toString e)
    Ok tss -> GotTS tss

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    RequestFile fileToMsg ->
      (model, Select.file ["application/json"] fileToMsg)
    UploadTS file ->
      (model, postTimeseries file apiMsg responseToError)
    UpdateTS file ->
      (model, putTimeseries file apiMsg responseToError)
    DeleteTS file ->
      (model, deleteTimeseries file apiMsg responseToError)
    ClearAllTS -> 
      ({ model | tss = [] }, deleteAllTimeseries basicApiMsg)
    ApiMsg resMsg -> 
      ({ model | serverMsg = resMsg }, Cmd.none)
    GetTS -> 
      (model, getAllTimeseries handleTS)
    GotTS tss -> 
      ({ tss = tss, serverMsg = "Success" }, Cmd.none)
    NoOp -> 
      (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model = 
  wrapper [] 
  [
    wrapped []
    [
      actionWrapper []
        [
          styledButton [ onClick (RequestFile UploadTS) ] [text "Upload Timeseries"]
        , styledButton [ onClick (RequestFile UpdateTS) ] [text "Update Timeseries"]
        , styledButton [ onClick (RequestFile DeleteTS) ] [text "Delete Timeseries"]
        , styledButton [ onClick ClearAllTS ] [ text "Clear All Data"]
        , styledButton [ onClick GetTS ] [ text "Get Timeseries" ] 
        ]
        , styledTable
          []
          (
            ( styledTHead []
              [ styleT th [] [ text "Timestamp" ]
              , styleT th [] [ text "Tag" ]
              , styleT th [] [ text "Value" ]
              ]
             ) :: List.map toTableRow model.tss
           )
         , serverMsg model.serverMsg 
      ]
  ]

serverMsg : String -> Html Msg
serverMsg s = 
  case s of
    "" -> textWrapper [] []
    error -> textWrapper [] 
            ( h3 [] [ text  "Server message: " ]
              :: (List.intersperse (br [] []) <| 
                    List.map text <| 
                    String.lines <| error)
            )


formatTimstamp : Int -> String
formatTimstamp = fromTime << millisToPosix 

toTableRow : TS -> Html Msg
toTableRow ts = 
  styledTR []
    [ styleT td [] [ text (formatTimstamp ts.timestamp)]
    , styleT td [] [ text (unpack identity Debug.toString ts.tag)]
    , styleT td [] [ text (Debug.toString ts.value)]
    ]
