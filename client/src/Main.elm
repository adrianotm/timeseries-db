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
import Maybe exposing (..)

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
  = { tss : List TS, serverMsg : String, queryM : QueryModel }

emptyQM = 
  { gt = Nothing 
  , lt = Nothing
  , ge = Nothing
  , le = Nothing
  , tsEq = Nothing
  , tagEq = Nothing
  , aggFunc = Nothing
  , groupBy = Nothing
  , sort = Nothing
  , limit = Nothing
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { tss = [], serverMsg = "", queryM = emptyQM }
  , Cmd.none  )

-- UPDATE

type Msg
  = RequestFile (File -> Msg)
  | UploadTS File
  | UpdateTS File
  | DeleteTS File
  | ChangeQM QueryModel
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
      ({ model | tss = tss, serverMsg = "Success." }, Cmd.none)
    ChangeQM qm ->
      ({ model | queryM = qm }, Cmd.none)
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
        [ queryView model
        , styledButton [ onClick GetTS ] [ text "Query Timeseries" ] 
        , styledButton [ onClick (RequestFile UploadTS) ] [text "Upload Timeseries"]
        , styledButton [ onClick (RequestFile UpdateTS) ] [text "Update Timeseries"]
        , styledButton [ onClick (RequestFile DeleteTS) ] [text "Delete Timeseries"]
        , styledButton [ onClick ClearAllTS ] [ text "Clear All Data"]
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
         , serverMsgView model.serverMsg 
      ]
  ]

parseQ : Maybe x -> String
parseQ = withDefault "" << Maybe.map Debug.toString

changeGT : QueryModel -> String -> Msg
changeGT queryM = 
  withDefault (ChangeQM { queryM | gt = Nothing }) << 
  Maybe.map (\ngt -> ChangeQM { queryM | gt = Just ngt}) << 
  String.toInt 

changeLT : QueryModel -> String -> Msg
changeLT queryM = 
  withDefault (ChangeQM { queryM | lt = Nothing }) << 
  Maybe.map (\nlt -> ChangeQM { queryM | lt = Just nlt}) << 
  String.toInt 

queryView : Model -> Html Msg
queryView model = 
  queryWrapper [] 
  [ text ("From POSIX time (in millisec) - " 
      ++ withDefault "" (Maybe.map formatTimstamp model.queryM.gt))
  , input 
    [ 
      type_ "number"
    , Html.Styled.Attributes.min "0"
    , value (parseQ model.queryM.gt)
    , onInput <| changeGT model.queryM 
    ] []
  , br [] []
  , text ("To POSIX time (in millisec) - "
      ++ withDefault "" (Maybe.map formatTimstamp model.queryM.lt))
  , input 
    [ 
      type_ "number"
    , Html.Styled.Attributes.min "0"
    , value (parseQ model.queryM.lt)
    , onInput <| changeLT model.queryM 
    ] []
  ]

serverMsgView : String -> Html Msg
serverMsgView s = 
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
