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
  = { tss : List TS
    , serverMsg : String
    , queryM : QueryModel
    , gtEqChecked: Bool
    , ltEqChecked: Bool
    }

initModel : Model
initModel = { tss = []
            , serverMsg = ""
            , queryM = emptyQM
            , gtEqChecked = False
            , ltEqChecked = False
            }

emptyQM : QueryModel
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
  (initModel, Cmd.none  )

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
  | ChangeQM QueryModel
  | ChangeGT String
  | ChangeLT String
  | GEClick
  | LEClick
  | ChangeTSEq String
  | ChangeTagEq String
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
update msg ({queryM} as model) = 
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
    ChangeGT gt -> 
      let newQM = if model.gtEqChecked 
                    then { queryM | ge = String.toInt gt }
                    else { queryM | gt = String.toInt gt }
      in ({ model | queryM = newQM }, Cmd.none)
    ChangeLT lt ->
      let newQM = if model.gtEqChecked 
                    then { queryM | le = String.toInt lt }
                    else { queryM | lt = String.toInt lt }
      in ({ model | queryM = newQM }, Cmd.none)
    GEClick ->
      let newQM = if model.gtEqChecked 
                    then { queryM | gt = queryM.ge, ge = Nothing }
                    else { queryM | ge = queryM.gt, gt = Nothing }
      in ({ model | queryM = newQM, gtEqChecked = not model.gtEqChecked }, Cmd.none)
    LEClick ->
      let newQM = if model.ltEqChecked 
                    then { queryM | lt = queryM.le, le = Nothing }
                    else { queryM | le = queryM.lt, lt = Nothing }
      in ({ model | queryM = newQM, ltEqChecked = not model.ltEqChecked }, Cmd.none)
    ChangeTSEq tsEq ->
      ({ model | queryM = { queryM | tsEq = String.toInt tsEq }}, Cmd.none)
    ChangeTagEq tagEq ->
      ({ model | queryM = { queryM | tagEq = if tagEq == "" then Nothing else Just tagEq }}, Cmd.none)
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
        , tableTS model.tss
        , serverMsgView model.serverMsg 
      ]
  ]

tableTS : (List TS) -> Html Msg
tableTS tss =
    styledTable
        []
        (
          ( styledTHead []
            [ styleT th [] [ text "Timestamp" ]
            , styleT th [] [ text "Tag" ]
            , styleT th [] [ text "Value" ]
            ]
           ) :: List.map toTableRow tss
         )

gtToTimstamp : QueryModel -> String
gtToTimstamp queryM =
  case queryM.gt of
    Nothing -> withDefault "" (Maybe.map formatTimstamp queryM.ge)
    (Just gt) -> formatTimstamp gt

ltToTimestamp : QueryModel -> String
ltToTimestamp queryM =
  case queryM.lt of
    Nothing -> withDefault "" (Maybe.map formatTimstamp queryM.le)
    (Just lt) -> formatTimstamp lt

queryView : Model -> Html Msg
queryView ({queryM} as model) = 
  queryWrapper [] 
  [ h3 [] [text "Query"]
  , text ("From POSIX time (in millisec) - "  ++ gtToTimstamp queryM)
  , fromTimestamp model
  , br [] []
  , text ("To POSIX time (in millisec) - " ++ ltToTimestamp queryM)
  , toTimestamp model
  , br [] []
  , text ("Equal POSIX time (in millisec) - "
      ++ withDefault "" (Maybe.map formatTimstamp queryM.tsEq))
  , eqTimestamp queryM
  , br [] []
  , text "Tag Equal"
  , eqTag queryM
  , br [] []
  , text "Aggregation Function"
  -- , aggFuncView queryM
  -- , text "Group By"
  -- , groupByView queryM
  -- , text "Sort By"
  -- , sortByView queryM
  -- , text "Limit timeseries"
  -- , limiTSView queryM
  ]

parseGT : QueryModel -> String
parseGT qm =
  case qm.gt of
    Nothing -> withDefault "" <| Maybe.map (String.fromInt) qm.ge
    Just gt -> String.fromInt gt

fromTimestamp : Model -> Html Msg
fromTimestamp ({queryM} as model) = 
  div [ style "margin-top" "5px" ] 
      [ input 
          [ 
            type_ "number"
          , Html.Styled.Attributes.min "0"
          , value (parseGT queryM)
          , onInput <| ChangeGT
          ] []
        , label [style "margin-left" "10px"] 
            [
              input
              [
                type_ "checkbox"
              , Html.Styled.Attributes.checked <| model.gtEqChecked
              , onClick <| GEClick
              ] []
            , text "Equal"
            ]
      ]

parseLT : QueryModel -> String
parseLT qm =
  case qm.lt of
    Nothing -> withDefault "" <| Maybe.map (String.fromInt) qm.le
    Just lt -> String.fromInt lt

toTimestamp : Model -> Html Msg
toTimestamp ({queryM} as model) = 
  div [ style "margin-top" "5px" ] 
      [ input 
          [ 
            type_ "number"
          , Html.Styled.Attributes.min "0"
          , value (parseLT queryM)
          , onInput <| ChangeLT
          ] []
        , label [style "margin-left" "5px"] 
            [
              input
              [
                type_ "checkbox"
              , Html.Styled.Attributes.checked <| model.ltEqChecked 
              , onClick <| LEClick
              ] []
            , text "Equal"
            ]
      ]

eqTimestamp : QueryModel -> Html Msg
eqTimestamp queryM =
  div [ style "margin-top" "5px" ] 
      [ input 
          [ 
            type_ "number"
          , Html.Styled.Attributes.min "0"
          , value (withDefault "" <| Maybe.map String.fromInt queryM.tsEq)
          , onInput <| ChangeTSEq
          ] []
      ]

eqTag : QueryModel -> Html Msg
eqTag queryM = 
  div [ style "margin-top" "5px" ] 
    [ input 
        [ value <| withDefault "" queryM.tagEq
        , onInput <| ChangeTagEq
        ] []
    ]

-- aggFuncView : QueryModel -> Html Msg
-- aggFuncView queryM =
--   let 
--       options = 
--   select [ onInput handleSetAgg ]
    

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
    , styleT td [] [ text ts.tag]
    , styleT td [] [ text (Debug.toString ts.value)]
    ]
