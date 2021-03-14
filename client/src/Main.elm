module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Either exposing (Either(..))
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
import Css.Global exposing (selector, global)
import Bytes exposing (Bytes(..), Endianness(..))
import Bytes.Decode as BD
import File.Download as Download

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
  = { queryR : Maybe QueryR
    , serverMsg : Maybe String
    , queryM : QueryModel
    , gtEqChecked: Bool
    , ltEqChecked: Bool
    }

initModel : Model
initModel = { queryR = Nothing
            , serverMsg = Just ""
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

maxLimit : number
maxLimit = 15

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
  | ApiMsg Bool String
  | QueryTS
  | ExportQ
  | ExportTS Bytes
  | GotQR QueryR
  | ChangeQM QueryModel
  | ChangeGT String
  | ChangeLT String
  | GEClick
  | LEClick
  | ChangeTSEq String
  | ChangeTagEq String
  | ChangeAggF String
  | ChangeGroupBy String
  | ChangeSort String
  | ChangeLimit String
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
    Api.BadStatus _ body ->
       (
         "Error: \n\n" 
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

sizedString : BD.Decoder String
sizedString = BD.andThen BD.string <| BD.unsignedInt32 BE 

responseToErrorBytes : Http.Response Bytes -> Result ErrorDetailed Bytes
responseToErrorBytes httpResponse =
  case httpResponse of
        Http.BadUrl_ url ->
            Err (Api.BadUrl url)

        Http.Timeout_ ->
            Err Api.Timeout

        Http.NetworkError_ ->
            Err Api.NetworkError

        Http.BadStatus_ metadata body ->
          let s = BD.decode (sizedString) body in
            Err (Api.BadStatus metadata (withDefault "" s))

        Http.GoodStatus_ _ body ->
            Ok body

responseToErrorQ : D.Decoder QueryR -> Http.Response String -> Result ErrorDetailed (QueryR)
responseToErrorQ decoder httpResponse =
  case httpResponse of
        Http.BadUrl_ url ->
            Err (Api.BadUrl url)

        Http.Timeout_ ->
            Err Api.Timeout

        Http.NetworkError_ ->
            Err Api.NetworkError

        Http.BadStatus_ metadata body -> 
            Err (Api.BadStatus metadata body)

        Http.GoodStatus_ _ body ->
            case D.decodeString decoder body of
              Ok value -> 
                Ok value
              Err err ->
                Err (Api.BadBody (D.errorToString err))

basicApiMsg : Result Http.Error (()) -> Msg
basicApiMsg res = 
  case res of
    Err e -> ApiMsg False (Debug.toString e)
    Ok _ -> ApiMsg True "Success."

apiRes : (a -> Msg) -> (Result ErrorDetailed a) -> Msg
apiRes toMsg res =
  case res of
    Err e -> ApiMsg False <| errToString e
    Ok r -> toMsg r

apiMsg : Result ErrorDetailed (()) -> Msg
apiMsg = apiRes (always <| ApiMsg True "Success.") 

handleTS : Result ErrorDetailed (QueryR) -> Msg
handleTS = apiRes GotQR 

handleTSBytes : Result ErrorDetailed Bytes -> Msg
handleTSBytes = apiRes (ExportTS)

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({queryM} as model) = 
  case msg of
    RequestFile fileToMsg ->
      (model, Select.file ["application/json"] fileToMsg)
    UploadTS file ->
      ({ model | serverMsg = Nothing }, postTimeseries file apiMsg responseToError)
    UpdateTS file ->
      ({ model | serverMsg = Nothing }, putTimeseries file apiMsg responseToError)
    DeleteTS file ->
      (model, deleteTimeseries file apiMsg responseToError)
    ClearAllTS -> 
      ({ model | serverMsg = Nothing, queryR = Nothing }, deleteAllTimeseries basicApiMsg)
    ApiMsg succeeded resMsg -> 
      let newQR = if not succeeded then Nothing else model.queryR in
      ({ model | queryR = newQR, serverMsg = Just resMsg }, Cmd.none)
    QueryTS -> 
      ({ model | serverMsg = Nothing }
      , getTimeseries {queryM | limit = Just (Basics.min maxLimit <| withDefault maxLimit queryM.limit)} handleTS responseToErrorQ)
    ExportQ ->
      ({ model | serverMsg = Nothing }, getTimeseriesBytes queryM handleTSBytes responseToErrorBytes)
    ExportTS ts ->
      ({ model | serverMsg = Just "Success"}
      , Download.bytes "query.json" "application/json" ts)
    GotQR qr -> 
      ({ model | queryR = Just qr, serverMsg = Just "Success." }, Cmd.none)
    ChangeQM qm ->
      ({ model | queryM = qm }, Cmd.none)
    ChangeGT gt -> 
      let newQM = if model.gtEqChecked 
                    then { queryM | ge = String.toInt gt }
                    else { queryM | gt = String.toInt gt }
      in ({ model | queryM = newQM }, Cmd.none)
    ChangeLT lt ->
      let newQM = if model.ltEqChecked
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
    ChangeAggF aggF ->
      ({ model | queryM = { queryM | aggFunc = stringToAgg aggF }}, Cmd.none)
    ChangeGroupBy groupBy ->
      ({ model | queryM = { queryM | groupBy = stringToGroupBy groupBy }}, Cmd.none)
    ChangeSort sort ->
      ({ model | queryM = { queryM | sort = stringToSort sort }}, Cmd.none)
    ChangeLimit limit ->
      ({ model | queryM = { queryM | limit = String.toInt limit }}, Cmd.none)
    NoOp -> 
      (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model = 
  page [] 
  [
    global [
      selector "@keyframes spin"
      [ Css.property "0% { transform" "rotate(0deg); } 100% { transform: rotate(360deg); }" ]  
    ],  -- For loader
    wrapper [style "gap" "1vw"]
    [
      actionWrapper []
        [ queryView model
        , div [] 
          [
            styledButton [ onClick (RequestFile UploadTS) ] [text "Upload Timeseries"]
          , styledButton [ onClick (RequestFile UpdateTS) ] [text "Update Timeseries"]
          , styledButton [ onClick (RequestFile DeleteTS) ] [text "Delete Timeseries"]
          , styledButton [ onClick ClearAllTS ] [ text "Clear All Data"]
          ]
        ]
        , tableQR model.queryR
        , serverMsgView model.serverMsg 
      ]
  ]


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
  , aggFuncView queryM
  , br [] []
  , text "Group By"
  , groupByView queryM
  , br [] []
  , text "Sort Timestamp"
  , sortView queryM
  , br [] []
  , text "Limit timeseries"
  , limitTSView queryM
  , br [] []
  , styledButton 
    [ 
      css 
      [
       padding2 (Css.em 0.6) (Css.em 1.2)
      ]
    , onClick QueryTS 
    ] [ text "Query Timeseries" ] 
   , styledButton 
      [ 
        css 
        [
         padding2 (Css.em 0.6) (Css.em 1.2)
        ]
      , onClick ExportQ 
      ] [ text "Query and Export Timeseries" ]
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
        , label [style "margin-left" "10px"] 
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

aggToString : Maybe Agg -> String
aggToString agg = 
  case agg of
    Just AvgAgg -> "Average"
    Just SumAgg -> "Sum"
    Just CountAgg -> "Count"
    Just MinAgg -> "Min"
    Just MaxAgg -> "Max"
    Nothing -> "-- Select --"

stringToAgg : String -> Maybe Agg
stringToAgg s =
  case s of
    "Average" -> Just AvgAgg
    "Sum" -> Just SumAgg
    "Count" -> Just CountAgg
    "Min" -> Just MinAgg
    "Max" -> Just MaxAgg
    _ -> Nothing

aggFuncView : QueryModel -> Html Msg
aggFuncView queryM =
  let 
      options = 
        [ aggToString Nothing
        , aggToString <| Just AvgAgg
        , aggToString <| Just SumAgg
        , aggToString <| Just CountAgg
        , aggToString <| Just MinAgg
        , aggToString <| Just MaxAgg
        ]
      buildOptions v =
        option [ value v, selected <| stringToAgg v == queryM.aggFunc ] [ text v ]
      viewOptions = List.map buildOptions options
  in
  select 
  [
    style "margin-top" "5px"
  , onInput ChangeAggF
  ] viewOptions

groupByToString : Maybe GroupBy -> String
groupByToString groupBy = 
  case groupBy of
    Just GByTimestamp -> "Timestamp"
    Just GByTag -> "Tag"
    Nothing -> "-- Select --"

stringToGroupBy : String -> Maybe GroupBy
stringToGroupBy s =
  case s of
    "Timestamp" -> Just GByTimestamp
    "Tag" -> Just GByTag
    _ -> Nothing

groupByView : QueryModel -> Html Msg
groupByView queryM =
  let 
      options = 
        [ groupByToString Nothing
        , groupByToString <| Just GByTimestamp
        , groupByToString <| Just GByTag
        ]
      buildOptions v =
        option [ value v, selected <| stringToGroupBy v == queryM.groupBy ] [ text v ]
      viewOptions = List.map buildOptions options
  in
  select 
  [
    style "margin-top" "5px"
  , onInput ChangeGroupBy
  ] viewOptions

sortToString : Maybe Sort -> String
sortToString groupBy = 
  case groupBy of
    Just Desc -> "Descending"
    _ -> "Ascending"

stringToSort : String -> Maybe Sort
stringToSort s =
  case s of
    "Descending" -> Just Desc
    _ -> Nothing

sortView : QueryModel -> Html Msg
sortView queryM =
  let 
      options = 
        [ sortToString Nothing
        , sortToString <| Just Desc
        ]
      buildOptions v =
        option [ value v, selected <| stringToSort v == queryM.sort ] [ text v ]
      viewOptions = List.map buildOptions options
  in
  select 
  [
    style "margin-top" "5px"
  , onInput ChangeSort
  ] viewOptions
    
limitTSView : QueryModel -> Html Msg
limitTSView queryM =
  div [ style "margin-top" "5px" ] 
      [ input 
          [ 
            type_ "number"
          , Html.Styled.Attributes.min "1"
          , value (withDefault "" <| Maybe.map String.fromInt queryM.limit)
          , onInput <| ChangeLimit
          ] []
      ]

serverMsgView : Maybe String -> Html Msg
serverMsgView s = 
  case s of
    Nothing -> loader []
    Just "" -> textWrapper [] [] 
    Just error -> textWrapper [] 
            ( h3 [] [ text  "Server message: " ]
              :: (List.intersperse (br [] []) <| 
                    List.map text <| 
                    String.lines <| error)
            )

formatTimstamp : Int -> String
formatTimstamp = fromTime << millisToPosix 

tableQR : Maybe QueryR -> Html Msg
tableQR qr = 
  case qr of
    Just (Left tss) -> tableTS tss
    Just (Right (Left gAggR)) -> tableGroup gAggR
    Just (Right (Right aggR)) -> tableAggR aggR
    Nothing -> div [] []

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
           ) :: List.map toTSTableRow tss
         )

toTSTableRow : TS -> Html Msg
toTSTableRow ts = 
  styledTR []
    [ styleT td [] [ text (formatTimstamp ts.timestamp)]
    , styleT td [] [ text ts.tag]
    , styleT td [] [ text (String.fromFloat ts.value)]
    ]

tableGroup : (List GroupAggR) -> Html Msg
tableGroup groupAggR =
    styledTable
        []
        (
          ( styledTHead []
            [ styleT th [] [ text "Group" ]
            , styleT th [] [ text "Value" ]
            ]
           ) :: List.map toGTableRow groupAggR
         )

toGTableRow : GroupAggR -> Html Msg
toGTableRow groupAggR = 
  let 
      formatGroup g = 
        case g of
          Left tag -> tag
          Right timestamp -> formatTimstamp timestamp
  in
  styledTR []
    [ styleT td [] [ text (formatGroup groupAggR.group)]
    , styleT td [] [ text (String.fromFloat groupAggR.result)]
    ]

tableAggR : AggR -> Html Msg
tableAggR aggR =
    styledTable
        []
        (
          [ styledTHead []
              [ styleT th [] [ text "Result" ] ]
          , styledTR []
              [ styleT td [] [ text (String.fromFloat aggR.result)]]
          ] 
         )
