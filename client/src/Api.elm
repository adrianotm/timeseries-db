module Api exposing(..)

import Json.Decode exposing (int, string, map)
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Either exposing (Either(..))
import Dict exposing (Dict)
import File exposing (File)
import Http
import Url.Builder

type alias Timestamp = Int
type alias Val = Float
type alias Limit = Int

type alias TS  =
   { timestamp: Timestamp
   , tag: Tag
   , value: Val
   }

type ErrorDetailed
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String
    | BadBody String

jsonDecTimestamp: Json.Decode.Decoder Int
jsonDecTimestamp = Json.Decode.int

jsonEncTimestamp: Timestamp -> Value
jsonEncTimestamp = Json.Encode.int

jsonDecLimit: Json.Decode.Decoder Int
jsonDecLimit = Json.Decode.int

jsonEncLimit: Limit -> Value
jsonEncLimit = Json.Encode.int

jsonDecVal: Json.Decode.Decoder Float
jsonDecVal = Json.Decode.float

jsonEncVal: Val -> Value
jsonEncVal = Json.Encode.float

jsonDecTS : Json.Decode.Decoder ( TS )
jsonDecTS =
   Json.Decode.succeed (\ptimestamp ptag pvalue -> {timestamp = ptimestamp, tag = ptag, value = pvalue})
   |> required "timestamp" (jsonDecTimestamp)
   |> required "tag" (jsonDecTag)
   |> required "value" (jsonDecVal)

jsonEncTS : TS -> Value
jsonEncTS  val =
   Json.Encode.object
   [ ("timestamp", jsonEncTimestamp val.timestamp)
   , ("tag", jsonEncTag val.tag)
   , ("value", jsonEncVal val.value)
   ]

type alias QueryModel  =
   { gt: (Maybe Timestamp)
   , lt: (Maybe Timestamp)
   , ge: (Maybe Timestamp)
   , le: (Maybe Timestamp)
   , tsEq: (Maybe Timestamp)
   , tagEq: (Maybe Tag)
   , aggFunc: (Maybe Agg)
   , groupBy: (Maybe GroupBy)
   , sort: (Maybe Sort)
   , limit: (Maybe Limit)
   }

jsonDecQueryModel : Json.Decode.Decoder ( QueryModel )
jsonDecQueryModel =
   Json.Decode.succeed (\pgt plt pge ple ptsEq ptagEq paggFunc pgroupBy psort plimit -> {gt = pgt, lt = plt, ge = pge, le = ple, tsEq = ptsEq, tagEq = ptagEq, aggFunc = paggFunc, groupBy = pgroupBy, sort = psort, limit = plimit})
   |> fnullable "gt" (jsonDecTimestamp)
   |> fnullable "lt" (jsonDecTimestamp)
   |> fnullable "ge" (jsonDecTimestamp)
   |> fnullable "le" (jsonDecTimestamp)
   |> fnullable "tsEq" (jsonDecTimestamp)
   |> fnullable "tagEq" (jsonDecTag)
   |> fnullable "aggFunc" (jsonDecAgg)
   |> fnullable "groupBy" (jsonDecGroupBy)
   |> fnullable "sort" (jsonDecSort)
   |> fnullable "limit" (jsonDecLimit)

jsonEncQueryModel : QueryModel -> Value
jsonEncQueryModel  val =
   Json.Encode.object
   [ ("gt", (maybeEncode (jsonEncTimestamp)) val.gt)
   , ("lt", (maybeEncode (jsonEncTimestamp)) val.lt)
   , ("ge", (maybeEncode (jsonEncTimestamp)) val.ge)
   , ("le", (maybeEncode (jsonEncTimestamp)) val.le)
   , ("tsEq", (maybeEncode (jsonEncTimestamp)) val.tsEq)
   , ("tagEq", (maybeEncode (jsonEncTag)) val.tagEq)
   , ("aggFunc", (maybeEncode (jsonEncAgg)) val.aggFunc)
   , ("groupBy", (maybeEncode (jsonEncGroupBy)) val.groupBy)
   , ("sort", (maybeEncode (jsonEncSort)) val.sort)
   , ("limit", (maybeEncode (jsonEncLimit)) val.limit)
   ]

type alias Tag  = String

jsonDecTag : Json.Decode.Decoder ( Tag )
jsonDecTag = Json.Decode.string

jsonEncTag : Tag -> Value
jsonEncTag = Json.Encode.string

type Sort  =
    Asc
    | Desc 

jsonDecSort : Json.Decode.Decoder ( Sort )
jsonDecSort = 
    let jsonDecDictSort = Dict.fromList [("Asc", Asc), ("Desc", Desc)]
    in  decodeSumUnaries "Sort" jsonDecDictSort

jsonEncSort : Sort -> Value
jsonEncSort  val =
    case val of
        Asc -> Json.Encode.string "asc"
        Desc -> Json.Encode.string "desc"

type GroupBy  =
    GByTimestamp 
    | GByTag 

jsonDecGroupBy : Json.Decode.Decoder ( GroupBy )
jsonDecGroupBy = 
    let jsonDecDictGroupBy = Dict.fromList [("GByTimestamp", GByTimestamp), ("GByTag", GByTag)]
    in  decodeSumUnaries "GroupBy" jsonDecDictGroupBy

jsonEncGroupBy : GroupBy -> Value
jsonEncGroupBy  val =
    case val of
        GByTimestamp -> Json.Encode.string "timestamp"
        GByTag -> Json.Encode.string "tag"


type Agg  =
    AvgAgg 
    | SumAgg 
    | CountAgg 
    | MinAgg 
    | MaxAgg 

jsonDecAgg : Json.Decode.Decoder ( Agg )
jsonDecAgg = 
    let jsonDecDictAgg = Dict.fromList [("AvgAgg", AvgAgg), ("SumAgg", SumAgg), ("CountAgg", CountAgg), ("MinAgg", MinAgg), ("MaxAgg", MaxAgg)]
    in  decodeSumUnaries "Agg" jsonDecDictAgg

jsonEncAgg : Agg -> Value
jsonEncAgg  val =
    case val of
        AvgAgg -> Json.Encode.string "avg"
        SumAgg -> Json.Encode.string "sum"
        CountAgg -> Json.Encode.string "count"
        MinAgg -> Json.Encode.string "min"
        MaxAgg -> Json.Encode.string "max"


type alias CollectR = List TS

type alias QueryR  = (Either CollectR (Either (List GroupAggR) AggR))

jsonDecQueryR : Json.Decode.Decoder ( QueryR )
jsonDecQueryR = Json.Decode.oneOf [map Left (Json.Decode.list jsonDecTS), map (\k -> Right (Left k)) (Json.Decode.list jsonDecGroupAggR), map (\k -> Right (Right k)) jsonDecAggR]

type alias GroupAggR  =
   { group: (Either Tag Timestamp)
   , result: Val
   }

jsonDecGroupAggR : Json.Decode.Decoder ( GroupAggR )
jsonDecGroupAggR =
   Json.Decode.succeed (\p_group p_result -> {group = p_group, result = p_result})
   |> required "group" (Json.Decode.oneOf [map Left jsonDecTag, map Right jsonDecTimestamp])
   |> required "result" (jsonDecVal)


type alias AggR  = { result : Val }

jsonDecAggR : Json.Decode.Decoder ( AggR )
jsonDecAggR = 
   Json.Decode.succeed (\p_res -> {result = p_res})
   |> required "result" jsonDecVal

jsonEncAggR : AggR -> Value
jsonEncAggR val = 
   Json.Encode.object
   [ ("result", jsonEncVal val.result)
   ]


type alias DTS  =
   { timestamp: Timestamp
   , tag: Tag
   }

jsonDecDTS : Json.Decode.Decoder ( DTS )
jsonDecDTS =
   Json.Decode.succeed (\ptimestamp ptag -> {timestamp = ptimestamp, tag = ptag})
   |> required "timestamp" (jsonDecTimestamp)
   |> required "tag" (jsonDecTag)

jsonEncDTS : DTS -> Value
jsonEncDTS  val =
   Json.Encode.object
   [ ("timestamp", jsonEncTimestamp val.timestamp)
   , ("tag", jsonEncTag val.tag)
   ]


postTimeseries : File -> (Result ErrorDetailed (())  -> msg) -> (Http.Response String -> Result ErrorDetailed (())) -> Cmd msg
postTimeseries body toMsg handleRes =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8081"
                    [ "timeseries"
                    ]
                    params
            , body =
                Http.fileBody body
            , expect =
                Http.expectStringResponse toMsg handleRes
            , timeout 
               = Nothing
            , tracker 
               = Nothing
            }

putTimeseries : File -> (Result ErrorDetailed (())  -> msg) -> (Http.Response String -> Result ErrorDetailed (())) -> Cmd msg
putTimeseries file toMsg handleRes =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8081"
                    [ "timeseries"
                    ]
                    params
            , body =
                Http.fileBody file
            , expect =
                Http.expectStringResponse toMsg handleRes
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getTimeseries : QueryModel 
       -> (Result ErrorDetailed (QueryR)  -> msg) 
       -> (Json.Decode.Decoder QueryR -> Http.Response String -> Result ErrorDetailed (QueryR)) 
       -> Cmd msg
getTimeseries body toMsg handleRes =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
               []
            , url =
                Url.Builder.crossOrigin "http://localhost:8081"
                    [ "timeseries"
                    , "query"
                    ]
                    []
            , body =
                Http.jsonBody (jsonEncQueryModel body)
            , expect =
                Http.expectStringResponse toMsg <| handleRes jsonDecQueryR
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteTimeseries : File -> (Result ErrorDetailed (())  -> msg) -> (Http.Response String -> Result ErrorDetailed (())) -> Cmd msg
deleteTimeseries file toMsg handleRes =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8081"
                    [ "timeseries"
                    ]
                    params
            , body =
                Http.fileBody file
            , expect =
                Http.expectStringResponse toMsg handleRes
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteAllTimeseries : (Result Http.Error  (())  -> msg) -> Cmd msg
deleteAllTimeseries toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8081"
                    [ "timeseries"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getTimeseriesTimestamps : Bool -> (Result Http.Error  ((List Int))  -> msg) -> Cmd msg
getTimeseriesTimestamps query_bounded toMsg =
    let
        params =
            List.filterMap identity
            ([ if query_bounded then
                  Just (Url.Builder.string "bounded" "")
               else
                  Nothing ]
            )
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8081"
                    [ "timeseries"
                    , "timestamps"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (Json.Decode.int))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getTimeseriesTags : (Result Http.Error  ((List Tag))  -> msg) -> Cmd msg
getTimeseriesTags toMsg =
    let
        params =
            List.filterMap identity
            ([])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8081"
                    [ "timeseries"
                    , "tags"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecTag))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
