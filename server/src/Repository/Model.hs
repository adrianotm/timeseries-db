{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}
module Repository.Model where

import           Control.Applicative  ((<|>))
import           Control.Lens         (makeLenses)
import           Control.Monad        (forM)
import           Control.Monad.Except (ExceptT)
import           Control.Monad.Fail   (fail)
import           Control.Monad.Reader (Reader)
import           Data.Acid            (Query, Update, makeAcidic)
import           Data.Aeson           (FromJSON, Object, ToJSON,
                                       Value (Number, String), object, pairs,
                                       parseJSON, toEncoding, toJSON,
                                       withObject, (.!=), (.:), (.:?), (.=))
import           Data.Aeson.TH        as AS (defaultOptions, deriveFromJSON,
                                             deriveJSON, fieldLabelModifier,
                                             rejectUnknownFields)
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as HM
import qualified Data.IntMap          as IM
import qualified Data.Map             as M
import           Data.Maybe           (isJust, mapMaybe)
import           Data.SafeCopy        (SafeCopy, base, contain, deriveSafeCopy,
                                       getCopy, putCopy, safeGet, safePut)
import           Data.Scientific      (toBoundedInteger)
import qualified Data.Set             as S
import           Data.Text            (Text, unpack)
import           Data.Typeable        (Typeable)
import qualified Data.Vector          as V
import           Elm.Derive           as ELM (defaultOptions, deriveBoth,
                                              deriveElmDef)
import           GHC.Generics

type Timestamp = Int
type Val = Double
type Ix = Int
type Limit = Int
type Tag = String

data GroupBy = GByTimestamp | GByTag
    deriving (Show)

data Sort = Asc | Desc
    deriving (Show)

data Agg = AvgAgg | SumAgg | CountAgg | MinAgg | MaxAgg
        deriving (Show, Generic)

type ExceptionQuery = ExceptT String (Query TimeseriesDB)

type CollectR = [TS]

newtype AggR = AggR { result :: Val }
                deriving(Show, Generic, ToJSON, FromJSON)

data GroupAggR = GroupAggR { _group :: Either Tag Timestamp, _result :: Val}
                deriving(Show, Generic)

newtype QueryR = QR (Either CollectR (Either [GroupAggR] AggR))
                deriving(Show, Generic)

data TS = TS { timestamp :: Timestamp, tag :: Tag, value :: Val }
    deriving (Show, Generic)

data DTS = DTS { __timestamp :: Timestamp, __tag :: Tag }
    deriving (Show, Generic)

type TimestampIndex = IM.IntMap [Ix]
type TagIndex = HM.HashMap Tag (IM.IntMap Ix)

data TimeseriesDB = TimeseriesDB { _tIx   :: TimestampIndex, -- composite timestamp/tag index
                                   _sIx   :: TagIndex, -- composite tag index
                                   _data' :: V.Vector TS } -- all data

data QueryModel = Q { gt      :: Maybe Timestamp
                    , lt      :: Maybe Timestamp
                    , ge      :: Maybe Timestamp
                    , le      :: Maybe Timestamp
                    , tsEq    :: Maybe Timestamp
                    , tagEq   :: Maybe Tag
                    , aggFunc :: Maybe Agg
                    , groupBy :: Maybe GroupBy
                    , sort    :: Maybe Sort
                    , limit   :: Maybe Limit
                    }
        deriving (Generic, Show)

illegalQM :: QueryModel -> (Bool, String)
illegalQM Q {groupBy = (Just _), aggFunc = Nothing} = (True, "You must provie 'aggFunc' with 'groupBy'.")
illegalQM Q {gt = (Just _), ge = (Just _)}          = (True, "Can't query 'gt' and 'ge' at the same time.")
illegalQM Q {lt = (Just _), le = (Just _)}          = (True, "Can't query 'lt' and 'le' at the same time.")
illegalQM Q {tsEq = (Just _), gt = (Just _)}        = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {tsEq = (Just _), ge = (Just _)}        = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {tsEq = (Just _), lt = (Just _)}        = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {tsEq = (Just _), le = (Just _)}        = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM _                                         = (False, "")

instance (Eq k, Typeable k, Typeable v, Hashable k, SafeCopy k, SafeCopy v) => SafeCopy (HM.HashMap k v) where
    getCopy = contain $ fmap HM.fromList safeGet
    putCopy = contain . safePut . HM.toList

instance Bounded Double where
    { minBound = -1/0; maxBound = 1/0 }

instance ToJSON QueryR where
    toJSON (QR qr) = either toJSON (either toJSON toJSON) qr
    toEncoding (QR qr) = either toEncoding (either toEncoding toEncoding) qr

instance FromJSON GroupBy where
    parseJSON (String "timestamp") = return GByTimestamp
    parseJSON (String "tag")       = return GByTag
    parseJSON _                    = fail "Illegal groupBy."

instance ToJSON GroupBy where
    toJSON GByTimestamp = "timestamp"
    toJSON GByTag       = "tag"

instance FromJSON Sort where
    parseJSON (String "asc")  = return Asc
    parseJSON (String "desc") = return Desc
    parseJSON _               = fail "Illegal sort."

instance ToJSON Sort where
    toJSON Asc  = "asc"
    toJSON Desc = "desc"

instance FromJSON Agg where
    parseJSON (String "avg")   = return AvgAgg
    parseJSON (String "sum")   = return SumAgg
    parseJSON (String "count") = return CountAgg
    parseJSON (String "min")   = return MinAgg
    parseJSON (String "max")   = return MaxAgg
    parseJSON _                = fail "Illegal aggFunc."

instance ToJSON Agg where
    toJSON AvgAgg   = "avg"
    toJSON SumAgg   = "sum"
    toJSON CountAgg = "count"
    toJSON MinAgg   = "min"
    toJSON MaxAgg   = "max"

instance ToJSON GroupAggR where
    toJSON (GroupAggR (Left tg) res) =
        object ["group" .= tg, "result" .= res]
    toJSON (GroupAggR (Right ts) res) =
        object ["group" .= ts, "result" .= res]
    toEncoding (GroupAggR (Left tg) res) =
        pairs ("group" .= tg <> "result" .= res)
    toEncoding (GroupAggR (Right ts) res) =
        pairs ("group" .= ts <> "result" .= res)

makeLenses ''TimeseriesDB

$(deriveJSON AS.defaultOptions{rejectUnknownFields = True, fieldLabelModifier = drop 2} ''DTS)
$(deriveJSON AS.defaultOptions{rejectUnknownFields = True} ''TS)
$(deriveFromJSON AS.defaultOptions{rejectUnknownFields = True} ''QueryModel)
$(deriveFromJSON AS.defaultOptions{rejectUnknownFields = True} ''GroupAggR)
$(deriveFromJSON AS.defaultOptions{rejectUnknownFields = True} ''QueryR)

deriveElmDef ELM.defaultOptions ''Agg
deriveElmDef ELM.defaultOptions ''GroupBy
deriveElmDef ELM.defaultOptions ''Sort
deriveElmDef ELM.defaultOptions ''AggR
deriveElmDef AS.defaultOptions{rejectUnknownFields = True} ''GroupAggR
deriveElmDef AS.defaultOptions{rejectUnknownFields = True} ''TS
deriveElmDef AS.defaultOptions{rejectUnknownFields = True, fieldLabelModifier = drop 2} ''DTS
deriveElmDef AS.defaultOptions{rejectUnknownFields = True} ''QueryModel
deriveElmDef ELM.defaultOptions ''QueryR

deriveSafeCopy 0 'base ''AggR
deriveSafeCopy 0 'base ''GroupAggR
deriveSafeCopy 0 'base ''QueryR
deriveSafeCopy 0 'base ''DTS
deriveSafeCopy 0 'base ''TS
deriveSafeCopy 0 'base ''Agg
deriveSafeCopy 0 'base ''GroupBy
deriveSafeCopy 0 'base ''Sort
deriveSafeCopy 0 'base ''QueryModel
deriveSafeCopy 0 'base ''TimeseriesDB
