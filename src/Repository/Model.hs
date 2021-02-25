{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Repository.Model where

import           Control.Applicative  ((<|>))
import           Control.Monad.Except (ExceptT)
import           Data.Acid            (Query, Update, makeAcidic)
import           Data.Aeson           (FromJSON, Object, ToJSON, object, pairs,
                                       parseJSON, toEncoding, toJSON,
                                       withObject, (.!=), (.:), (.:?), (.=))
import           Data.Maybe           (isJust)
import           Data.SafeCopy        (base, deriveSafeCopy)
import qualified Data.Sequence        as S
import qualified Data.Vector          as V
import qualified DataS.IntMap         as IM
import qualified DataS.Map            as M
import           GHC.Generics         (Generic)

type Timestamp = Int
type Tag = Either String Int
type Value = Float
type Ix = Int
type Agg = String
type Group = Bool

instance Bounded Float where
    { minBound = -1/0; maxBound = 1/0 }

type ExceptionQuery = ExceptT String (Query TimeseriesDB)

type CollectR = [TS]

newtype AggR = AggR { result :: Value }
                deriving(Show, Generic, ToJSON, FromJSON)

data GroupAggR = GroupAggR { _tag :: Tag, _result :: Value}
                deriving(Show, Generic, FromJSON)

newtype QueryR = QR (Either CollectR (Either [GroupAggR] AggR))
                deriving(Show, Generic)

instance ToJSON QueryR where
    toJSON (QR qr) = either toJSON (either toJSON toJSON) qr
    toEncoding (QR qr) = either toEncoding (either toEncoding toEncoding) qr

data TS = TS { timestamp :: Timestamp, tag :: Tag, value :: Value }
    deriving (Show,Generic)

instance FromJSON TS where
    parseJSON = withObject "TS" $ \v -> TS
        <$> v .: "timestamp"
        <*> (   (Left <$> v .: "tag")
            <|> (Right <$> v .: "tag")
            )
        <*> v .: "value"

instance ToJSON TS where
    toJSON (TS ts (Left tg) v) =
        object ["timestamp" .= ts, "tag" .= tg, "value" .= v]
    toJSON (TS ts (Right tg) v) =
        object ["timestamp" .= ts, "tag" .= tg, "value" .= v]
    toEncoding (TS ts (Right tg) v) =
        pairs ("timestamp" .= ts <> "tag" .= tg <> "value" .= v)
    toEncoding (TS ts (Left tg) v) =
        pairs ("timestamp" .= ts <> "tag" .= tg <> "value" .= v)

instance ToJSON GroupAggR where
    toJSON (GroupAggR (Left tg) res) =
        object ["tag" .= tg, "result" .= res]
    toJSON (GroupAggR (Right tg) res) =
        object ["tag" .= tg, "result" .= res]
    toEncoding (GroupAggR (Left tg) res) =
        pairs ("tag" .= tg <> "result" .= res)
    toEncoding (GroupAggR (Right tg) res) =
        pairs ("tag" .= tg <> "result" .= res)

type TagMap = M.Map Tag Ix

data TimeseriesDB = TimeseriesDB { tIx   :: IM.IntMap TagMap, -- composite timestamp/tag index
                                   sIx   :: M.Map Tag (S.Seq Ix), -- composite tag index
                                   data' :: V.Vector TS } -- all data

data QueryModel = Q { gt      :: Maybe Timestamp
                    , lt      :: Maybe Timestamp
                    , ge      :: Maybe Timestamp
                    , le      :: Maybe Timestamp
                    , tsEq    :: Maybe Timestamp
                    , tagEq   :: Maybe Tag
                    , aggFunc :: Maybe Agg
                    , group   :: Group
                    }
        deriving (Generic, ToJSON)

instance FromJSON QueryModel where
    parseJSON = withObject "QueryModel" $ \v -> Q
        <$> v .:? "gt"
        <*> v .:? "lt"
        <*> v .:? "ge"
        <*> v .:? "le"
        <*> v .:? "tsEq"
        <*> (   (fmap Left <$> v .:? "tagEq")
            <|> (fmap Right <$> v .:? "tagEq")
            )
        <*> v .:? "aggFunc"
        <*> v .:? "group" .!= False

emptyQM :: QueryModel -> Bool
emptyQM (Q Nothing Nothing Nothing Nothing Nothing Nothing Nothing _) = True
emptyQM _                                                             = False

justTag :: QueryModel -> Maybe Tag
justTag (Q Nothing Nothing Nothing Nothing Nothing a _ _) = a
justTag _                                                 = Nothing

illegalQM :: QueryModel -> (Bool, String)
illegalQM Q {gt = (Just _), ge = (Just _)}    = (True, "Can't query 'gt' and 'ge' at the same time.")
illegalQM Q {lt = (Just _), le = (Just _)}    = (True, "Can't query 'lt' and 'le' at the same time.")
illegalQM Q {tsEq = (Just _), gt = (Just _)}  = (True, "Can't query 'tsEq' with any other timeseries condition.")
illegalQM Q {tsEq = (Just _), ge = (Just _)}  = (True, "Can't query 'tsEq' with any other timeseries condition.")
illegalQM Q {tsEq = (Just _), lt = (Just _)}  = (True, "Can't query 'tsEq' with any other timeseries condition.")
illegalQM Q {tsEq = (Just _), le = (Just _)}  = (True, "Can't query 'tsEq' with any other timeseries condition.")
illegalQM Q {group = True, aggFunc = Nothing} = (True, "You must provie 'aggFunc' with 'group'.")
illegalQM Q {group = True, tagEq = (Just _)}  = (True, "The composition of 'timestamp' and 'tag' is unique, so grouping with 'tagEq' is the same as only 'tagEq'")
illegalQM Q {group = True, tsEq = (Just _)}   = (True, "The composition of 'timestamp' and 'tag' is unique, so grouping with 'tagEq' is the same as only 'tagEq'")
illegalQM _                                   = (False, "")

deriveSafeCopy 0 'base ''AggR
deriveSafeCopy 0 'base ''GroupAggR
deriveSafeCopy 0 'base ''QueryR
deriveSafeCopy 0 'base ''TS
deriveSafeCopy 0 'base ''QueryModel
deriveSafeCopy 0 'base ''TimeseriesDB
