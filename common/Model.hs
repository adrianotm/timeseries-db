{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeInType        #-}
module Model where

import           Data.Aeson      (FromJSON, ToJSON, Value (Number, String),
                                  object, pairs, parseJSON, toEncoding, toJSON,
                                  withObject, (.!=), (.:), (.:?), (.=))
import           Data.Aeson.TH   (defaultOptions, deriveFromJSON, deriveJSON,
                                  fieldLabelModifier, rejectUnknownFields)
import           Data.Hashable   (Hashable)
import           Data.SafeCopy   (SafeCopy, base, contain, deriveSafeCopy,
                                  getCopy, putCopy, safeGet, safePut)
import           Data.Scientific
import           Data.Text       (Text, unpack)
import           GHC.Generics

type Timestamp = Int
type Val = Double
type Ix = Int
type Limit = Int

newtype Tag = Tag (Either String Int)
    deriving (Eq, Ord, Hashable, Generic)

instance Show Tag where
    show (Tag t) = either show show t

instance FromJSON Tag where
    parseJSON (String s) = return $ Tag (Left $ unpack s)
    parseJSON (Number n) = maybe (fail "Invalid number.") (return . Tag . Right) (toBoundedInteger n)
    parseJSON _ = fail "Tag type not allowed. Expected Int or String."

instance ToJSON Tag where
    toJSON (Tag t) = either toJSON toJSON t
    toEncoding (Tag t) = either toEncoding toEncoding t

data GroupBy = GByTimestamp | GByTag
    deriving (Show)

data Sort = Asc | Desc
    deriving (Show)

data Agg = AvgAgg | SumAgg | CountAgg | MinAgg | MaxAgg
        deriving (Show, Generic)


data TS = TS { timestamp :: Timestamp, tag :: Tag, value :: Val }
    deriving (Show, Generic)

data DTS = DTS { __timestamp :: Timestamp, __tag :: Tag }
    deriving (Show, Generic)


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

data GroupAggR = GroupAggR { _tag :: Either Tag Timestamp, _result :: Val}
                deriving(Show, Generic)

type CollectR = [TS]

newtype AggR = AggR { result :: Val }
                deriving(Show, Generic, ToJSON, FromJSON)

newtype QueryR = QR (Either CollectR (Either [GroupAggR] AggR))
                deriving(Show, Generic)

instance ToJSON GroupAggR where
    toJSON (GroupAggR (Left tg) res) =
        object ["tag" .= tg, "result" .= res]
    toJSON (GroupAggR (Right ts) res) =
        object ["timestamp" .= ts, "result" .= res]
    toEncoding (GroupAggR (Left tg) res) =
        pairs ("tag" .= tg <> "result" .= res)
    toEncoding (GroupAggR (Right ts) res) =
        pairs ("timestamp" .= ts <> "result" .= res)

instance ToJSON QueryR where
    toJSON (QR qr) = either toJSON (either toJSON toJSON) qr
    toEncoding (QR qr) = either toEncoding (either toEncoding toEncoding) qr

emptyQM = Q Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance FromJSON GroupBy where
    parseJSON (String "timestamp") = return GByTimestamp
    parseJSON (String "tag")       = return GByTag
    parseJSON _                    = fail "Illegal groupBy."

instance FromJSON Sort where
    parseJSON (String "asc")  = return Asc
    parseJSON (String "desc") = return Desc
    parseJSON _               = fail "Illegal sort."

instance FromJSON Agg where
    parseJSON (String "avg")   = return AvgAgg
    parseJSON (String "sum")   = return SumAgg
    parseJSON (String "count") = return CountAgg
    parseJSON (String "min")   = return MinAgg
    parseJSON (String "max")   = return MaxAgg
    parseJSON _                = fail "Illegal aggFunc."

$(deriveJSON defaultOptions{rejectUnknownFields = True, fieldLabelModifier = drop 2} ''DTS)
$(deriveJSON defaultOptions{rejectUnknownFields = True} ''TS)
$(deriveFromJSON defaultOptions{rejectUnknownFields = True} ''QueryModel)

deriveSafeCopy 0 'base ''Tag
deriveSafeCopy 0 'base ''DTS
deriveSafeCopy 0 'base ''TS
deriveSafeCopy 0 'base ''Agg
deriveSafeCopy 0 'base ''GroupBy
deriveSafeCopy 0 'base ''AggR
deriveSafeCopy 0 'base ''GroupAggR
deriveSafeCopy 0 'base ''QueryR
deriveSafeCopy 0 'base ''Sort
deriveSafeCopy 0 'base ''QueryModel
