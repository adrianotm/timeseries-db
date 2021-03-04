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
import           Data.Aeson.TH        (defaultOptions, deriveFromJSON,
                                       deriveJSON, rejectUnknownFields)
import qualified Data.DList           as DL
import           Data.Functor
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as HM
import qualified Data.IntMap          as IM
import           Data.List            (intercalate)
import qualified Data.Map             as M
import           Data.Maybe           (isJust, mapMaybe)
import           Data.SafeCopy        (SafeCopy, base, contain, deriveSafeCopy,
                                       getCopy, putCopy, safeGet, safePut)
import           Data.Scientific
import qualified Data.Set             as S
import           Data.Text            (Text, unpack)
import           Data.Typeable        (Typeable)
import qualified Data.Vector          as V
import           GHC.Generics

type Timestamp = Int
type Val = Float
type Ix = Int

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

data GroupBy = GByTimestemp | GByTag
    deriving (Show)

data Sort = Asc | Desc
    deriving (Show)

data Agg = AvgAgg | SumAgg | CountAgg | MinAgg | MaxAgg
        deriving (Show, Generic)

type ExceptionQuery = ExceptT String (Query TimeseriesDB)

type CollectR = [TS]

newtype AggR = AggR { result :: Val }
                deriving(Show, Generic, ToJSON, FromJSON)

data GroupAggR = GroupAggR { _tag :: Either Tag Timestamp, _result :: Val}
                deriving(Show, Generic)

newtype QueryR = QR (Either CollectR (Either [GroupAggR] AggR))
                deriving(Show, Generic)

data TS = TS { timestamp :: Timestamp, tag :: Tag, value :: Val }
    deriving (Show,Generic)

type TimestampIndex = IM.IntMap (DL.DList Ix)
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
                    }
        deriving (Generic, Show)

emptyQM = Q Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance (SafeCopy a, Typeable a) => SafeCopy (DL.DList a) where
    getCopy = contain $ fmap DL.fromList safeGet
    putCopy = contain . safePut . DL.toList

instance (Eq k, Typeable k, Typeable v, Hashable k, SafeCopy k, SafeCopy v) => SafeCopy (HM.HashMap k v) where
    getCopy = contain $ fmap HM.fromList safeGet
    putCopy = contain . safePut . HM.toList

instance Bounded Float where
    { minBound = -1/0; maxBound = 1/0 }


instance ToJSON QueryR where
    toJSON (QR qr) = either toJSON (either toJSON toJSON) qr
    toEncoding (QR qr) = either toEncoding (either toEncoding toEncoding) qr

instance FromJSON GroupBy where
    parseJSON (String "timestamp") = return GByTimestemp
    parseJSON (String "tag")       = return GByTag
    parseJSON _                    = fail "Illegal groupBy"

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

instance ToJSON GroupAggR where
    toJSON (GroupAggR (Left tg) res) =
        object ["tag" .= tg, "result" .= res]
    toJSON (GroupAggR (Right ts) res) =
        object ["timestamp" .= ts, "result" .= res]
    toEncoding (GroupAggR (Left tg) res) =
        pairs ("tag" .= tg <> "result" .= res)
    toEncoding (GroupAggR (Right ts) res) =
        pairs ("timestamp" .= ts <> "result" .= res)

makeLenses ''TimeseriesDB

$(deriveJSON defaultOptions{rejectUnknownFields = True} ''TS)
$(deriveFromJSON defaultOptions{rejectUnknownFields = True} ''QueryModel)

deriveSafeCopy 0 'base ''AggR
deriveSafeCopy 0 'base ''Tag
deriveSafeCopy 0 'base ''GroupAggR
deriveSafeCopy 0 'base ''QueryR
deriveSafeCopy 0 'base ''TS
deriveSafeCopy 0 'base ''Agg
deriveSafeCopy 0 'base ''GroupBy
deriveSafeCopy 0 'base ''Sort
deriveSafeCopy 0 'base ''QueryModel
deriveSafeCopy 0 'base ''TimeseriesDB
