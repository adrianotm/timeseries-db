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
import           Data.Aeson           (FromJSON, Object, ToJSON, Value (String),
                                       object, pairs, parseJSON, toEncoding,
                                       toJSON, withObject, (.!=), (.:), (.:?),
                                       (.=))
import qualified Data.DList           as DL
import           Data.Functor
import qualified Data.HashMap.Strict  as HM
import           Data.List            (intercalate)
import           Data.Maybe           (isJust, mapMaybe)
import           Data.SafeCopy        (SafeCopy, base, contain, deriveSafeCopy,
                                       getCopy, putCopy, safeGet, safePut)
import           Data.Text            (Text, unpack)
import           Data.Typeable        (Typeable)
import qualified Data.Vector          as V
import qualified DataS.IntMap         as IM
import qualified DataS.Map            as M
import           GHC.Generics

type Timestamp = Int
type Tag = Either String Int
type Val = Float
type Ix = Int

data QueryType = TSQType | GeneralQ

data GroupBy = GByTimestemp | GByTag | IllegalGBy
    deriving (Show)

data Agg = AvgAgg | SumAgg | CountAgg | MinAgg | MaxAgg | IllegalAgg
        deriving (Show, Generic)

type ExceptionQuery = ExceptT String (Query TimeseriesDB)

type CollectR = [TS]

newtype AggR = AggR { result :: Val }
                deriving(Show, Generic, ToJSON, FromJSON)

data GroupAggR = GroupAggR { _tag :: Either Tag Timestamp, _result :: Val}
                deriving(Show, Generic, FromJSON)

newtype QueryR = QR (Either CollectR (Either [GroupAggR] AggR))
                deriving(Show, Generic)

data TS = TS { timestamp :: Timestamp, tag :: Tag, value :: Val }
    deriving (Show,Generic)

type TagMap = M.Map Tag Ix

data TimeseriesDB = TimeseriesDB { _tIx   :: IM.IntMap TagMap, -- composite timestamp/tag index
                                   _sIx   :: M.Map Tag (DL.DList Ix), -- composite tag index
                                   _data' :: V.Vector TS } -- all data

makeLenses ''TimeseriesDB

data QueryModel = Q { gt      :: Maybe Timestamp
                    , lt      :: Maybe Timestamp
                    , ge      :: Maybe Timestamp
                    , le      :: Maybe Timestamp
                    , tsEq    :: Maybe Timestamp
                    , tagEq   :: Maybe Tag
                    , aggFunc :: Maybe Agg
                    , groupBy :: Maybe GroupBy
                    }
        deriving (Generic, Show)


instance (Typeable a,SafeCopy a) => SafeCopy (DL.DList a) where
    getCopy = contain $ fmap DL.fromList safeGet
    putCopy = contain . safePut . DL.toList

instance Bounded Float where
    { minBound = -1/0; maxBound = 1/0 }

instance ToJSON QueryR where
    toJSON (QR qr) = either toJSON (either toJSON toJSON) qr
    toEncoding (QR qr) = either toEncoding (either toEncoding toEncoding) qr

instance FromJSON GroupBy where
    parseJSON (String "timestamp") = return GByTimestemp
    parseJSON (String "tag")       = return GByTag
    parseJSON _                    = return IllegalGBy

instance FromJSON Agg where
    parseJSON (String "avg")   = return AvgAgg
    parseJSON (String "sum")   = return SumAgg
    parseJSON (String "count") = return CountAgg
    parseJSON (String "min")   = return MinAgg
    parseJSON (String "max")   = return MaxAgg
    parseJSON _                = return IllegalAgg

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
    toJSON (GroupAggR (Left (Left tg)) res) =
        object ["tag" .= tg, "result" .= res]
    toJSON (GroupAggR (Left (Right tg)) res) =
        object ["tag" .= tg, "result" .= res]
    toJSON (GroupAggR (Right ts) res) =
        object ["timestamp" .= ts, "result" .= res]
    toEncoding (GroupAggR (Left (Left tg)) res) =
        pairs ("tag" .= tg <> "result" .= res)
    toEncoding (GroupAggR (Left (Right tg)) res) =
        pairs ("tag" .= tg <> "result" .= res)
    toEncoding (GroupAggR (Right ts) res) =
        pairs ("timestamp" .= ts <> "result" .= res)

class Fields a where
    fields :: [String]

instance Fields f => Fields (D1 x f) where
    fields = fields @f

instance Fields f => Fields (C1 x f) where
    fields = fields @f

instance (Selector s) => Fields (S1 s a) where
  fields = [selName (undefined :: M1 S s (K1 R t) ())]

instance (Fields f, Fields g) => Fields (f :*: g) where
    fields = fields @f ++ fields @g

instance FromJSON QueryModel where
    parseJSON = withObject "QueryModel" $ \v -> do
        gt <- v .:? "gt"
        lt <- v .:? "lt"
        ge <- v .:? "ge"
        le <- v .:? "le"
        tsEq <- v .:? "tsEq"
        tagEq <- (fmap Left <$> v .:? "tagEq")
             <|> (fmap Right <$> v .:? "tagEq")
        aggFunc <- v .:? "aggFunc"
        groupBy <- v .:? "groupBy"
        case mapMaybe (\k -> if unpack k `elem` fields @(Rep QueryModel)
                                        then Nothing
                                        else Just (show k)) (HM.keys v) of
           []  -> return $ Q{..}
           unexpectedFields -> fail $ "Fields: " ++ intercalate "," unexpectedFields ++ " are not allowed."

deriveSafeCopy 0 'base ''AggR
deriveSafeCopy 0 'base ''GroupAggR
deriveSafeCopy 0 'base ''QueryR
deriveSafeCopy 0 'base ''TS
deriveSafeCopy 0 'base ''Agg
deriveSafeCopy 0 'base ''GroupBy
deriveSafeCopy 0 'base ''QueryModel
deriveSafeCopy 0 'base ''TimeseriesDB
