{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Repository.Model where

import           Control.Applicative ((<|>))
import           Data.Acid           (Query, Update, makeAcidic)
import           Data.Aeson          (FromJSON, Object, ToJSON, object, pairs,
                                      parseJSON, toEncoding, toJSON, withObject,
                                      (.:), (.:?), (.=))
import           Data.SafeCopy       (base, deriveSafeCopy)
import qualified Data.Sequence       as S
import qualified Data.Vector         as V
import           GHC.Generics        (Generic)
import qualified IntMap              as IM
import qualified Map                 as M

type Timestamp = Int
type Tag = Either String Int
type Value = Float
type Ix = Int

type CollectR = [TS]

newtype AverageR = AvgR { result :: Value }
                deriving(Show, Generic, ToJSON, FromJSON)

type AggregateR = Either CollectR AverageR

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

type TagMap = M.Map Tag Ix

data TimeseriesDB = TimeseriesDB { tIx   :: IM.IntMap TagMap, -- composite timestamp/tag index
                                   sIx   :: M.Map Tag (S.Seq Ix), -- composite tag index
                                   data' :: V.Vector TS } -- all data

data QueryModel = Q { gt      :: Maybe Timestamp
                    , lt      :: Maybe Timestamp
                    , ge      :: Maybe Timestamp
                    , le      :: Maybe Timestamp
                    , tagEq   :: Maybe Tag
                    , aggFunc :: Maybe String
                    }
        deriving (Generic, ToJSON)

instance FromJSON QueryModel where
    parseJSON = withObject "QueryModel" $ \v -> Q
        <$> v .:? "gt"
        <*> v .:? "lt"
        <*> v .:? "ge"
        <*> v .:? "le"
        <*> (   (fmap Left <$> v .:? "tagEq")
            <|> (fmap Right <$> v .:? "tagEq")
            )
        <*> v .:? "aggFunc"

emptyQM :: QueryModel -> Bool
emptyQM (Q Nothing Nothing Nothing Nothing Nothing Nothing) = True
emptyQM _                                                   = False

justTag :: QueryModel -> Maybe Tag
justTag (Q Nothing Nothing Nothing Nothing a _) = a
justTag _                                       = Nothing

illegalQM :: QueryModel -> Bool
illegalQM Q {gt = (Just _), ge = (Just _)} = True
illegalQM Q {lt = (Just _), le = (Just _)} = True
illegalQM _                                = False

deriveSafeCopy 0 'base ''AverageR
deriveSafeCopy 0 'base ''TS
deriveSafeCopy 0 'base ''QueryModel
deriveSafeCopy 0 'base ''TimeseriesDB
