{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Repository.Model where

import           Control.Lens         (makeLenses)
import           Control.Monad.Except (ExceptT)
import           Data.Acid
import           Data.Aeson           (FromJSON, Object, ToJSON,
                                       Value (Number, String), object, pairs,
                                       parseJSON, toEncoding, toJSON,
                                       withObject, (.!=), (.:), (.:?), (.=))
import           Data.Aeson.TH        (defaultOptions, deriveFromJSON,
                                       deriveJSON, fieldLabelModifier,
                                       rejectUnknownFields)
import qualified Data.DList           as DL
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as HM
import qualified Data.IntMap          as IM
import qualified Data.Map             as M
import           Data.Maybe           (isJust, mapMaybe)
import           Data.SafeCopy        (SafeCopy, base, contain, deriveSafeCopy,
                                       getCopy, putCopy, safeGet, safePut)
import qualified Data.Set             as S
import           Data.Text            (Text, unpack)
import           Data.Typeable        (Typeable)
import           Data.Vector          as V
import           GHC.Generics
import           Model

type ExceptionQuery = ExceptT String (Query TimeseriesDB)

type TimestampIndex = IM.IntMap (DL.DList Ix)
type TagIndex = HM.HashMap Tag (IM.IntMap Ix)

data TimeseriesDB = TimeseriesDB { _tIx   :: TimestampIndex, -- composite timestamp/tag index
                                   _sIx   :: TagIndex, -- composite tag index
                                   _data' :: V.Vector TS } -- all data

instance Bounded Double where
    { minBound = -1/0; maxBound = 1/0 }

instance (SafeCopy a, Typeable a) => SafeCopy (DL.DList a) where
    getCopy = contain $ fmap DL.fromList safeGet
    putCopy = contain . safePut . DL.toList

instance (Eq k, Typeable k, Typeable v, Hashable k, SafeCopy k, SafeCopy v) => SafeCopy (HM.HashMap k v) where
    getCopy = contain $ fmap HM.fromList safeGet
    putCopy = contain . safePut . HM.toList

makeLenses ''TimeseriesDB

deriveSafeCopy 0 'base ''TimeseriesDB
