{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Repository.Handlers where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader   (ask)
import           Control.Monad.State    (MonadState, evalState, get, put)
import           Data.Acid              (Query, Update, makeAcidic)
import           Data.Foldable
import           Data.Functor
import qualified Data.Sequence          as S
import qualified Data.Vector            as V
import qualified DataS.IntMap           as IM
import qualified DataS.Map              as M

import           Aggregates
import           Repository.Model
import           Repository.Queries.Tag
import           Repository.Queries.TS

qmToF :: QueryModel -> (IM.IntMap TagMap -> IM.IntMap TagMap)
qmToF Q {gt = (Just gt), lt = (Just lt)} = IM.lookupGLT' False False gt lt
qmToF Q {gt = (Just gt), le = (Just le)} = IM.lookupGLT' False True gt le
qmToF Q {lt = (Just lt), ge = (Just ge)} = IM.lookupGLT' True False ge lt
qmToF Q {ge = (Just ge), le = (Just le)} = IM.lookupGLT' True True ge le
qmToF Q {gt = (Just gt)}                 = IM.lookupGT' False gt
qmToF Q {ge = (Just ge)}                 = IM.lookupGT' True ge
qmToF Q {lt = (Just lt)}                 = IM.lookupLT' False lt
qmToF Q {le = (Just le)}                 = IM.lookupLT' True le
qmToF Q {}                               = id         --- returns error

insertTS :: [TS] -> Update TimeseriesDB ()
insertTS ts = do db@TimeseriesDB{..} <- get
                 let startIx = V.length data' in
                     put $ TimeseriesDB (evalState (IM.foldIx ts f tIx) startIx)
                                        (evalState (M.foldIx ts z sIx) startIx)
                                        (data' V.++ V.fromList ts)
                         where f tss = (timestamp tss, tag tss)
                               z = tag

filterTS :: QueryModel
         -> Query TimeseriesDB (Either String QueryR)
filterTS qm@Q{..} = maybe
                    (runExceptT $ tsQuery aggFunc tsEq tagEq $ qmToF qm)
                    (runExceptT . tagQuery aggFunc)
                    (justTag qm)

getAllTS :: Query TimeseriesDB [TS]
getAllTS = ask <&> aggTS' getList toCollect Nothing id

clearTS :: Update TimeseriesDB ()
clearTS = put $ TimeseriesDB IM.empty M.empty V.empty

makeAcidic ''TimeseriesDB ['insertTS, 'getAllTS, 'clearTS, 'filterTS]
