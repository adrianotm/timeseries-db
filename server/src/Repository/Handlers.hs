{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Repository.Handlers where

import           Control.Lens              ((%~), (.~))
import           Control.Monad.Except      (forM_, runExceptT)
import           Control.Monad.Reader      (ask, runReader, runReaderT)
import           Control.Monad.State       (MonadState, evalState, get, put)
import           Data.Acid                 (Query, Update, makeAcidic)
import           Data.Foldable             (forM_)
import           Data.Function             ((&))
import           Data.Functor              (($>), (<&>))
import           Data.Maybe                (fromMaybe)
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
import qualified DataS.DList               as DL
import qualified DataS.HashMap             as HM
import qualified DataS.IntMap              as IM

import           Repository.Model          (DTS, QueryModel (..), QueryR, TS,
                                            Tag, TimeseriesDB (..), Timestamp,
                                            data')
import           Repository.Queries        (query)
import           Repository.Queries.Shared (InternalQ (InternalQ))
import           Repository.Utils          (Error, sIxAppendTS, sIxDeleteTS,
                                            tIxAppendTS, tIxDeleteTS,
                                            unsafeIndexOf, vDeleteTS,
                                            validDelete, validInsert,
                                            validUpdate)

insertTS :: [TS] -> Update TimeseriesDB [Error]
insertTS ts = do db@TimeseriesDB{..} <- get
                 case validInsert db ts of
                   [] -> let startIx = V.length _data' in
                                 put (TimeseriesDB (tIxAppendTS ts _tIx startIx)
                                                   (sIxAppendTS ts _sIx startIx)
                                                   (_data' V.++ V.fromList ts)) $> []
                   errors -> return $ take 10 errors

updateTS :: [TS] -> Update TimeseriesDB [Error]
updateTS ts = get >>= \db -> case validUpdate db ts of
                               [] -> put (db & data' %~ V.modify
                                                        (\v -> forM_ ts (\ts -> VM.write v (unsafeIndexOf (Left ts) db) ts)))
                                                        $> []
                               errors -> return $ take 10 errors

clearTS :: [DTS] -> Update TimeseriesDB [Error]
clearTS dts = case dts of
                [] -> put (TimeseriesDB IM.empty HM.empty V.empty) $> []
                dtss -> get >>= \db@TimeseriesDB{..}
                          -> case validDelete db dtss of
                              []     -> put (TimeseriesDB (tIxDeleteTS dtss db)
                                                          (sIxDeleteTS dtss db)
                                                          (vDeleteTS dtss db)) $> []
                              errors -> return $ take 10 errors

filterTS :: QueryModel
         -> Query TimeseriesDB (Either Error QueryR)
filterTS qm@Q{..} = ask <&> runReader (runExceptT query) . InternalQ qm

allTimestamps :: Bool -> Query TimeseriesDB (Either Error [Timestamp])
allTimestamps bounded = ask <&> \db ->
                                  if bounded
                                    then fromMaybe (Left "No data.") $
                                          (\(min, _) (max, _) -> Right [min, max])
                                            <$> IM.lookupMin (_tIx db) <*> IM.lookupMax (_tIx db)
                                    else Right $ IM.keys $ _tIx db

allTags :: Query TimeseriesDB [Tag]
allTags = ask <&> HM.keys . _sIx

makeAcidic ''TimeseriesDB ['insertTS, 'clearTS, 'filterTS, 'updateTS, 'allTimestamps, 'allTags]
