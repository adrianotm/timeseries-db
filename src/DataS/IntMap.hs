module DataS.IntMap
   ( module DataS.IntMap
   , IM.IntMap
   , IM.foldrWithKey
   , IM.lookup
   , IM.empty
   , IM.insertWith
   ) where

import           Control.Monad.State
import           Data.IntMap.Internal
import qualified Data.IntMap.Strict   as IM
import qualified Data.Map.Strict      as M


--- Bool - return the equal key
lookupGT' :: Bool -> Key -> IM.IntMap a -> IM.IntMap a
lookupGT' re k im =
   case im of
     Bin p m l r
       | nomatch k p m -> if mask k m > p then Nil else im
       | zero k m -> Bin p m (lookupGT' re k l) r
       | otherwise -> r
     t@(Tip ky y)
       | k < ky -> t
       | re && k == ky -> t
       | otherwise -> Nil
     Nil -> Nil

--- Bool - return the equal key
lookupLT' :: Bool -> Key -> IM.IntMap a -> IM.IntMap a
lookupLT' re k im =
   case im of
     Bin p m l r
       | nomatch k p m -> if mask k m < p then Nil else im
       | zero k m -> lookupLT' re k l
       | otherwise -> Bin p m l $ lookupLT' re k r
     t@(Tip ky y)
       | k > ky -> t
       | re && k == ky -> t
       | otherwise -> Nil
     Nil -> Nil


--- Bool - return the equal key
lookupGLT' :: Bool -> Bool -> Key -> Key -> IM.IntMap a -> IM.IntMap a
lookupGLT' re1 re2 k1 k2 im =
   case im of
     Bin p m l r
       | mask k1 m <= p && p <= mask k2 m -> Bin p m (lookupGLT' re1 re2 k1 k2 l) (lookupGLT' re1 re2 k1 k2 r)
       | otherwise -> Nil
     t@(Tip ky y)
       | k1 < ky && ky < k2 -> t
       | re1 && ky < k2 && k1 == ky -> t
       | k1 < ky && re2 && k2 == ky -> t
       | otherwise -> Nil
     Nil -> Nil

insertWithIx :: Ord a => Key -> a -> IM.IntMap (M.Map a Int) -> State Int (IM.IntMap (M.Map a Int))
insertWithIx k s im = do ix <- get
                         put $ ix + 1
                         return $ IM.insertWith M.union k (M.fromList [(s, ix)]) im

foldIx :: Ord a => [b] -> (b -> (Key, a)) -> IM.IntMap (M.Map a Int) -> State Int (IM.IntMap (M.Map a Int))
foldIx ls f im = foldM (\acc b -> let (key, s) = f b in insertWithIx key s acc) im ls
