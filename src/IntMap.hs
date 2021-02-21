module IntMap
   ( module IntMap
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
lookupGT' :: Bool -> Key -> IM.IntMap a -> [a]
lookupGT' re k im =
   case im of
     Bin p m l r
       | nomatch k p m -> if mask k m > p then [] else IM.elems im
       | zero k m -> lookupGT' re k l ++ IM.elems r
       | otherwise -> lookupGT' re k r
     Tip ky y
       | k < ky -> [y]
       | re -> [y | k == ky]
       | otherwise -> []
     Nil -> []

--- Bool - return the equal key
lookupLT' :: Bool -> Key -> IM.IntMap a -> [a]
lookupLT' re k im =
   case im of
     Bin p m l r
       | nomatch k p m -> if mask k m < p then [] else IM.elems im
       | zero k m -> lookupLT' re k l
       | otherwise -> (IM.elems $! l) ++ lookupLT' re k r
     Tip ky y
       | k > ky -> [y]
       | re -> [y | k == ky]
       | otherwise -> []
     Nil -> []


--- Bool - return the equal key
lookupGLT' :: Bool -> Bool -> Key -> Key -> IM.IntMap a -> [a]
lookupGLT' re1 re2 k1 k2 im =
   case im of
     Bin p m l r
       | mask k1 m <= p && p <= mask k2 m -> lookupGLT' re1 re2 k1 k2 l
                                          ++ lookupGLT' re1 re2 k1 k2 r
       | otherwise -> []
     Tip ky y
       | k1 < ky && ky < k2 -> [y]
       | re1 && ky < k2 -> [y | k1 == ky]
       | k1 < ky && re2 -> [y | k2 == ky]
       | otherwise -> []
     Nil -> []

lookupLT = lookupLT' False
lookupLE = lookupLT' True

lookupGT = lookupGT' False
lookupGE = lookupGT' True

lookupGLT = lookupGLT' False False
lookupGTLE = lookupGLT' False True
lookupGELT = lookupGLT' True False
lookupGELE = lookupGLT' True True

insertWithIx :: Ord a => Key -> a -> IM.IntMap (M.Map a Int) -> State Int (IM.IntMap (M.Map a Int))
insertWithIx k s im = do ix <- get
                         put $ ix + 1
                         return $ IM.insertWith M.union k (M.fromList [(s, ix)]) im

foldIx :: Ord a => [b] -> (b -> (Key, a)) -> IM.IntMap (M.Map a Int) -> State Int (IM.IntMap (M.Map a Int))
foldIx ls f im = foldM (\acc b -> let (key, s) = f b in insertWithIx key s acc) im ls
