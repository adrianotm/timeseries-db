module Map
   ( module Map
   , M.Map
   , M.empty
   , M.foldl
   , M.lookup
   ) where

import           Control.Monad.State
import           Data.Map.Strict     as M
import qualified Data.Sequence       as S

type SeqMap a = M.Map a (S.Seq Int)

insertWithIx :: Ord a => a -> SeqMap a -> State Int (SeqMap a)
insertWithIx s m = do ix <- get
                      put $ ix + 1
                      return $ M.insertWith (\a b -> b S.|> S.index a 0) s (S.singleton ix) m

foldIx :: Ord a => [b] -> (b -> a) -> SeqMap a -> State Int (SeqMap a)
foldIx ls f m = foldM (\acc b -> let s = f b in insertWithIx s acc) m ls
