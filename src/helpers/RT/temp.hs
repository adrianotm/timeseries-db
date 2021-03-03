{-# LANGUAGE BangPatterns #-}
module RT.RadixTree where

import           Data.Array
import           Data.Bits
import           Data.Foldable
import           Data.Word

type Timestamp = Word32

type Prefix = Word32
type Mask = Word32
type Chunk = Word8

data B2 = Z | F | S | T

-- Branching Mode
data BM a =
    MBinary !a
  | M2B !a

data Children a =
      NB !a !a
    | N2B !a !a !a !a
  deriving (Show)

data RT a =
    Branch !Prefix !Mask !(Children (RT a))
  | Leaf !Prefix !a
  | Empty
  deriving (Show)


--- Empty RadixTree
empty :: RT a
empty = Empty

--- Get the Prefix mask
mask :: Timestamp -> Mask -> Prefix
mask i m = i .&. shiftL (2 ^ lz - 1) (wordSize - lz)
  where lz = countLeadingZeros m

--- Do the prefixes match
nomatch :: Timestamp -> Prefix -> Mask -> Bool
nomatch t p m = mask t m /= p

--- Is the branching bit zero
zero :: Timestamp -> Mask -> Bool
zero t m = t .&. m == 0

--- Highest bit mask - counts the leading zeroes
highestBitMask :: Prefix -> Prefix
highestBitMask w = shiftL 1 (wordSize - 1 - countLeadingZeros w)

--- Branching mask that is after the common prefix
branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = highestBitMask (p1 `xor` p2)

--- Word size
wordSize :: Int
wordSize = finiteBitSize (0 :: Prefix)

branchingMode :: Mask -> BM Mask
branchingMode m
  | m .&. 0xFFFF0000 == m = MBinary m
  | otherwise = M2B $ if even (countLeadingZeros m) then m + shiftR m 1 else m + shiftL m 1

-- Calculate and split node
link :: Timestamp -> RT a -> Prefix -> RT a -> RT a
link p1 t1 p2 = linkWithMask (branchingMode $ branchMask p1 p2) p1 t1 p2

linkWithMask :: BM Mask -> Timestamp -> RT a -> Prefix -> RT a -> RT a
linkWithMask bm k t1 p2 t2 =
  case bm of
    (MBinary m)
        | zero k m -> Branch p m (NB t1 t2)
        | otherwise -> Branch p m (NB t2 t1)
       where
           p = mask k m
    (M2B m) -> Branch p m $ handleBranch rm1 rm2
        where p = mask k m
              rm1 = reduceMask k m
              rm2 = reduceMask p2 m
              handleBranch a b =
                case (a,b) of
                  (Z,F) -> N2B t1 t2 Empty Empty
                  (F,Z) -> N2B t2 t1 Empty Empty
                  (Z,S) -> N2B t1 Empty t2 Empty
                  (S,Z) -> N2B t2 Empty t1 Empty
                  (Z,T) -> N2B t1 Empty Empty t2
                  (T,Z) -> N2B t2 Empty Empty t1
                  (F,S) -> N2B Empty t1 t2 Empty
                  (S,F) -> N2B Empty t2 t1 Empty
                  (F,T) -> N2B Empty t1 Empty t2
                  (T,F) -> N2B Empty t2 Empty t1
                  (S,T) -> N2B Empty Empty t1 t2
                  (T,S) -> N2B Empty Empty t2 t1
                  (_,_) -> N2B Empty Empty Empty Empty    -- Should not happen

-- Insert function
insert :: Timestamp -> a -> RT a -> RT a
insert !k !v t =
  case t of
    Branch p m childs
      | nomatch k p m -> link k (Leaf k v) p t
      | otherwise -> Branch p m (handleChilds k v m childs)
    Leaf p _
      | k == p -> Leaf k v
      | otherwise -> link k (Leaf k v) p t
    Empty -> Leaf k v

handleChilds :: Timestamp -> a -> Mask -> Children (RT a) -> Children (RT a)
handleChilds !k !v m c =
  case c of
    (NB l r)
        | zero k m -> NB (insert k v l) r
        | otherwise -> NB l (insert k v r)
    (N2B z f s t) -> case rm of
                      Z -> N2B (insert k v z) f s t
                      F -> N2B z (insert k v f) s t
                      S -> N2B z f (insert k v s) t
                      T -> N2B z f s (insert k v t)
                    where rm = reduceMask k m

reduceMask :: Timestamp -> Mask -> B2
reduceMask t m
  | m - masked == m  = Z
  | m - masked == 0  = T
  | m - masked == shiftL masked 1 = F
  | otherwise = S
    where masked = t .&. m


a = [(x, "40") | x <- [1613303665..1613819332]]
y = [(x, "40") | x <- [0..2]]

x = foldl' (\rt (t, v) -> insert t v rt) empty a

main1 :: RT String -> IO ()
main1 !t = do putStrLn "DONE"
