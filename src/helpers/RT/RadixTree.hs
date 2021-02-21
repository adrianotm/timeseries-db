{-# LANGUAGE BangPatterns #-}
module RT.RadixTree where

import           Data.Array
import           Data.Bits
import           Data.Foldable
import           Data.Word
import           Debug.Trace

type Timestamp = Word32

type Prefix = Word32
type Mask = Word32
type Chunk = Word8

-- Branching Mode
data BM a =
    M2 !a
  | M4 !a

data Children a =
      N2 !a !a
    | N4 !(Array Chunk a)
  deriving (Show)

data RT a =
    Branch !Prefix !Mask !(Children (RT a))
  | Leaf !Prefix !a
  | Empty
  deriving (Show)

debug = flip trace


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
  | m .&. 0xFFFF0000 == m = M2 m
  | otherwise = M4 $ if even (countLeadingZeros m) then m + shiftR m 1 else m + shiftL m 1

-- Calculate and split node
link :: Timestamp -> RT a -> Prefix -> RT a -> RT a
link p1 t1 p2 = linkWithMask (branchingMode $ branchMask p1 p2) p1 t1 p2

linkWithMask :: BM Mask -> Timestamp -> RT a -> Prefix -> RT a -> RT a
linkWithMask (M2 m) k t1 _ t2
  | zero k m = Branch p m (N2 t1 t2)
  | otherwise = Branch p m (N2 t2 t1)
 where
     p = mask k m

linkWithMask (M4 m) k t1 p2 t2 = Branch p m (N4 $ array (0, 3)
                                                        [(i, rt) | i <- [0..3],
                                                                   let rt | i == rm1 = t1
                                                                          | i == rm2 = t2
                                                                          | otherwise = Empty])
  where p = mask k m
        rm1 = reduceMask k m
        rm2 = reduceMask p2 m

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
    (N2 l r)
        | zero k m -> N2 (insert k v l) r
        | otherwise -> N2 l (insert k v r)
    (N4 arr) -> N4 $ arr // [(rm, insert k v rt)]
          where rt = arr ! rm
                rm = reduceMask k m

reduceMask :: Timestamp -> Mask -> Word8
reduceMask t m
  | m - masked == m  = 0
  | m - masked == 0  = 3
  | m - masked == shiftL masked 1 = 1
  | otherwise = 2
    where masked = t .&. m


a = [(x, "40") | x <- [1613303665..1613819332]]

x = foldl' (\rt (t, v) -> insert t v rt) empty a

main1 :: RT String -> IO ()
main1 !t = do putStrLn "DONE"
