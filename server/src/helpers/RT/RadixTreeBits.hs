{-# LANGUAGE BangPatterns #-}
module RT.RadixTreeBits where

import           Data.Bits
import           Data.Foldable
import           Data.IntMap.Strict (IntMap, fromList)
import           Data.Word

type Timestamp = Word32

type Prefix = Word32
type Mask = Word32

data RT a =
    Branch !Prefix !Mask !(RT a) !(RT a)
  | Leaf !Prefix a
  | Empty
  deriving (Show)


--- Empty RadixTree
empty :: RT a
empty = Empty

--- Get the Prefix mask
mask :: Timestamp -> Mask -> Prefix
mask i m = i .&. ((-m) `xor` m)

--- Do the prefixes match
nomatch :: Timestamp -> Prefix -> Mask -> Bool
nomatch i p m = mask i m /= p

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

-- Calculate and split node
link :: Prefix -> RT a -> Prefix -> RT a -> RT a
link p1 t1 p2 = linkWithMask (branchMask p1 p2) p1 t1

linkWithMask :: Mask -> Prefix -> RT a -> RT a -> RT a
linkWithMask m p1 t1 t2
  | zero p1 m = Branch p m t1 t2
  | otherwise = Branch p m t2 t1
 where
     p = mask p1 m

-- Insert function
insert :: Timestamp -> a -> RT a -> RT a
insert !k !x t =
  case t of
    Branch p m l r
      | nomatch k p m -> link k (Leaf k x) p t
      | zero k m      -> Branch p m (insert k x l) r
      | otherwise     -> Branch p m l (insert k x r)
    Leaf ky _
      | k==ky         -> Leaf k x
      | otherwise     -> link k (Leaf k x) ky t
    Empty -> Leaf k x

a = [(x, "40") | x <- [1613303665..1613819332]]

-- x = foldl' (\rt (t, v) -> insert t v rt) empty a

y = fromList $! a

main1 :: RT String -> IO ()
main1 !t = do putStrLn "DONE"

main2 :: IntMap String -> IO ()
main2 !t = do putStrLn "DONE"
