-- original statement
import Data.List
import Data.Char
--import Hugs.IOExts (unsafeCoerce)

data Nat = Zero | Succ Nat deriving Show

-- answer a
natToIntegera Zero = 0
natToIntegera (Succ n) = natToIntegera n + 1

-- answer b
natToIntegerb (Succ n) = natToIntegerb n + 1
natToIntegerb Zero = 0

-- answer c
natToIntegerc n = natToIntegerc n

-- answer d
natToIntegerd (Succ n) = 1 + natToIntegerd n
natToIntegerd Zero = 0

-- answer e
natToIntegere Zero = 1
natToIntegere (Succ n) = (1 + natToIntegere n) - 1

-- answer f
natToIntegerf = head . m
    where m Zero = [0]
          m (Succ n) = [sum [x | x <- (1 : m n)]]

-- answer g
natToIntegerg :: Nat -> Integer
natToIntegerg = \n -> genericLength [c | c <- show n, c == 'S']

-- the pattern (n+1) doesn't compile and i don't want to know why, fuck

{-
-- answer h
natToIntegerh :: Nat -> Integer
natToIntegerh = \n -> length [c | c <- show n, c == 'S']


integerToNat1 0 = Zero
integerToNat1 (n + 1) = Succ (integerToNat1 n)
-}

integerToNat2 0 = Succ Zero
integerToNat2 n = (Succ (integerToNat2 n))


--integerToNat3 n = product [(unsafeCoerce c) :: Integer | c <- show n]

integerToNat4 n = integerToNat4 n

{-
integerToNat5 (n + 1) = Succ (integerToNat5 n)
integerToNat5 0 = Zero

integerToNat6 (n + 1) = let m = integerToNat6 n in Succ m
integerToNat6 0 = Zero

integerToNat7 = head . m
  where {
        ; m 0 = [0]
        ; m (n + 1) = [sum [x | x <- (1 : m n)]]
        }

integerToNat8 :: Integer -> Nat
integerToNat8 = \n -> genericLength [c | c <- show n, isDigit c]
-}

-- adds 

add1 Zero n = n
add1 (Succ m) n = Succ (add1 n m)

add2 (Succ m) n = Succ (add2 n m)
add2 Zero n = n

add3 Zero n = Zero
add3 (Succ m) n = Succ(add3 m n)

add4 (Succ m) n = Succ (add4 m n)
add4 Zero n = Zero

add5 n Zero = Zero
add5 n (Succ m) = Succ (add5 n m)

add6 n (Succ m) = Succ (add6 n m)
add6 n Zero = Zero

add7 n Zero = n
add7 n (Succ m) = Succ (add7 m n)

add8 n (Succ m) = Succ (add8 m n)
add8 n Zero = n

-- mults

mult1 Zero Zero = Zero
mult1 m (Succ n ) = add1 m (mult1 m n)

mult2 m Zero = Zero
mult2 m (Succ n) = add1 m (mult2 m n)

mult3 m Zero = Zero
mult3 m (Succ n) = add1 n (mult3 m n)

mult4 m Zero = Zero
mult4 m n = add1 m (mult4 m (Succ n))

-- occurs

data Tree = Leaf Integer | Node Tree Integer Tree

occurs1 m (Leaf n) = m == n
occurs1 m (Node l n r)
  = case compare m n of
         LT -> occurs1 m l
         EQ -> True
         GT -> occurs1 m r

occurs2 m (Leaf n) = m == n
occurs2 m (Node l n r)
  = case compare m n of
         LT -> occurs2 m r
         EQ -> True
         GT -> occurs2 m l
{-
occurs3 m (Leaf n) =  compare m n
occurs3 m (Node l n r)
  = case compare m n of
         LT -> occurs3 m l
         EQ -> True
         GT -> occurs3 m r
-}
occurs4 m (Leaf n) = m == n
occurs4 m (Node l n r)
  = case compare m n of
         LT -> occurs4 m l
         EQ -> False
         GT -> occurs4 m r

occurs5 m (Leaf n) = m == n
occurs5 m (Node l n r)
  | m == n = True
  | m < n = occurs5 m l
  | otherwise = occurs5 m r

occurs6 m (Leaf n) = m == n
occurs6 m (Node l n r)
  | m == n = True
  | m > n = occurs6 m l
  | otherwise = occurs6 m r

{-
occurs7 m n = m == n
occurs7 m (Node l n r)
  | m == n = True
  | m < n = occurs7 m l
  | otherwise = occurs7 m r

occurs8 m n = m == n
occurs8 m (Node l n r)
  | m == n = False
  | m < n = occurs8 m r
  | otherwise = occurs8 m l 
-}

--leaves

data BTree = BLeaf Integer | BNode BTree BTree deriving Show

leaves1 (BLeaf x) = x
leaves1 (BNode l r) = leaves1 l + leaves1 r
balanced1 (BLeaf _) = True
balanced1 (BNode l r)
  = abs (leaves1 l - leaves1 r) <= 1 || balanced1 l || balanced1 r

{-
leaves2 (BLeaf _) = True
leaves2 (BNode l r) = leaves2 l + leaves2 r
balanced2 (BLeaf _) = True
balanced2 (BNode l r) = abs (leaves2 l - leaves2 r) <= 1

leaves3 (BLeaf _) = True
leaves3 (BNode l r) = leaves3 l + leaves3 r
balanced3 (BLeaf _) = True
balanced3 (BNode l r) = abs (leaves3 l + leaves3 r) <= 1
-}
leaves4 (BLeaf _) = 1
leaves4 (BNode l r) = leaves4 l + leaves4 r
balanced4 (BLeaf _) = True
balanced4  (BNode l r) = abs (leaves4 l - leaves4 r) <= 1 && balanced4 l && balanced4 r


-- halves
halves1 xs = splitAt (length xs `div` 2) xs
balance1 [x] = BLeaf x
balance1 xs = BNode (balance1 ys) (balance1 zs)
  where (ys, zs) = halves1 xs

{-
halves2 xs = splitAt (length xs / 2) xs
balance2 [x] = BLeaf x
balance2 xs = BNode (balance1 ys) (balance1 zs)
 where (ys,zs) = halves2 xs

halves3 xs = splitAt (length xs `div` 2) xs
balance3 [x] = BLeaf x
balance3 xs = BNode ys zs
  where (ys,zs) = balance3 (halves3 xs)

halves4 xs = splitAt (length xs `div` 2) xs
balance4 x = BLeaf x
balance4 xs = BNode (balance1 ys) (balance1 zs)
  where (ys,zs) = halves4 xs
-}

