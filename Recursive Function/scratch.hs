-- Powers

power m 0 = 0
power m n = m * power  m (n - 1)

power2 m 0 = 1
power2 m n = m * power2 m (n - 1)

power3 m 0 = 1
power3 m n = m * power3 m n - 1

power4 m 0 = 1
power4 m n = n * power4 n (m - 1)

power5 m 0 = 1
power5 m n = m * (power5) m (n - 1)

power6 m 0 = 1
power6 m n = m * m * power6 m (n - 2)

power7 m 0 = 1
power7 m n = power7 (m * m) (n - 1)

power8 m 1 = m
power8 m n = m * power8 m (n - 1)

-- length

mylength :: [a] -> Int
mylength [] = 0
mylength (_ : xs) = 1 + length xs

--drop

mydrop :: Int -> [a] -> [a]
mydrop 0 xs = xs
mydrop n [] = []
mydrop n (_ : xs) = mydrop (n - 1) xs

-- init

myinit :: [a] -> [a]
myinit [_] = []
myinit (x : xs) = x : init xs

-- ands

myand [] = True
myand (b : bs) = b && myand bs

myand2 [] = True
myand2 (b : bs)
  | b = and bs
  | otherwise = False

myand3 [] = False
myand3 (b : bs) = b && and bs

myand4 [] = False
myand4 (b : bs) = b || and bs

myand5 [] = True
myand5 (b : bs)
  | b == False = False
  | otherwise = myand5 bs

myand6 [] = True
myand6 (b : bs) = b || myand6 bs

myand7 [] = True
myand7 (b : bs) = myand7 bs && b

myand8 [] = True
myand8 (b: bs)
  | b = b
  | otherwise = myand8 bs

-- concats

myconcat [] = []
myconcat (xs : xss) = xs : myconcat xss

myconcat2 [] = [[]]
myconcat2 (xs : xss) = xs ++ myconcat2 xss

myconcat3 [] = [[]]
myconcat3 (xs : xss) = xs ++ myconcat3 xss

myconcat4 [[]] = []
myconcat4 (xs : xss) = xs ++ myconcat4 xss

-- replicates

{-
myreplicate :: Int -> a -> [a]
myreplicate 1 x = x
myreplicate n x = x : myreplicate (n - 1) x
-}
{-
myreplicate2 :: Int -> a -> [a]
myreplicate2 0 _ = []
myreplicate2 n x = myreplicate2 (n - 1) x : x
-}

myreplicate3 1 _ = []
myreplicate3 n x = myreplicate3 (n - 1) x ++ [x]

myreplicate4 0 _ = []
myreplicate4 n x = x : myreplicate4 (n - 1) x

-- Item

item (x : _) 1 = x
item (_ : xs) n = item xs (n - 1)

item2 (x : _) 0 = x
item2 (_ : xs) n = item2 xs (n - 1)
item2 [] n = 0

item3 (x : _) 0 = x
item3 (_ : xs) n = item3 xs (n - 1)

item4 (x : _) 0 = [x]
item4 (_ : xs) n = item4 xs (n - 1)

-- myelem

myelem _ [] = False
myelem x (y : ys)
  | x == y = True
  | otherwise = myelem x (y : ys)

myelem2 _ [] = False
myelem2 x (y : ys)
  | x == y = True
  | otherwise = myelem2 x (y : ys)

myelem3 _ [] = True
myelem3 x (y : ys)
 | x == y = True
 | otherwise = myelem3 x ys

myelem4 _ [] = False
myelem4 x (y : ys) = x == y

-- mymerge

mymerge [] ys = ys
mymerge xs [] = xs
mymerge (x : xs) (y : ys)
  = if x <= y then x : mymerge xs ys else y : mymerge xs ys

mymerge2 [] ys = ys
mymerge2 xs [] = xs
mymerge2 (x : xs) (y : ys)
  = if x <= y then y : mymerge2 xs (y : ys) else x : mymerge2 (x : xs) ys

mymerge3 [] ys = ys
mymerge3 xs [] = xs
mymerge3 (x : xs) (y : ys)
  = if x <= y then y : mymerge3 (x : xs) ys else x : mymerge3 xs (y : ys)

mymerge4 [] ys = ys
mymerge4 xs [] = xs
mymerge4 (x : xs) (y : ys)
  = if x <= y then x : mymerge3 xs (y : ys) else y : mymerge3 (x : xs) ys

-- msort

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort [] = []
msort xs = mymerge (msort zs) (msort ys)
  where (ys,zs) = halve xs

msort2 [] = []
msort2 [x] = [x]
msort2 xs = mymerge (msort2 ys) (msort2 zs)
  where (ys, zs) = halve xs

msort3 [] = []
msort3 [x] = [x]
msort3 xs = msort3 ys ++ msort3 zs
  where (ys, zs) = halve xs

msort4 [] = []
msort4 [x] = [x]
msort4 xs = msort4 (msort4 ys ++  msort4 zs)
  where (ys, zs) = halve xs
