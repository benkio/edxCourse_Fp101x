
--sum100 = sum [[x*x] | x<-[1 .. 100]]
sum100' = sum [ x ^ 2 | x <- [1 .. 100]]
sum100'' = sum [const 2 x | x <- [1 .. 100]]
sum100''' = foldl (+) (1) [ x^2 | x <- [1 .. 100]]

replacate n a = [True | _ <- [1 .. n]]
replacate' n a = [n | _ <- [1 .. n]]
replacate'' n a = [a | _ <- [1 .. a]]
replacate''' n a = [a | _ <- [1 .. n]]

pyths n = [(x,y,z) | x <- [1..n], y <- [1..x], z <- [1..y], x^2 + y^2 == z^2]
pyths' n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n], x^2 + y^2 == z^2]
pyths'' n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]
pyths''' n = [(x,y, (x^2 + y ^ 2)) | x <- [1..n], y <- [1..n]]

factors :: Int -> [Int]

factors n = [x | x <- [1..n], n `mod` x == 0]

perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect num = sum (factors num) == num
perfects' n = [x | x <- [1..n], isPerfect x]
  where isPerfect num = sum (init (factors num)) == num
perfects'' n = [isPerfect x | x <- [1..n]]
  where isPerfect num = sum (init (factors num)) == num
--perfects''' n = [x | x<-[1..n], isPerfect x]
--  where isPerfect num = init (factors num) == num

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t,k == k']

positions x xs = find x (zip xs [0..n])
  where n = length xs - 1
position' x xs = find x xs
--position'' x xs = find x (zipWith (+) xs [0..n])
--  where n = length xs - 1
position''' x xs = find n (zip xs [0..x])
  where n = length xs - 1

scalarproduct xs ys = sum [x * y | x <- xs, y <- ys]
scalarproduct'' xs ys = sum [x * y | (x,y) <- xs `zip` ys]
scalarproduct''' xs ys = product (zipWith (+) xs ys)
--scalarproduct'''' xs ys = sum (product [(x,y) | x <- xs, y <- ys])


