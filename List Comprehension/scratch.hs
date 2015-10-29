
--sum100 = sum [[x*x] | x<-[1 .. 100]]
sum100' = sum [ x ^ 2 | x <- [1 .. 100]]
sum100'' = sum [const 2 x | x <- [1 .. 100]]
sum100''' = foldl (+) (1) [ x^2 | x <- [1 .. 100]]

replacate n a = [True | _ <- [1 .. n]]
replacate' n a = [n | _ <- [1 .. n]]
replacate'' n a = [a | _ <- [1 .. a]]
replacate''' n a = [a | _ <- [1 .. n]]

pyths n = [(x,y,z) | x <- [1..n], y <- [1..x], z<-[1..y], x^2 + y^2 == z^2]
pyths' n = [(x,y,z) | x <- [1..n], y <- [x..n], z<-[y..n], x^2 + y^2 == z^2]
pyths'' n = [(x,y,z) | x <- [1..n], y <- [1..n], z<-[1..n], x^2 + y^2 == z^2]
pyths''' n = [(x,y, (x^2 + y ^ 2)) | x <- [1..n], y <- [1..n]]

factors :: Int -> [Int]

factors n = [x | x <-[1..n], n `mod` x == 0]

perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect num = sum (factors num) == num
--perfects' n =
--perfects'' n =
--perfects''' n = 
