-- fibonacci

fibs1 = 1 : [x + y | (x, y) <- zip fibs1 (tail fibs1)]
fibs2 = 0 : 1 : zipWith (*) fibs2 (tail fibs2)
fibs3 = 0 : 1 : [x + y | (x, y) <- zip fibs3 (tail fibs3)]
fibs4 = 1 : 1 : [x + y | (x, y) <- zip (tail fibs4) fibs4]

-- fibonacci 2

fib1 n = last (take n fibs3)
fib2 n = head (drop (n - 1) fibs3)
fib3 n = fibs3 !! n
--fib4 n = index fibs3 n

-- large Fibs

largeFib1 = head (dropWhile (<= 1000) fibs3)
largeFib2 = last (take 19 fibs3)
largeFib3 = filter (> 1000) fibs3
largeFib4 = head (drop 1000 fibs3)

-- repeat tree

data Tree a = Leaf | Node (Tree a) a (Tree a)

repeat :: a -> [a]
repeat x = xs
  where xs = x : xs

-- repeatTree1 x = Node x x x
repeatTree2 x = Node t x t
  where t = repeatTree2 x

-- repeatTree3 x = repeatTree3 (Node Leaf x Leaf)
-- repeatTree4 x = Node t x t
--   where t = repeatTree4 (Node t x t)
