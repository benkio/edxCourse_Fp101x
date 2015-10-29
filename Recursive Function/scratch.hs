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


