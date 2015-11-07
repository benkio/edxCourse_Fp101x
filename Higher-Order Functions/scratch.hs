 -- All functions

myall p xs = and (map p xs)
-- myall2 p xs = map p (and xs)
myall3 p = and . map p
myall4 p = not . any (not . p)
-- myall5 p = map p . and
myall6 p xs = foldl (&&) True (map p xs)
myall7 p xs = foldr (&&) False (map p xs)
myall8 p = foldr (&&) True . map p

-- Any functions

-- myany p = map p . or
myany2 p = or . map p
myany3 p xs = length (filter p xs) > 0
myany4 p = not . null . dropWhile (not . p)
myany5 p = null . filter p
myany6 p xs = not (all (\x -> not (p x)) xs)
myany7 p xs = foldr (\x acc -> (p x) || acc) False xs
myany8 p xs = foldr (||) True (map p xs)

-- TakeWhile functions

myTakeWhile _ [] = []
myTakeWhile p (x : xs)
  | p x = x : myTakeWhile p xs
  | otherwise = myTakeWhile p xs

myTakeWhile2 _ [] = []
myTakeWhile2 p (x : xs)
  | p x = x : myTakeWhile2 p xs
  | otherwise = []

myTakeWhile3 _ [] = []
myTakeWhile3 p (x : xs)
  | p x = myTakeWhile3 p xs
  | otherwise = []

myTakeWhile4 p = foldl (\acc x -> if p x then x : acc else acc) []

-- DropWhile functions

myDropWhile _ [] = []
myDropWhile p (x : xs)
  | p x = myDropWhile p xs
  | otherwise = x : xs

myDropWhile2 _ [] = []
myDropWhile2 p (x : xs)
  | p x = myDropWhile2 p xs
  | otherwise = xs

myDropWhile3 p = foldr (\x acc -> if p x then acc else x : acc) []

myDropWhile4 p = foldl add []
  where add [] x = if p x then [] else [x]
        add acc x = x : acc

-- Map functions

myMap f = foldr (\x xs -> xs ++ [f x]) []
myMap2 f = foldr (\x xs -> f x ++ xs) []
myMap3 f = foldl (\xs x -> f x : xs) []
myMap4 f = foldl (\xs x -> xs ++ [f x]) []

-- Filter Functions

myFilter p = foldl (\xs x -> if p x then x : xs else xs) []
myFilter2 p = foldr (\x xs -> if p x then x : xs else xs) []
myFilter3 p = foldr (\x xs -> if p x then xs ++ [x] else xs) []
--myFilter4 p = foldl (\x xs -> if p x then xs ++ [x] else xs) []

-- Dec2int Functions

myDec2int = foldr (\x y -> 10 * x + y) 0
myDec2int2 = foldl (\x y -> x + 10 * y) 0
myDec2int3 = foldl (\x y -> 10 * x + y) 0
myDec2int4 = foldr (\x y -> x + 10 * y) 0

-- ex 8

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

--sumsqreven = compose [sum,map (^2), filter even]

-- myCurry Functions

myCurry f = \x y -> f x y
myCurry2 f = \x y -> f
myCurry3 f = \x y -> f (x,y)
myCurry4 f = \(x,y) -> f x y

-- uncurry Functions

myUncurry f = \(x,y) -> f x y
myUncurry2 f = \x y -> f (x,y)
myUncurry3 f = \(x,y) -> f
myUncurry4 f = \x y -> f
 
--ex 9

type Bit = Int

unfold p h t x
 | p x = []
 | otherwise = h x : unfold p h t (t x)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- chop8' = unfold [] (drop 8) (take 8)
chop8'2 = unfold null (take 8) (drop 8)
chop8'3 = unfold null (drop 8) (take 8)
chop8'4 = unfold (const False) (take 8) (drop 8)

-- map unfold
map' f = unfold null (f) tail
map'2 f = unfold null (f (head)) tail
map'3 f = unfold null (f . head) tail
--map'4 f = unfold empty (f . head) tail

-- Iterate Function

iterate' f = unfold (const False) id f
iterate'2 f = unfold (const False) f f
iterate'3 f = unfold (const True) id f
iterate'4 f = unfold (const True) f f
