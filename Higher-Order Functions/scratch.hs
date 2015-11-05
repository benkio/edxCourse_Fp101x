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

