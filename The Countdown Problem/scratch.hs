{-
removeone1 x [] = [x]
removeone1 x ys
  | x == head ys = ys
  | otherwise = y : removeone1 x ys
-}
removeone2 x [] = []
removeone2 x (y : ys)
  | x == y = ys
  | otherwise = x : removeone2 y ys

removeone3 x [] = []
removeone3 x (y : ys)
  | x == y = ys
  | otherwise = removeone3 x ys

removeone4 x [] = []
removeone4 x (y : ys)
  | x == y = ys
  | otherwise = y : removeone4 x ys


isChoice1 [] _ = True
isChoice1 (x : xs) [] = True
isChoice1 (x : xs) ys = elem x ys && isChoice1 xs (removeone4 x ys)

isChoice2 [] _ = False
isChoice2 (x : xs)  [] = True
isChoice2 (x : xs) (y : ys)
  = elem y ys && isChoice2 xs (removeone4 x ys)

isChoice3 [] _ = True
isChoice3 xs [] = True
isChoice3 (x : xs) (y : ys)
  = elem y xs && isChoice3 xs (removeone4 x ys)

isChoice4 [] _ = True
isChoice4 (x : xs) [] = False
isChoice4 (x : xs) ys = elem x ys && isChoice4 (removeone4 x xs) ys
