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
