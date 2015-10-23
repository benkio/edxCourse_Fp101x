import Prelude hiding ((||))

--halve xs = (take n xs, drop n xs)
--    where n = length xs / 2

halve' xs = splitAt (length xs `div` 2) xs

halve'' xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
    where n = length xs

halve''' xs = splitAt (length xs `div` 2)

halve'''' xs = (take n xs, drop (n + 1) xs)
    where n = length xs `div` 2

halve''''' xs = splitAt (div (length xs) 2) xs

--halve'''''' xs = splitAt (length xs / 2) xs

halve''''''' xs = (take n xs, drop n xs)
    where n = length xs `div` 2

safetail xs = if null xs then [] else tail xs

safetail' [] = []
safetail' (_:xs) = xs

safetail'' (_:xs)
    | null xs = []
    | otherwise = tail xs

safetail''' xs
    | null xs = []
    | otherwise = tail xs

safetail'''' xs = tail xs
safetail'''' [] = []

safetail''''' [] = []
safetail''''' xs = tail xs

safetail'''''' [x] = [x]
safetail'''''' (_:xs) = xs

safetail'''''''
    = \ xs ->
        case xs of
            [] -> []
            (_:xs) -> xs


--mult x y z = \x -> (\y -> (\z -> x*y*z))
--mult' x y z = \x -> (x* \y -> (y* \z->z))
--mult'' = \x -> (\y -> (\z -> x*y*z))
--mult''' = ((((\x -> \y) -> \z) -> x * y)* z)

remove n xs = take n xs ++ drop n xs
remove' n xs = drop n xs ++ take n xs
remove'' n xs = take (n+1) xs ++ drop n xs
remove''' n xs = take n xs ++ drop (n+1) xs

funct :: Int -> [a] -> [a]
funct x xs = take (x+1) xs ++ drop x xs
