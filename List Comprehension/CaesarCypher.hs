import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
 | isSpace c = c
 | otherwise = int2let ((let2int (toLower c) + n) `mod` 26)

encode :: Int -> String -> String
encode n xs = [shift n x | x  <- xs]
