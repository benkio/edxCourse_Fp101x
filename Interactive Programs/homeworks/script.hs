-- Ex 1
putStr'1 [] = return ""
putStr'1 (x : xs) = putChar x >> putStr'1 xs

putStr'2 [] = return ()
putStr'2 (x : xs) = putChar x >> putStr'2 xs

{-
putStr'3 [] = return ()
putStr'3 (x : xs) = putChar x >>= putStr'3 xs

putStr'4 [] = return ()
putStr'4 (x : xs) = putStr'4 xs >>= putChar x
-}

-- Ex 2

putStrLn'1 [] = putChar '\n'
putStrLn'1 xs = putStr xs >> putStrLn'1 ""

putStrLn'2 [] = putChar '\n'
putStrLn'2 xs = putStr xs >> putChar '\n'

putStrLn'3 [] = putChar '\n'
putStrLn'3 xs = putStr xs >>= \x -> putChar '\n'

{-
putStrLn'4 [] = putChar '\n'
putStrLn'4 xs = putStr xs >> \x -> putChar '\n'
-}

putStrLn'5 [] = putChar '\n'
putStrLn'5 xs = putStr xs >> putStr "\n"

putStrLn'6 [] = putChar '\n'
putStrLn'6 xs = putStr xs >> putStrLn'6 "\n"

{-
putStrLn'7 [] = return ""
putStrLn'7 xs = putStrLn'7 xs >> putStr "\n"
-}
{-
putStrLn'8 [] = putChar "\n"
putStrLn'8 xs = putStr xs >> putChar '\n'
-}

-- Ex 3

getLine'1 = get1 ""

get1 :: String -> IO String
get1 xs
  = do x <- getChar
       case x of
            ' ' -> return xs
            '\n' -> return xs
            _ -> get1 (xs ++ [x])

getLine'2 = get2 ""

get2 :: String -> IO String
get2 xs
  = do x <- getChar
       case x of
            '\n' -> return xs
            _ -> get2 (x : xs)

getLine'3 = get3 []

get3 :: String -> IO String
get3 xs
  = do x <- getChar
       case x of
            '\n' -> return xs
            _ -> get3 (x : xs)

getLine'4 = get4 []

get4 :: String -> IO String
get4 xs
  = do x <- getChar
       case x of
            '\n' -> return (x : xs)
            _ -> get4 (xs ++ [x])


-- Ex 4

interact'1 f
  = do input <- getLine
       putStrLn (f input)

interact'2 f
  = do input <- getLine
       putStrLn input

interact'3 f
  = do input <- getChar
       putStrLn (f input)

interact'4 f
  = do input <- getLine
       putStr (f input)

--Ex 5
{-
sequence_'1 [] = return []
sequence_'1 (m : ms) = m >> \_ -> sequence_'1 ms
-}
sequence_'2 [] = return ()
sequence_'2 (m : ms) = (foldl (>>) m ms) >> return ()

sequence_'3 ms = foldl (>>) (return ()) ms

sequence_'4 [] = return ()
sequence_'4 (m : ms) = m >> sequence_'4 ms

sequence_'5 [] = return ()
sequence_'5 (m : ms) = m >>= \_ -> sequence_'5 ms

-- sequence_'6 ms = foldr (>>=) (return ()) ms

sequence_'7 ms = foldr (>>) (return ()) ms

sequence_'8 ms = foldr (>>) (return []) ms
