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

-- es 6

sequence'1 [] = return []
sequence'1 (m : ms)
  = m >>=
      \a ->
       do as <- sequence'1 ms
          return (a : as)

{-
sequence'2 ms = foldr func (return ()) ms
  where
        func :: (Monad m) => m a -> m [a] -> m [a]
        func m acc
          = do x <- m
               xs <- acc
               return (x : xs)

sequence'3 ms = foldr func (return []) ms
  where
        func :: (Monad m) => m a -> m [a] -> m [a]
        func m acc = m : acc

sequence'4 [] = return []
sequence'4 (m : ms) = return (a : as)
  where
      a <- m
      as <- sequence'4 ms
-}
sequence'5 ms = foldr func (return []) ms
  where
        func :: (Monad m) => m a -> m [a] -> m [a]
        func m acc
          = do x <- m
               xs <- acc
               return (x : xs)

{-
sequence'6 [] = return []
sequence'6 (m : ms)
  = m >> \a ->
          do as <- sequence'6 ms
             return (a : as)

sequence'7 [] = return []
sequence'7 (m : ms) = m >> \a ->
       as <- sequence'7 ms
       return (a : as)
-}
sequence'8 [] = return []
sequence'8 (m : ms)
  = do a <- m
       as <- sequence'8 ms
       return (a : as)

--Ex 7

mapM'1 f as = sequence'1 (map f as)

mapM'2 f [] = return []
mapM'2 f (a : as)
  = f a >>= \b -> mapM'2 f as >>= \bs -> return (b : bs)
{-
mapM'3 f as = sequence_'2 (map f as)

mapM'4 f [] = return []
mapM'4 f (a : as)
  = f a >> \b -> mapM'4 f as >> \bs -> return (b : bs)


mapM'5 f [] = return []
mapM'5 f (a : as) =
  do
     f a -> b
     mapM'5 f as -> bs
     return (b : bs)
-}

mapM'6 f [] = return []
mapM'6 f (a : as)
  = do b <- f a
       bs <- mapM'6 f as
       return (b : bs)

mapM'7 f [] = return []
mapM'7 f (a : as)
  = f a >>=
      \b ->
       do bs <- mapM'7 f as
          return (b : bs)

mapM'8 f [] = return []
mapM'8 f (a : as)
  = f a >>=
      \b ->
       do bs <- mapM'8 f as
          return (bs ++ [b])

-- Es 8
{-
filterM'1 _ [] = return []
filterM'1 p (x : xs)
  = do flag <- p x
       ys <- filterM'1 p xs
       return (x : xs)
-}
filterM'2 _ [] = return []
filterM'2 p (x : xs)
  = do flag <- p x
       ys <- filterM'2 p xs
       if flag then return (x : ys) else return ys
{-
filterM'3 _ [] = return []
filterM'3 p (x : xs)
  = do ys <- filterM'3 p xs
       if p x then return (x : ys) else return ys
-}
filterM'4 _ [] = return []
filterM'4 p (x : xs)
 = do flag <- p x
      ys <- filterM'4 p xs
      if flag then return ys else return (x : ys)

-- Es 9

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (x : xs) = f a x >>= \b -> foldLeftM f b xs

-- Es 10
foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f b [] = return b
foldRightM f b (x: xs) = foldRightM f b xs >>= \c -> f x c

-- es 11
liftM1 f m
  = do x <- m
       return (f x)

liftM2 f m = m >>= \a -> f a
liftM3 f m = m >>= \a -> return (f a)
liftM4 f m = return (f m)
liftM5 f m = m  >>= \a -> m >>= \b -> return (f a)
liftM6 f m = m >>= \a -> m >>= \b -> return (f b)
liftM7 f m = mapM f [m]
liftM8 f m = m >> \a -> return (f a)
