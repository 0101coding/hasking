greaterThan100 :: [Integer] -> [Integer]
--greaterThan100 x = filter (> 100) x
--greaterThan100 x = filter (\y -> y > 100) x

greaterThan100 [] = []
greaterThan100 (x:xs)  
        | x > 100 = x : greaterThan100 xs
        | otherwise = greaterThan100 xs 

dot :: (b -> c) -> (a -> b) -> (a -> c)
dot f g = \x -> f (g x)

myTest :: [Integer] -> Bool
--myTest xs = even (length (greaterThan100 xs))
myTest = even `dot` length `dot` greaterThan100

f :: (Int, Int) -> Int
f (x, y) = 2*x + y

schönfinkel :: ((a,b) -> c) -> a -> b -> c
schönfinkel f x y = f (x,y)

unschönfinkel :: (a -> b -> c) -> (a,b) -> c
unschönfinkel f (x,y) = f x y

{- foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
        | x > 3     = (7*x + 2) + foobar xs
        | otherwise = foobar xs -}

foobar :: [Integer] -> Integer
foobar = sum . map (\x -> 7*x + 2) . filter (>3)


sum' :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' :: [Integer] -> Integer
product' [] = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

fold' :: (b -> b -> b) -> b -> [b] -> b
fold' f z [] = z
fold' f z (x:xs) = f x ( fold' f z xs )

sum'' = fold' (+) 0
product'' = fold' (*) 1
length'' = fold' (\_ s -> 1 + s) 0

myList = [1,2,3,4,5]


fold' (\_ s -> 1 + s) 0 [1,2,3,4,5]
--first fold'

