zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x  

flipp :: (a -> b -> c) -> b -> a -> c  
flipp f y x = f x y  

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [1000000,999999..])
    where p x = mod x 3829 == 0

sumAllOdd ::  (Integral a) => a
sumAllOdd = sum (filter odd [10000,9999..0])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (div n 2)
    | odd n = n:chain(n*3 + 1)  

numLongChains :: Int 
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

sum' :: (Num a) => [a] -> a 
sum' xs = foldl(\acc x -> acc + x ) 0 xs

