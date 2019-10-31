take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ []  = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a  -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)] 
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

--My Version
elem' :: (Eq a) => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs) =  n == x || elem' n xs

--Book Version

elem1 :: (Eq a) => a -> [a] -> Bool  
elem1 a [] = False  
elem1 a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs   

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

--filter' :: (a -> Bool)  -> [a] -> [a]
--filter' f [] = []
---filter' f (x:xs) = f x : filter f xs 

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallSorted = quicksort [a | a <- xs , a <=x]
        biggerSorted = quicksort [a | a <- xs, a > x]
        in smallSorted  ++ [x] ++ biggerSorted

    