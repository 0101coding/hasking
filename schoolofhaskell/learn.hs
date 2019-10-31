
{- data IntList = E | C Int IntList
    deriving Show

absAll :: IntList -> IntList
absAll E = E
absAll (C x xs) = C (abs x) (absAll xs) 

squareAll :: IntList -> IntList
squareAll E = E
squareAll (C x xs) = C (x * x) (squareAll xs)

addOneToAll :: IntList -> IntList
addOneToAll E = E
addOneToAll (C x xs) = C (x + 1) (addOneToAll xs) 



myIntList = C 2 (C (-3) (C 5 E))

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ E          = E
mapIntList f (C x xs)   = C (f x)   (mapIntList f xs)



addOneToAll xs = mapIntList addOne xs

absAll xs = mapIntList abs xs

squareAll xs = mapIntList square xs


keepOnlyEven :: IntList -> IntList
keepOnlyEven E = E
keepOnlyEven (C x xs)
        | even x = C x (keepOnlyEven xs)
        | otherwise = keepOnlyEven xs


filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ E = E 
filterIntList f (C x xs) 
    | f x                = C x (filterIntList f xs)
    | otherwise          = filterIntList f xs 

-}

data List t = E | C t (List t) deriving Show

addOne x = x + 1
square x = x * x

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

filterList :: (a -> Bool) -> List a -> List a
filterList _ E = E 
filterList f (C x xs) 
                | f x = C x (filterList f xs)
                | otherwise = filterList f xs

                
mapList :: (a -> b) -> List a -> List b 
mapList _ E = E 
mapList f (C x xs) = C (f x) (mapList f xs)