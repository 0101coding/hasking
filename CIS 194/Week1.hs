y :: Int
y = y + 1

biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

reallyBig :: Integer
reallyBig = 2 ^ (2 ^ ( 2 ^ (2 ^ 2)))

numDigits :: Int
numDigits = length (show reallyBig)

d1,d2 :: Double
d1 = 4.5387
d2 = 6.2831e-4

b1,b2 :: Bool 
b1 = True
b2 = False

s :: String
s = "Hello, Haskell!"