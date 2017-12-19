import Data.Char (digitToInt)

asInt xs = loop 0 xs
loop :: Int -> String ->Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

three x = 3
non_term x = if x < 5
             then x + non_term (x+1)
             else 5
maxi :: Int -> Int -> Int
maxi x y | x>=y = x
           | otherwise = y

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

factorial_one :: Int -> Int
factorial_one x = if x >1 
                  then x * factorial_one (x-1)
                  else 1



factorial_two :: Int -> Int
factorial_two 0 = 1
factorial_two n = n * factorial_two (n-1)

biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

foo :: Integer -> Integer
foo 0 = 16
foo 1
    | "Haskell"> "C++" = 3
    | otherwise        = 4
foo n
    | n < 0            = 0
    | n `mod` 17 ==2   = -43
    | otherwise        = n + 3

data Person = Person String Int String
    deriving Show

brent = Person "brent" 31 "hi"

myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs




toDigitsRev :: Integer -> [Integer]
toDigitsRev n
        | n <=0 = []
        | otherwise  = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n 
        | n >0 = toDigits (n `div` 10) ++ [n `mod` 10]
        | otherwise = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = [] 
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:ys) = x : y*2 : doubleEveryOther ys

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate x 
        | sumDigits (doubleEveryOther (toDigitsRev x)) `mod` 10 == 0 = True
        | otherwise = False


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c
       | x == 1 =[(a , c)]
       | otherwise = hanoi (x-1) a c b ++ hanoi 1 a b c ++ hanoi (x-1) b a c

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<=x) xs) ++ [x] ++ quicksort (filter (>x) xs)


elem' ::(Ord a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

maximum' ::(Ord a) => [a] -> a
maximum' = foldr1 (\acc x -> if x> acc then x else acc)
    
