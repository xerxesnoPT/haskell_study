multiThree :: (Num a)=> a -> a -> a ->a
multiThree x y z = x * y * z

twicef :: (a -> a) -> a -> a 
twicef f x = f (f x)


zipWith' :: (a-> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs


flip' :: (a -> b-> c) -> b -> a -> c
flip' f x y = f y x


-- map and filte
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x: filter' f xs
    | otherwise = filter' f xs

findNumber :: (Integral a) =>[a] -> a
findNumber xs = head (filter (\x -> x `mod` 3829 ==0) xs)

findNumber' :: (Integral a) => [a] -> a
findNumber' xs = sum (takeWhile (<10000) (filter odd (map (^2) xs)))

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x
    | odd x = x : collatz (3 * x + 1)
    | otherwise = x : collatz (x `div` 2)
