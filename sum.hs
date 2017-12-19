capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of" ++ all ++ "is" ++ [x]

bmiTell :: (RealFloat a) => a-> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're emo"
    | weight / height ^ 2 <= 25.0 = "You're normal"
    | weight / height ^ 2 <= 30.0 = "You're fatter"
    | otherwise                   = "what's the fuck"

maxMy :: (Ord a) => a -> a -> a
maxMy a b 
    | a>b = a
    | otherwise = b


maximu :: (Ord a) => [a] -> a
maximu [] = error "there is something wrong"
maximu [x] = x
maximu (x:xs)
    | x > tailx = x
    | otherwise = tailx
    where tailx = maximu xs

replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' n x
    | n <=0  = []
    | otherwise = x : replicate' (n-1) x

