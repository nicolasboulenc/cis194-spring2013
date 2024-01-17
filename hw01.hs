
-- https://www.seas.upenn.edu/~cis1940/spring13/hw/01-intro.pdf

-- exercise 1
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (div n 10) ++ [mod n 10] 


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise =  mod n 10 : toDigits (div n 10)


-- exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = [ (xs !! i) * if even (length xs - i) then 2 else 1 | i <- [0..length xs - 1] ]


-- exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum . toDigits $ x) + sumDigits xs


-- exercise 4
validate :: Integer -> Bool
validate n
    | n <= 0 = False
    | otherwise = mod (sumDigits . doubleEveryOther . toDigits $ n) 10 == 0

-- 4012888888881881
-- [4  0  1  2  8  8  8  8  8  8  8  8  1  8  8  1]
-- [8  0  2  2 16  8 16  8 16  8 16  8  2  8 16  1]
-- [8 0 2 2 1 6 8 1 6 8 1 6 8 1 6 8 2 8 1 6 1]


-- exercise 5