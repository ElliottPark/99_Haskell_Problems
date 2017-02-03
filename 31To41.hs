
import System.IO

main = do
    -- putStrLn $ show $ zip (map prime [2..20]) [2..20]
    -- putStrLn $ show $ sieve 20
    -- putStrLn $ show $ hasFactor 12 [5,6,7,8,9,10,11]
    -- putStrLn $ show $ gcd' 33 64
    -- putStrLn $ show $ coprime 33 66 
    -- putStrLn $ show $ totient_phi 10
    putStrLn $ show $ prime_factors 315

-- Problem 31
-- Determine whether a given integer is a prime number

prime :: (Integral a) => a -> Bool
prime n = foldl (\acc x -> (0 /= (mod n x)) && acc) True [2..(floor (sqrt (fromIntegral n)))]

sieve :: (Integral a) => a -> [a]
sieve n = gen' 2 []
    where gen' i xs 
                | i > n = xs
                | otherwise = if hasFactor i xs
                              then gen' (i+1) xs
                              else gen' (i+1) (i:xs)  --gen' (i - 1) (i:xs)


hasFactor :: (Integral a) => a -> [a] -> Bool
hasFactor n [] = False
hasFactor n (x:xs) = if (rem n x) == 0 then True else hasFactor n xs

letTest :: Int
letTest = let a = 5
              b = 7
              in a + b


-- TODO: Use sieve of Erastothenes

-- Problem 32
-- Determine the greatest common divisor of two positive integer numbers.
-- Use Euclid's algorithm

-- gcd' :: (Integral a) => a -> a -> a
-- gcd' x y = 
--     let mx = div (min x y) 2 in
--       gcd'' $ reverse [1..mx]
--         where gcd'' [] = 0
--               gcd'' (z:zs) = if rem x z == 0 && rem y z == 0 then z else gcd'' zs

-- A better implementation that uses the Euclid's Algorithm
gcd' :: (Integral a) => a -> a -> a
gcd' x y
        | x == y = x
        | x == 0 || y == 0 = 0
        | otherwise = 
            let mx = max x y
                mn = min x y
                rm = rem mx mn in
                  if rm == 0 then mn
                  else if rm == mn then mn else gcd' rm mn
          

-- Problem 33
-- Determine if two numbers are coprime
coprime :: Int -> Int -> Bool
coprime x y = 1 == gcd' x y


-- Problem 34
-- number of integers r such that r(1 <= r < m)
totient_phi :: Int -> Int
totient_phi x = length $ foldr (\y acc -> if coprime x y then y:acc else acc) [] [1..(x-1)]


-- Problem 35
-- Determine the prime factors of a given positive integer.
-- prime_factors :: Int -> [Int]
-- prime_factors x = 
--     let prms = sieve (isqrt x) in
--       foldr (\y acc -> if rem x y == 0 then (rep (div x y) y)++acc else acc) [] prms

prime_factors :: Int -> [Int]
prime_factors x = pf $ sieve (isqrt x)
      where pf x = 

isqrt :: Int -> Int
isqrt = ceiling . sqrt . fromIntegral

-- Problem 36
-- Determine the prime factors of a number and give their multiplicity
prime_factors_mult :: Int -> [(Int)]
prime_factors_mult x = 
    let prms = sieve (isqrt x) in
      foldr (\y acc -> if rem x y == 0 then y:acc else acc) [] prms

rep :: Int -> Int -> [Int]
rep x y = take x (repeat y)


















