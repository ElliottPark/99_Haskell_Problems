
main = do
    -- putStrLn $ show $ encodeModified "aaaabccaadeeee"
    -- putStrLn $ show $ decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
        -- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
    -- putStrLn $ show $ dupli [1, 2, 3]
    -- putStrLn $ show $ repli [1,2,3] 3
    -- putStrLn $ show $ dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 3
    -- putStrLn $ show $ split [1,2,3,4,5,6,7,8,9,10,11,12] 3
    -- putStrLn $ show $ slice [1,2,3,4,5,6,7,8,9,10,11,12] 3 5
    -- putStrLn $ show $ rotate [1,2,3,4,5,6,7] 3
    putStrLn $ show $ remove_at [1,2,3,4,5,6,7] 3

-- Problem 11
-- Modify run-length encoding such that all elements have duplicates
data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified xs = foldr (\x acc-> accum x acc) [] xs
                        where
                            accum y [] = [Single y]
                            accum y ((Single a):as) = if a == y
                                                    then (Multiple 2 a) : as
                                                    else (Single y):(Single a):as
                            accum y ((Multiple i a):as) = if a == y
                                                    then (Multiple (i+1) a) : as
                                                    else (Single y):(Multiple i a):as


-- Problem 12
-- Decode a run-length encoded list
-- decodeModified :: (Eq a) => [(Int, a)] -> [a]
-- decodeModified [] = []
-- decodeModified ((i, c):es) = (take i $ repeat c) ++ (decodeModified es)
decodeModified :: (Eq a) => [ListItem a] -> [a]
decodeModified [] = []
decodeModified ((Multiple i a):es) = (take i $ repeat a) ++ (decodeModified es)
decodeModified ((Single a):es) = a : (decodeModified es)


-- Problem 13
-- Implement run-length encoding without creating sub-lists.
-- That's the same as my version of 11.

-- Problem 14
-- Duplicate the elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs


-- Problem 15
-- Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli [] i = []
repli (x:xs) i = (take i $ repeat x) ++ repli xs i

-- Problem 16
-- Drop every the nth element from the list
dropEvery :: [a] -> Int -> [a]
dropEvery [] i = []
dropEvery xs i = dropEv xs i i
            where 
                dropEv [] _ _ = []
                dropEv (x:xs) i n = if i == 1
                                    then dropEv xs n n
                                    else x : dropEv xs (i-1) n

-- Problem 17
-- Split a list into two parts. The length of the first is given.
split :: [a] -> Int -> ([a], [a])
split [] i = ([], [])
split xs i = split' xs i ([],[])
    where
        split' [] i (as, bs) = (as, bs)
        split' (x:xs) i (as, bs) = if i <= 1
                                     then (as++[x], xs)
                                     else split' xs (i-1) (as++[x], bs)

                     
-- Problem 18
-- Extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) s e = if s <= 1 && e > 1
                   then x : slice xs (s-1) (e-1)
                   else slice xs (s-1) (e-1)




-- Problem 19
-- Rotate a list N places to the left
rotate :: [a] -> Int -> [a]
rotate [] i = []
rotate xs i =
        let (s, e) = split xs i in
            e ++ s


-- Problem 20
-- Remove the K'th element from a list
remove_at :: [a] -> Int -> [a]
remove_at [] i = []
remove_at (x:xs) i = if i == 1 then xs else x : (remove_at xs (i-1))











