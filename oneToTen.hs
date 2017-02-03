import System.IO


-- https://wiki.haskell.org/99_questions/1_to_10
-- https://wiki.haskell.org/99_questions/Solutions 

-- Problem 1
main :: IO ()
main = do
    -- putStrLn $ show $ myLast [1,2,3,4,5]
    -- putStrLn $ show $ butMyLast [1,2,3,4,5]
    -- putStrLn $ show $ elementAt [1,2,3,4,5] 4
    -- putStrLn $ show $ myLength [1,2,3,4]
    -- putStrLn $ show $ myReverse [1,2,3,4]
    -- putStrLn $ show $ isPalindrome [1,2,3,4,3,2]
    -- putStrLn $ show $ isPalindrome "madamimadam"
    -- putStrLn $ show $ flatten (List [Elem 2, Elem 3, (List [Elem 4, Elem 5, Elem 6])])
    -- putStrLn $ show $ compress "aaaabccaadeeee"
    -- putStrLn $ show $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
    putStrLn $ show $ encode "aaaabccaadeeee"
    

myLast :: [a] -> a
myLast [] = error "Error: empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

-- Problem 2

butMyLast :: [a] -> a
butMyLast [] = error "Error: empty list"
butMyLast [x] = error "Error: only one element in list"
butMyLast [x,y] = x
butMyLast (x:xs) = butMyLast xs


-- Problem 3
elementAt :: [a] -> Int -> a
elementAt _ x
    | x <= 0 = error "Index must be greater than zero"
elementAt [] _ = error "Element exceeds list size"
elementAt [x] n = if n == 1 then x else error "Element exceeds list size"
elementAt (x:xs) n = if n == 1 then x else elementAt xs (n-1)

-- Problem 4

myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs

-- Problem 5

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs = foldl (\x y -> x && y) True $ zipWith (==) xs $ reverse xs

-- Problem 7
-- Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

flatten :: (NestedList a) -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))
flatten (List []) = []

-- Problem 8
-- Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress xs = foldr (\x acc -> if x == (head acc) then acc else x:acc) [last xs] xs

-- Problem 9
-- Pack consecutive duplicates of list elements inro sublists.
-- If a list contains repeated elements, they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack xs = foldr (func) [] xs
    where func x acc = if (length acc) == 0
                       then
                            [x]:acc
                       else
                            if (head $ head acc) == x
                            then
                                (x : (head acc)) : (tail acc)
                            else
                                [x] : acc 

-- pack :: (Eq a) => [a] -> [[a]]
-- pack [] = []
-- pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- pack :: (Eq a) => [a] -> [[a]]
-- pack = foldr func []
--     where func x []     = [[x]]
--           func x (y:xs) =
--               if x == (head y) then ((x:y):xs) else ([x]:y:xs)


-- Problem 10
-- Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
-- encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = foldr (\x acc -> ((length x), (head x)) : acc) [] (pack xs)

-- encode xs = (enc . pack) xs
--     where enc = foldr (\x acc -> (length x, head x) : acc) []

-- encode [] = []
-- encode (x:xs) = (length $ x : takeWhile (==x) xs, x)
--                  : encode (dropWhile (==x) xs)














