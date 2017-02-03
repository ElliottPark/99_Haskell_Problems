
import System.Random
import qualified Data.Map as M


main = do
    -- putStrLn $ show $ insertAt 'c' "Hello" 3
    -- putStrLn $ show $ range 4 7
    -- putStrLn  $ rnd_select "hellothere" 3
    -- g <- rnd_select "hello world" 6 --rnd_select "hello there" 4
    -- g <- rnd_select "hello world" 6
    -- g <- lotto 6 46
    -- g <- rnd_permu [1,2,3,4,5,6,7]
    -- print g
    -- putStrLn $ show $ combinations 2 [1,2,3]
    -- putStrLn $ show $ group [1,2] ["aldo","bob","carla"]
    -- putStrLn $ show $ lsort ["hi", "a", "hello", "wow", "greetings"]
    -- putStrLn $ show $ minLen ["hi", "hello", "greetings"]
    putStrLn $ show $ sortFreq ["hi", "a", "hi", "hello", "greetings", "greetings", "wow", "greetings"]
    -- putStrLn $ show $ sortC [("hi", 2), ("hello", 1), ("wow", 3)]


-- Problem 21
-- Insert an element at a given position into a list

insertAt :: a -> [a] -> Int -> [a]
insertAt c [] 0 = [c]
insertAt _ [] _ = []
insertAt c (x:xs) i = if i == 1
                      then c:x:xs
                      else x : insertAt c xs (i-1)

-- Problem 22
-- Create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range x y = [x..y]

-- Problem 23
-- Extract a given number of randomly selected elements from a list

rnd_select :: [a] -> Int -> IO([a])
rnd_select _ 0 = return []
rnd_select xs n = do
  r  <- randomRIO (0,n-1)
  rs <- rnd_select xs (n-1)
  return ((xs !! r):rs) 

-- randomList :: Int -> IO([Int])
-- randomList 0 = return []
-- randomList n = do
--   r  <- randomRIO (0,n)
--   rs <- randomList (n-1)
--   return (r:rs) 


-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M
lotto :: Int -> Int -> IO([Int])
lotto 0 m = return []
lotto n m = do
    r <- randomRIO(1,m)
    rs <- lotto (n-1) m
    return (r:rs)


-- Problem 25
-- Generate a random permutation of the elements of a list
rnd_permu :: [a] -> IO([a])
rnd_permu [] = return []
rnd_permu (x:xs) = do
            rand <- randomRIO (0, (length xs))
            rs <- rnd_permu xs
            return $ let (ys, zs) = splitAt rand rs in
                ys ++ (x:zs)


-- Problem 26
-- Generate the combinations of K distinct objects from the N elements of a list


-- Problem 27
-- Group the elements of a set into disjoint subsets
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]
 
group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs =
    [ g:gs | (g,rs) <- combination n xs
           ,  gs    <- group ns rs ]


-- Problem 28
-- Sort a list according to the length of sublists
lsort :: (Eq a) => [[a]] -> [[a]]
lsort [] = []
lsort xs = mn : lsort (remove mn xs)
      where mn = minLen xs
            remove y (z:zs) = if y == z
                              then zs
                              else z : (remove y zs)


minLen :: [[a]] -> [a]
minLen [] = []
minLen (x:xs) = mnLn x xs
        where mnLn mn [] = mn
              mnLn mn (x:xs) = if (length x) < (length mn)
                               then mnLn x xs
                               else mnLn mn xs

-- type Dict = M.Map String String

-- foldr (\(a, b) acc -> a:acc) [] $
-- Problem 28b
-- Sort the list by string frequency
sortFreq :: (Ord a) => [a] -> [a] -- [(a, Int)]
sortFreq [] = []
sortFreq xs = foldr (\(a, b) acc -> a:acc) [] $ sortC $ M.toList $ foldl (\acc x -> insertM acc x) M.empty xs
      where insertM ac z = M.insert z ((M.findWithDefault 0 z ac)+1) ac

sortC :: (Ord a) => [(b, a)] -> [(b, a)]
sortC [] = []
sortC xs = mn : sortC (remove mn xs)
  where mn = min' xs
        remove (y1, y2) ((z1, z2):zs) = if y2 == z2
                                        then zs
                                        else (z1, z2) : (remove (y1, y2) zs)


min' :: (Ord a) => [(b, a)] -> (b,a)
min' [] = error "can't get min of nothing"
min' (x:xs) = mnLn x xs
        where mnLn mn [] = mn
              mnLn (m1, m2) ((x1, x2):xs) = if x2 > m2
                               then mnLn (x1,x2) xs
                               else mnLn (m1,m2) xs









