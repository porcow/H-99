import Data.List
import Control.Monad
import System.IO
import System.IO.Error
import System.Directory
import System.Environment
import System.Random
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

-- 1
myLast :: [c] -> c
myLast [x] = x
myLast (_:xs) = myLast xs
myLast' = head . reverse

-- 3
elementAt :: Num n => [a] -> n -> a
elementAt xs 1 = head xs
elementAt (_:xs) m = elementAt xs (m-1)

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
myLength' = foldl (\acc _ -> acc + 1) 0
myLength'' = sum . map (\x -> 1)

-- 5
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []
myReverse' = foldl (flip (:)) []

-- 6
myPalindrome :: (Eq a) => [a] -> Bool
myPalindrome x
  | reverseX == x = True
  | otherwise      = False
  where reverseX = reverse x

-- 7
data NestedList a = Elem a | List [NestedList a] deriving (Show)
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

flatten = foldr1 (++)

-- 8
myCompress :: (Eq a, Ord a) => [a] -> [a]
myCompress [] = []
myCompress xs = foldr (\x acc -> if x == (head acc) then acc else x:acc) [last xs] $ sort xs


-- 9
myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack (x:xs) = (x:ys) : myPack zs
                where (ys, zs) = span ((==) x) xs
myPack' :: (Eq a) => [a] -> [[a]]
myPack' [] = []
myPack' (x:xs) = (takeWhile (== x) xs) : myPack' (dropWhile (== x) xs)


-- 10
myEncode :: (Eq a) => [a] -> [(Int, a)]
myEncode x = zip (map length packedX) (map head packedX)
             where packedX = myPack x

myEncode' :: (Eq a) => [a] -> [(Int, a)]
myEncode' [] = []
myEncode' (x:xs) = (length $ x : takeWhile (== x) xs, x)
                   : myEncode' (dropWhile (== x) xs)

-- 11
data Count a = Single a | Multiple Int a deriving (Show)
encodeModified :: (Eq a) => [a] -> [Count a]
encodeModified [] = []
encodeModified x = map foo (myEncode x)
                   where
                     foo (m, n)
                       | m > 1    = Multiple m n
                       | otherwise = Single n

-- 12
decodeModified :: (Eq a) => [Count a] -> [a]
decodeModified [] = []
decodeModified xs = concatMap foo xs
                    where foo (Single x) = [x]
                          foo (Multiple i y) = replicate i y
                          
-- 13
encodeDirect :: (Eq a) => [a] -> [Count a]
encodeDirect [] = []
encodeDirect (x:xs)
  | count == 1    = (Single x) : encodeDirect xs
  | otherwise     = (Multiple count x) : encodeDirect rest
  where
    (matched, rest) = span (== x) xs
    count = 1 + length matched

-- 14
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs

-- 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli _ 0 = []
repli x i = concatMap (replicate i) x

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery x n = init (take n x) ++ dropEvery (drop n x) n

-- 17
mySplit :: [a] -> Int -> ([a], [a])
mySplit [] _ = ([], [])
mySplit xs 0 = ([], xs)
mySplit (x:xs) n = (x:ys, zs)
                 where (ys, zs) = mySplit xs (n-1)

-- 18
mySlice :: [a] -> Int -> Int -> [a]
mySlice [] _ _ = []
mySlice xs n m = drop n $ take m xs

-- 19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate x 0 = x
rotate (y@(x:xs)) n = if n >= 0 then rotate (xs ++ [x]) (n-1)
                    else rotate y (length y + n)

rotate' :: [a] -> Int -> [a]
rotate' xs n  = take len . drop (n `mod` len) . cycle $ xs
                where len = length xs

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n x = ((x !! n), (take n x) ++ (drop (n+1) x))

removeAt' :: Int -> [a] -> (Maybe a, [a])
removeAt' _ [] = (Nothing, [])
removeAt' 0 (x:xs) = (Just x, xs)
removeAt' n (x:xs) = (y, x:r)
                     where (y, r) = removeAt' (n-1) xs

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n-1) xs) ++ [x] ++ (drop (n-1) xs)

insertAt' :: a -> [a] -> Int -> [a]
insertAt' e l n = fst $ foldl helper ([], 1) l
                  where helper (acc, i) x = if i == n then (acc ++ [e, x], i+1) else (acc ++ [x], i+1)
                        
-- 22
myRange :: Int -> Int -> [Int]
myRange x y
  | x == y    = [y]
  | x < y     = x : myRange (x+1) y
  | x > y     = x : myRange (x-1) y

myRange' :: (Ord a, Enum a) => a -> a -> [a]
myRange' m n
  | m == n    = [n]
  | otherwise = m : myRange' ((if m < n then succ else pred ) m) n

-- 23




{-
main = do
  gen <- getStdGen  
  askForNumber gen  
            
askForNumber :: StdGen -> IO ()  
askForNumber gen = do  
  let (randNumber, newGen) = randomR (1,100) gen :: (Int, StdGen)  
  putStrLn "Which number in the range from 1 to 10 am I thinking of? "  
  numberString <- getLine  
  let number = read numberString  
  guess number randNumber


guess :: Int -> Int -> IO ()
guess c n
  | c == n = do
    putStrLn $ "Bingo! it's " ++ show n
    return ()
  | c < n = do
    putStrLn $ "Greater than it!"
    c1 <- getLine
    guess (read c1) n
  | otherwise  = do
    putStrLn $ "Less than it!"
    c2 <- getLine
    guess (read c2) n
-}
