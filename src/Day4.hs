{-# LANGUAGE BangPatterns #-}
module Day4 where

import Data.List
import Data.Maybe

breakNumberToList :: Int -> [Int]
breakNumberToList !num = go num []
  where go !n !accum = if (n > 9) then (go (divNum (realToFrac n)) ([remNum n] <> accum)) else ([n] <> accum) 

divNum :: (RealFrac a) => a -> Int
divNum a = floor (a/10)

remNum :: Int -> Int
remNum a = a `rem` 10 

checkForSameTwo :: [Int] -> Int -> Bool
checkForSameTwo !xs !lim
  | lim >= 5 = False
  | (!!) xs lim == (!!) xs (lim + 1) = True  
  | otherwise = checkForSameTwo xs (lim + 1)  

checkForIncreasing :: [Int] -> Int -> Bool
checkForIncreasing !xs !lim
  | lim >= 5 = True
  | (!!) xs lim <= (!!) xs (lim + 1) = checkForIncreasing xs (lim + 1) 
  | otherwise = False

findRepeatingSequences :: [Int] -> Int -> Int -> (Int, Int, Int)
findRepeatingSequences !xs !start !ind
  | ind >= 5 = ((!!) xs (start), ind - start + 1, ind)
  | (!!) xs (start) == (!!) xs (ind + 1) = findRepeatingSequences xs start (ind + 1)
  | otherwise = ((!!) xs (start), ind - start + 1, ind) 

getUniqueSequences :: [Int] -> Int -> [(Int, Int)] -> [(Int, Int)]
getUniqueSequences !xs !ind !accum
  | ind >= 5 = accum 
  | otherwise = getUniqueSequences xs (t3 + 1) reps
  where reps = if (t2 >= 2) then (accum <> [(t1, t2)]) else (accum)
        (t1, t2, t3) = findRepeatingSequences xs ind ind

main :: IO ()
main = do 
  putStrLn "Starting Day 4"
  part1
  part2
  putStrLn "Done Day 4"

part1 :: IO ()
part1 = do
  let valNums = [x | x <- [402328..864247], ((checkForSameTwo (breakNumberToList x) 0) == True) &&  ((checkForIncreasing (breakNumberToList x) 0) == True)]
  print $ length valNums


part2 :: IO ()
part2 = do
  let valNums = 
  	[x | 
  	 x <- [402328..864247] 
  	 , ((checkForSameTwo (breakNumberToList x) 0) == True)
  	 , ((checkForIncreasing (breakNumberToList x) 0) == True)
  	]
  let valNums2 = [((getUniqueSequences (breakNumberToList y) 0 []), y)| 
                  y <- valNums
                  , (length $ getUniqueSequences (breakNumberToList y) 0 []) >= 2 || (snd $ head (getUniqueSequences (breakNumberToList y) 0 [])) == 2
                 ]
  let f = filter (\x -> if ((length (fst x) == 2) && (snd $ head $ fst  x) == 3 && (snd $ last $ fst x) == 3) then (False) else (True)) valNums2  

  print $ length f
