{-# LANGUAGE BangPatterns #-}

module Day3 where

import System.IO


-- readLine :: Handle -> [Line] -> IO [Line]
readLine :: Handle -> [Line] -> IO [Line]
readLine handle accum = do
  isEnd <- hIsEOF handle
  case isEnd of
  	True -> return $ accum
  	False -> do
  	  s <- hGetLine handle
  	  -- let l = stringToLine s
  	  let splStr = split s ',' []
  	  let l = map strToDir splStr
  	  readLine handle (accum ++ [l])

strToDir :: String -> (Char, Int)
strToDir "" = error "Empty string"
strToDir (x:xs) = (x, ((read xs)::Int))

split :: String -> Char -> [String] -> [String]
split "" _ a = a
split !a !b !c = split rems b (c <> [(takeWhile (\x -> x /= b) a)])   
  where r = dropWhile (\x -> x /= b) a
        rems = if (length r > 0) then (tail r) else ""

main :: IO ()
main = do
  putStrLn "Starting Day 3" 
  dataHandle <- openFile "./data/day3.txt" ReadMode
  coords <- readLine dataHandle []
  print $ coords
  hClose dataHandle
  -- let l1 = [('R',75),('D',30),('R',83),('U',83),('L',12),('D',49),('R',71),('U',7),('L',72)]
  -- let l1 = [('R',8),('U',5),('L',5),('D',3)]
  -- let l2 = [('U', 7),('R', 6),('D', 4),('L', 4)]
  let l1 = (!!) coords 0
  let l2 = (!!) coords 1
  let lcomp1 = getAllCoords l1 [(0,0)]
  let lcomp2 = getAllCoords l2 [(0,0)]
  let ints = filter (\x -> (elem x lcomp1) && (x /= (0,0))) lcomp2
  print $ shortestPythagorean ints 100000000000
  -- l1 = [('R',10),('D',10),('R',5),('U',10),('L',5)]
  -- l1 = [('R',10)]
  -- print $ drawLine (0, 0) ('R', 10)
  -- print $ drawLine (0, 0) ('L', 10)
  -- print $ drawLine (0, 0) ('U', 10)
  -- print $ drawLine (0, 0) ('D', 10)
  putStrLn "Ending Day 3"


drawLine :: (Int, Int) -> (Char, Int) -> [(Int, Int)]
drawLine (x, y) (command, dist)
  | command == 'R' = drawRight (x, y) dist
  | command == 'L' = drawLeft (x, y) dist
  | command == 'U' = drawUp (x, y) dist
  | command == 'D' = drawDown (x, y) dist
  | otherwise = error "Unkown Command"

drawRight :: (Int, Int) -> Int -> [(Int, Int)]
drawRight (x, y) dist = go 1 (dist + 1) [(x,y)]
  where go !i !re !accum = if (i == re) then (accum) else (go (i + 1) dist (accum <> [((x + i), y)]))  

drawLeft :: (Int, Int) -> Int -> [(Int, Int)]
drawLeft (x, y) dist = go 1 (dist + 1) [(x,y)]
  where go !i !re !accum = if (i == re) then (accum) else (go (i + 1) dist (accum <> [((x - i), y)]))

drawUp :: (Int, Int) -> Int -> [(Int, Int)]
drawUp (x, y) dist = go 1 (dist + 1) [(x,y)]
  where go !i !re !accum = if (i == re) then (accum) else (go (i + 1) dist (accum <> [(x, (y + i))]))

drawDown :: (Int, Int) -> Int -> [(Int, Int)]
drawDown (x, y) dist = go 1 (dist + 1) [(x,y)]
  where go !i !re !accum = if (i == re) then (accum) else (go (i + 1) dist (accum <> [(x, (y - i))]))  

getAllCoords :: [(Char, Int)] -> [(Int, Int)] -> [(Int, Int)]
getAllCoords [] !accum = accum
getAllCoords !(x:xs) !accum = getAllCoords xs (accum <> (drawLine (last accum) x))

-- grid :: Int -> Int -> a -> [[a]]
-- grid x y = replicate y . replicate x
shortestPythagorean :: [(Int, Int)] -> Int -> Int 
shortestPythagorean [] a = a 
shortestPythagorean !(x:xs) !acc = shortestPythagorean xs (if (dist < acc) then dist else acc) 
  where dist = ((abs $ fst x) + (abs $ snd x))

type Direction = (Char, Int)
type Line = [(Char, Int)]

-- data Grid = Grid { xy :: [[Int]] } deriving (Eq)

-- gridCollection :: [[Int]] -> String
-- gridCollection g = unlines $ map (\x -> rowToStr x "") g 

-- rowToStr :: [Int] -> String -> String
-- rowToStr [] a = a
-- rowToStr !(s:xs) !a = rowToStr xs (a <> ((show s) <> " "))
{-
drawLine :: (Int, Int) -> (Char, Int) -> [[Int]] -> [[Int]]  
drawLine (x, y) (c, z) a
  | 
-}

{-
Make all coordinates don't make your life hard
-}
-- drawRight :: 

{-
data Evaluator = Evaluator 
  { line :: [Line]
  , maxX :: Int
  , minX :: Int 
  , maxY :: Int
  , minY :: Int
  , curX :: Int
  , curY :: Int
  } deriving (Eq, Show)
-}