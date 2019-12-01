{-# LANGUAGE BangPatterns #-}

module Day1 (main) where


import System.IO

type InputDataHandle = Handle
type OutputDataHandle = Handle

calculateFuel :: (RealFrac a, Integral b) => a
                                          -> b
calculateFuel mass = (floor $  (/) mass 3) - 2 

recursiveFuel :: (RealFrac a, Integral b) => a 
                                          -> b 
                                          -> b
recursiveFuel fuelmass !accum
  | calculatedFuel > 0 = recursiveFuel (fromIntegral calculatedFuel) ((+) accum  calculatedFuel)
  | otherwise          = accum 
  where calculatedFuel = calculateFuel fuelmass

part1 :: InputDataHandle 
       -> OutputDataHandle
       -> Int 
       -> IO ()
-- | Add some Strictness to the Accumulator so we're not throwing thunks all over (even though theres only 100 inputs)
part1 dat sol !accum = do
	isEnd <- hIsEOF dat
	case isEnd of
	  True -> print $ "Part 1 Solution: " <>  (show accum)
	  False -> do
	  	line <- hGetLine dat
	  	let num = (read line)::Int
	  	let fuel = calculateFuel (fromIntegral num)
	  	hPutStrLn sol (show fuel)
	  	part1 dat sol (fuel + accum)

part2 :: InputDataHandle 
       -> OutputDataHandle
       -> Int 
       -> IO ()
-- | Add some Strictness to the Accumulator so we're not throwing thunks all over (even though theres only 100 inputs)
part2 dat sol !accum = do
	isEnd <- hIsEOF dat
	case isEnd of
	  True -> print $ "Part 2 Solution: " <> (show accum)
	  False -> do
	  	line <- hGetLine dat
	  	let num = (read line)::Int
	  	let totalFuel = recursiveFuel (fromIntegral num) 0
	  	hPutStrLn sol (show totalFuel)
	  	part2 dat sol (totalFuel + accum)

main :: IO ()
main = do
  putStrLn "Starting Day 1"
  -- | Acquire Handle to data
  dataHandle <- openFile "./data/day1.txt" ReadMode
  -- | Acquire Handle to Ouput of Solution 1
  solutionHandle1 <- openFile "./solutions/day1.txt" WriteMode
  -- | Acquire Handle to Output of SOlution 2
  solutionHandle2 <- openFile "./solutions/day1-part2.txt" WriteMode
  part1 dataHandle solutionHandle1 0
  -- | Reset the handle to the zero position of the file as it will be EOF
  hSeek dataHandle AbsoluteSeek 0
  part2 dataHandle solutionHandle2 0
  -- | Close all handles... 
  hClose dataHandle
  hClose solutionHandle1
  hClose solutionHandle2
  putStrLn "Day 1 Over"



