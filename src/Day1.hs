{-# LANGUAGE BangPatterns #-}

module Day1 (main) where


import System.IO

type InputDataHandle = Handle
type OutputDataHandle = Handle

calculateFuel :: (RealFrac a, Integral b) => a -> b
calculateFuel mass = (floor $  (/) mass 3) - 2 

readAndWrite :: InputDataHandle 
             -> OutputDataHandle
             -> Int 
             -> IO ()
readAndWrite dat sol !accum = do
	isEnd <- hIsEOF dat
	case isEnd of
	  True -> print accum
	  False -> do
	  	line <- hGetLine dat
	  	let num = (read line)::Int
	  	let fuel = calculateFuel (fromIntegral num)
	  	hPutStrLn sol (show fuel)
	  	readAndWrite dat sol (fuel + accum)

main :: IO ()
main = do
  putStrLn "Starting Day 1"
  dataHandle <- openFile "./data/day1.txt" ReadMode
  solutionHandle <- openFile "./solutions/day1.txt" WriteMode
  readAndWrite dataHandle solutionHandle 0
  hClose dataHandle
  hClose solutionHandle
  putStrLn "Day 1 Over"



