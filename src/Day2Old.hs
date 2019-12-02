{-# LANGUAGE BangPatterns #-}

module Day2Old (main) where


import System.IO

type InputDataHandle = Handle
type OutputDataHandle = Handle

{-
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
-}

type OpCode = Int

readUntilComma :: InputDataHandle -> IO String
readUntilComma handle = go []
  where go !accum = do
         isEnd <- hIsEOF handle
         case isEnd of
           True -> return "E"
           False -> do
            c <- hGetChar handle
            if (c /= ',')  then (go (accum ++ [c])) else (return accum)

interpreter :: OpCode -> IO ()
interpreter opcode
  | opcode == 1 = print $ "GREAT" -- readUntilComma 
  | otherwise = error "That is an invalid opcode"



readOpCode :: Handle -> IO (Maybe Int)
readOpCode handle = do
  val <- readUntilComma handle
  case val of
    "E" -> return $ Nothing
    _ -> do
      print $ val
      return $ Just ((read val)::Int)
{-  
processOpCode1 :: InputDataHandle -> IO ()
processOpCode1 handle = do
  v1 <- readUntilComma handle
  v2 <- readUntilComma handle
  v3 <- readUntilComma handle
  let m = catMaybes [v1,v2,v3]
  case (length m == 3) of
    False -> error "Unexpected amount of operands" 
    True -> do
      print $ "DOne"
-}

-- | Copy Original Data into Solution File.
copyFile :: IO ()
copyFile = do
  -- | Copying Files
  dataHandle <- openFile "./data/day2.txt" ReadMode
  -- | Acquire Handle to Ouput of Solution 1
  solutionHandle1 <- openFile "./solutions/day2.txt" WriteMode 
  -- | Recurisvely Add all characters to 
  go dataHandle solutionHandle1 
  hClose dataHandle
  hClose solutionHandle1
  where go handle1 handle2 = do
         isEnd <- hIsEOF handle1
         case isEnd of
          True -> print "Done Copying File Data"
          False -> do
            c <- hGetChar handle1
            hPutChar handle2 c
            go handle1 handle2


main :: IO ()
main = do
  putStrLn "Starting Day 2"
  -- | Copy File so we dont write over test data
  copyFile
  -- | Acquire Handle to data
  -- |dataHandle <- openFile "./data/day2.txt" ReadMode
  -- | Acquire Handle to Ouput of Solution 1
  solutionHandle1 <- openFile "./solutions/day2.txt" ReadWriteMode
  readOpCode solutionHandle1
  -- part1 dataHandle solutionHandle1 0
  -- | Reset the handle to the zero position of the file as it will be EOF
  -- hSeek dataHandle AbsoluteSeek 0
  -- | Close all handles... 
  -- | hClose dataHandle
  hClose solutionHandle1
  putStrLn "Day 2 Over"
