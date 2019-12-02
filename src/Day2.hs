{-# LANGUAGE BangPatterns #-}

module Day2 (main) where


import System.IO

type InputDataHandle = Handle
type OutputDataHandle = Handle

type OpCode = Int
type Position = Int
type DataSet = (Position, [Int])


readUntilComma :: InputDataHandle -> IO (Maybe String)
readUntilComma handle = go []
  where go !accum = do
         isEnd <- hIsEOF handle
         case isEnd of
           True -> return $ Just accum
           False -> do
            c <- hGetChar handle
            if (c /= ',')  then (go (accum ++ [c])) else (return $ Just accum)

readOpCode :: Position -> [Int] -> Int
readOpCode pos arr = (!!) arr pos

processOpCode :: OpCode -> DataSet -> DataSet
processOpCode opcode ds 
  | opcode == 1 = processOpCode1 ds
  | opcode == 2 = processOpCode2 ds
  | otherwise = error "Invalid Op Code"

processOpCode1 :: DataSet -> DataSet
processOpCode1 (pos, ls) = (pos + 4, (ls1 <> [o1 + o2] <> ls2))
  where o1           = (!!) ls ((!!) ls (pos + 1))
        o2           = (!!) ls ((!!) ls (pos + 2))
        targetPos    = (!!) ls (pos + 3)
        (ls1, _:ls2) = splitAt targetPos ls

processOpCode2 :: DataSet -> DataSet
processOpCode2 (pos, ls) = (pos + 4, (ls1 <> [o1 * o2] <> ls2))
  where o1           = (!!) ls ((!!) ls (pos + 1))
        o2           = (!!) ls ((!!) ls (pos + 2))
        targetPos    = (!!) ls (pos + 3)
        (ls1, _:ls2) = splitAt targetPos ls

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

getStartData :: IO ([Int])
getStartData = do
  solutionHandle1 <- openFile "./solutions/day2.txt" ReadMode 
  vals <- go solutionHandle1 []
  hClose solutionHandle1
  return $ vals
  where go handle !accum = do
         isEnd <- hIsEOF handle
         case isEnd of
          True -> return $ accum
          False -> do
            c <- readUntilComma handle
            case c of 
              Nothing -> return $ accum
              Just v -> do
               let readInt = (read v)::Int
               go handle (accum  ++ [readInt])

main :: IO ()
main = do
  putStrLn "Starting Day 2"
  copyFile
  vals <- getStartData
  let xy = [(x, y) | x <- [0..99], y <- [0..99]]
  val <- solver xy vals
  print $ val
  putStrLn "Day 2 Over"

controller :: DataSet -> IO [Int]
controller !(pos, ls) = do
  -- print $ (pos, ls)
  let opCode = readOpCode pos ls
  case opCode of
    99 -> return $ ls
    _ -> do
     controller (processOpCode opCode (pos, ls))

replaceNum :: [Int] -> Int -> Int -> [Int]
replaceNum ls ind repl = ls1 <> [repl] <> ls2 
  where (ls1, _:ls2) = splitAt ind ls

setVals :: (Int, Int) -> [Int] -> [Int]
setVals !(v1, v2) !ls = l2
  where l1 = replaceNum ls 1 v1
        l2 = replaceNum l1 2 v2

solver :: [(Int, Int)] -> [Int] -> IO (Maybe (Int, Int))
solver [] _ = return Nothing
solver !(x:xs) !ls = do
  let newList = setVals x ls
  newTotal <- controller (0, newList)
  let newVal = (!!) newTotal 0 
  case newVal of
    19690720 -> do
      return $ Just x
    _ -> do
     solver xs ls  
