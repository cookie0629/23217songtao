import System.Environment
import Data.Char (chr, ord)
import Control.Monad.State

-- | 定义数据带的类型别名
type DataTape = [Int]

-- | 定义指令带的类型别名
type InstructionTape = String

-- | 定义 Brainf*ck 解释器的状态，包括数据带、数据指针、指令带、指令指针、输入缓冲和输出缓冲
data BFState = BFState { dataTape :: DataTape
                       , dataPtr :: Int
                       , instructionTape :: InstructionTape
                       , instructionPtr :: Int
                       , inputBuffer :: String
                       , outputBuffer :: String
                       } deriving (Show)

-- | 从输入缓冲中读取一个字符，如果需要的话更新输入缓冲
readInput :: State BFState Int
readInput = do
  state <- get
  case inputBuffer state of
    [] -> return 0 -- 如果输入缓冲为空，则返回 0
    (c:cs) -> do
      put $ state { inputBuffer = cs } -- 更新输入缓冲，移除已读取的字符
      return $ ord c -- 返回读取字符的 ASCII 码

-- | 将一个字符写入输出缓冲
writeOutput :: Int -> State BFState ()
writeOutput n = do
  state <- get
  let char = chr n -- 将数字转换为字符
  put $ state { outputBuffer = outputBuffer state ++ [char] } -- 将字符添加到输出缓冲中

-- | 执行一个 Brainf*ck 指令
executeInstruction :: Char -> State BFState ()
executeInstruction '>' = modify (\s -> s { dataPtr = dataPtr s + 1 }) -- 将数据指针向右移动一位
executeInstruction '<' = modify (\s -> s { dataPtr = dataPtr s - 1 }) -- 将数据指针向左移动一位
executeInstruction '+' = modify (\s -> let dt = dataTape s
                                           dp = dataPtr s
                                           (left, _:right) = splitAt dp dt
                                       in s { dataTape = left ++ [(head right) + 1] ++ tail right }) -- 在当前数据单元上增加 1
executeInstruction '-' = modify (\s -> let dt = dataTape s
                                           dp = dataPtr s
                                           (left, _:right) = splitAt dp dt
                                       in s { dataTape = left ++ [(head right) - 1] ++ tail right }) -- 在当前数据单元上减少 1
executeInstruction '.' = do
  state <- get
  let dt = dataTape state
      dp = dataPtr state
      value = dt !! dp
  writeOutput value -- 将当前数据单元的值添加到输出缓冲中
executeInstruction ',' = do
  value <- readInput -- 从输入缓冲中读取一个字符
  modify (\s -> let dt = dataTape s
                    dp = dataPtr s
                    (left, _:right) = splitAt dp dt
                in s { dataTape = left ++ [value] ++ tail right }) -- 将读取的值存储到当前数据单元
executeInstruction '[' = do
  state <- get
  let dt = dataTape state
      dp = dataPtr state
      value = dt !! dp
  if value == 0
    then do
      let it = instructionTape state
          ip = instructionPtr state
          closeBracketIdx = findCloseBracket it ip 1
      modify (\s -> s { instructionPtr = closeBracketIdx }) -- 将指令指针移到对应的 ']' 后面的指令
    else return ()
executeInstruction ']' = do
  state <- get
  let dt = dataTape state
      dp = dataPtr state
      value = dt !! dp
  if value /= 0
    then do
      let it = instructionTape state
          ip = instructionPtr state
          openBracketIdx = findOpenBracket it ip 1
      modify (\s -> s { instructionPtr = openBracketIdx }) -- 将指令指针移到对应的 '[' 后面的指令
    else return ()
executeInstruction _ = return () -- 忽略其他字符

-- | 查找与给定 ']' 对应的 ']' 的索引
findCloseBracket :: InstructionTape -> Int -> Int -> Int
findCloseBracket tape idx count
  | count == 0 = idx
  | otherwise =
    let nextIdx = idx + 1
        nextChar = tape !! nextIdx
    in case nextChar of
         '[' -> findCloseBracket tape nextIdx (count + 1)
         ']' -> findCloseBracket tape nextIdx (count - 1)
         _ -> findCloseBracket tape nextIdx count

-- | 查找与给定 '[' 对应的 ']' 的索引
findOpenBracket :: InstructionTape -> Int -> Int -> Int
findOpenBracket tape idx count
  | count == 0 = idx
  | otherwise =
    let prevIdx = idx - 1
        prevChar = tape !! prevIdx
    in case prevChar of
         ']' -> findOpenBracket tape prevIdx (count + 1)
         '[' -> findOpenBracket tape prevIdx (count - 1)
         _ -> findOpenBracket tape prevIdx count

-- | 执行整个 Brainf*ck 程序
executeProgram :: State BFState ()
executeProgram = do
  state <- get
  let it = instructionTape state
      ip = instructionPtr state
  if ip >= length it -- 检查指令指针是否越界
    then return ()
    else do
      let instruction = it !! ip
      executeInstruction instruction -- 执行当前指令
      modify (\s -> s { instructionPtr = ip + 1 }) -- 移动到下一条指令
      executeProgram -- 递归执行剩余指令

-- | 运行 Brainf*ck 解释器
runBFInterpreter :: InstructionTape -> String -> String
runBFInterpreter instructions input =
  let initialState = BFState { dataTape = replicate 30000 0 -- 初始化数据带
                             , dataPtr = 0
                             , instructionTape = instructions
                             , instructionPtr = 0
                             , inputBuffer = input
                             , outputBuffer = ""
                             }
      finalState = execState executeProgram initialState -- 运行程序
  in outputBuffer finalState -- 返回输出缓冲区

