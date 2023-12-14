import System.Environment (getArgs)
import Data.Char (isAlpha, isUpper, toUpper, ord, chr)

alph :: String
alph = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

shift :: Char -> Int -> Char
shift c n
  | isAlpha c = let base = if isUpper c then 'A' else 'a'
                in chr $ (ord c - ord base + n) `mod` 26 + ord base
  | otherwise = c

vigenere :: String -> String -> String
vigenere key text = go (cycle key) text
  where
    go _ [] = []
    go k (c:cs)
      | isAlpha c = shift c (fromEnum (head k) - fromEnum 'a') : go (tail k) cs
      | otherwise = c : go k cs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mode, key, filepath] -> do
      content <- readFile filepath
      let result = case mode of
            "e" -> vigenere key content
            "d" -> vigenere (map (\x -> if isAlpha x then toUpper x else x) key) content
            _   -> "Invalid mode. Please use 'e' for encryption or 'd' for decryption."
      putStrLn result
    _ -> putStrLn "Usage: ./vigenere <mode> <key> <filepath>"
