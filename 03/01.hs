-- Correct output: 17383

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let l = lines contents
  print $ sum $ map findBiggest l

findBiggest :: String -> Int
findBiggest [] = 0
findBiggest l =
  let firstDigit = maximum $ init l
      secondDigit = maximum $ drop 1 $ dropWhile (/= firstDigit) l
   in read [firstDigit, secondDigit]