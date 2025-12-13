-- Correct output: 172601598658203

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let l = lines contents
      code = sum $ map (read . findBiggest) l
  print code

findBiggest :: String -> String
findBiggest [] = []
findBiggest l = findDigits l [11, 10 .. 0]
  where
    findDigits _ [] = []
    findDigits s (pos : rest) =
      let c = findXdigit s pos
          newS = drop 1 $ dropWhile (/= c) s
       in c : findDigits newS rest

removeLastX :: Int -> [a] -> [a]
removeLastX x l = take (length l - x) l

findXdigit :: String -> Int -> Char
findXdigit s x = maximum $ removeLastX x s