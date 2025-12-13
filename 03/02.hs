-- Correct output: 172601598658203

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let l = lines contents
      code = sum $ map (read . findBiggest) l
  print code

findXdigit :: String -> Int -> Char
findXdigit s x = maximum $ removeLastX x s

findBiggest :: String -> String
findBiggest [] = []
findBiggest l =
  let xs = [11, 10 .. 0]
      bests = scanl f (l, ' ') xs
      digits = drop 1 $ map snd bests
      seqs = drop 1 $ map fst bests
   in digits

removeLastX :: Int -> [a] -> [a]
removeLastX x l = take (length l - x) l

f :: (String, Char) -> Int -> (String, Char)
f (s, lastC) x =
  let bestC = findXdigit s x
      newS = drop 1 $ dropWhile (/= bestC) s
   in (newS, bestC)