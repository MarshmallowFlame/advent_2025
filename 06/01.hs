-- Correct answer: 6725216329103

import Data.List (transpose)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (ops : nrs) = reverse $ map (`wordswhen` ' ') $ lines contents
      newNrs = map (map read) (transpose nrs)
      opsNrs = zip newNrs ops
      res = map (uncurry calculate) opsNrs

  print $ sum res

wordswhen :: String -> Char -> [String]
wordswhen s sep = case dropWhile (== sep) s of
  "" -> []
  s' -> w : wordswhen s'' sep
    where
      (w, s'') = break (== sep) s'

getOperator :: String -> (Int -> Int -> Int)
getOperator "*" = (*)
getOperator "+" = (+)

calculate :: [Int] -> String -> Int
calculate [x] op = x
calculate (first : rest) op = getOperator op first (calculate rest op)