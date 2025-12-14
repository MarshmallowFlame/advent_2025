import Data.List

-- Correct answer: 369761800782619
main :: IO ()
main = do
  contents <- readFile "input.txt"
  let l = lines contents
      (freshIDs, _) = break (== "") l
      freshRanges = map ((\[x, y] -> (read x :: Int, read y :: Int)) . (`wordswhen` '-')) freshIDs
      nrIntervals = map nrInInterval $ mergeThem $ sort freshRanges
  print $ sum nrIntervals

wordswhen :: String -> Char -> [String]
wordswhen s sep = case dropWhile (== sep) s of
  "" -> []
  s' -> w : wordswhen s'' sep
    where
      (w, s'') = break (== sep) s'

nrInInterval :: (Int, Int) -> Int
nrInInterval (start, end) = end - start + 1

mergeThem :: [(Int, Int)] -> [(Int, Int)]
mergeThem [] = []
mergeThem [x] = [x]
mergeThem [x, y] = mergeIntervals x y
mergeThem (first : second : rest) =
  let l = mergeIntervals first second
   in if length l == 1 then mergeThem (l ++ rest) else first : mergeThem (second : rest)

-- Assume: s1 <= s2
mergeIntervals :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
mergeIntervals (s1, e1) (s2, e2) = if s2 <= e1 then [(s1, max e1 e2)] else [(s1, e1), (s2, e2)]