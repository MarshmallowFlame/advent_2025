-- Correct answer: 635
main :: IO ()
main = do
  contents <- readFile "input.txt"
  let l = lines contents
      (freshIDs, _ : ids') = break (== "") l
      ids = map read ids'
      freshRanges = map ((\[x, y] -> (read x, read y)) . (`wordswhen` '-')) freshIDs
      nrFreshItems = length $ filter (`inAnyRange` freshRanges) ids
  print nrFreshItems

wordswhen :: String -> Char -> [String]
wordswhen s sep = case dropWhile (== sep) s of
  "" -> []
  s' -> w : wordswhen s'' sep
    where
      (w, s'') = break (== sep) s'

inRange :: Int -> (Int, Int) -> Bool
inRange x (start, end) = x >= start && x <= end

inAnyRange :: Int -> [(Int, Int)] -> Bool
inAnyRange x = any (inRange x)

inRangeX :: Int -> ((Int, Int) -> Bool)
inRangeX = inRange