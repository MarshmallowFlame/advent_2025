-- Correct output: 45283684555
main :: IO ()
main = do
  contents <- readFile "input.txt"
  let idPairs = wordswhen contents ','
      integers = idPairs >>= ((\[a, b] -> [read a :: Int .. (read b)]) . (`wordswhen` '-'))
      invalids = [s | s <- integers, isInvalid $ show s]

  print $ sum invalids

wordswhen :: String -> Char -> [String]
wordswhen s sep = case dropWhile (== sep) s of
  "" -> []
  s' -> w : wordswhen s'' sep
    where
      (w, s'') = break (== sep) s'

splitAtX :: String -> Int -> [String]
splitAtX [] _ = []
splitAtX s x =
  let (l, r) = splitAt x s
   in l : splitAtX r x

isInvalid :: String -> Bool
isInvalid s = any allAreEqual splits
  where
    factors = getFactors $ length s
    splits = map (splitAtX s) factors

allAreEqual :: [String] -> Bool
allAreEqual [] = False
allAreEqual (first : rest) =
  all (first ==) rest

getFactors :: Int -> [Int]
getFactors i = [x | x <- [1 .. i `div` 2 + 1], i `mod` x == 0 && x /= i]