import Data.ByteString.Short (split)
import Data.Char qualified as Char

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let idPairs = wordswhen contents ','
      ranges = concatMap ((\[a, b] -> [read a :: Int .. (read b)]) . (`wordswhen` '-')) idPairs
      listOfStrings = map show ranges
      invalids = filter isInvalid listOfStrings
      invalidsInts = map (read :: String -> Int) invalids

  -- print $ take 1 $ map splitInHalf listOfStrings
  print $ sum invalidsInts

wordswhen :: String -> Char -> [String]
wordswhen s sep = case dropWhile (== sep) s of
  "" -> []
  s' -> w : wordswhen s'' sep
    where
      (w, s'') = break (== sep) s'

splitInHalf :: String -> (String, String)
splitInHalf s = do
  let half = length s `div` 2
  splitAt half s

isInvalid :: String -> Bool
isInvalid s = do
  let (a, b) = splitInHalf s
  a == b