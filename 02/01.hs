import Data.Char qualified as Char

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let idPairs = wordswhen contents ','
      ranges = map ((\[a, b] -> [read a :: Int .. (read b)]) . (`wordswhen` '-')) idPairs
  print ranges

wordswhen :: String -> Char -> [String]
wordswhen s sep = case dropWhile (== sep) s of
  "" -> []
  s' -> w : wordswhen s'' sep
    where
      (w, s'') = break (== sep) s'