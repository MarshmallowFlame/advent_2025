data S a = S
  { matched :: Bool,
    checkAgainst :: [a],
    buffer :: [a],
    str :: [a]
  }

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let idPairs = wordswhen contents ','
      integers = concatMap ((\[a, b] -> [read a :: Int .. (read b)]) . (`wordswhen` '-')) idPairs
      strings = map show integers
      invalids = filter isInvalid strings
      invalidsInts = map (read :: String -> Int) invalids

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

splitAtX :: String -> Int -> [String]
splitAtX [] _ = []
splitAtX s x =
  let (l, r) = splitAt x s
   in l : splitAtX r x

isInvalid :: String -> Bool
isInvalid s = do
  let factors = getFactors $ length s
      splits = map (splitAtX s) factors
      allEqual = filter allAreEqual splits

  allEqual /= []

allAreEqual :: [String] -> Bool
allAreEqual l = do
  let first = head l
  all (first ==) l

getFactors :: Int -> [Int]
getFactors i = do
  -- We wanna test all values up to half of i.
  let h = i `div` 2 + 1
      toTest = [1 .. h]
      (_, divs) = foldl p (i, []) toTest
      -- We don't wanna include i itself in its divisors.
      divs_final = filter (/= i) divs
  divs_final

p :: (Int, [Int]) -> Int -> (Int, [Int])
-- Input is the integer we are finding divisors for, and the divisors found so far. Output is new state.
p (s, divs) cand =
  -- If s is divisible by the candidate, add it to divs, else leave divs untouched.
  if s `mod` cand == 0 then (s, cand : divs) else (s, divs)
