-- Correct answer: 1569

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let l = lines contents
      hpadded = replicate (length $ head l) '.' : l ++ [replicate (length $ head l) '.']
      padded = map (\s -> '.' : s ++ ['.']) hpadded
      zipped = zipS padded
      deepZipped = map (applyTriple zipS) zipped
      conv2d = concatMap zipThrees deepZipped
      count = length $ filter accessable conv2d

  print count

applyTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
applyTriple f (a, b, c) = (f a, f b, f c)

zipS :: [a] -> [(a, a, a)]
zipS s = zip3 s (tail s) (tail (tail s))

zipThrees :: ([(a, a, a)], [(a, a, a)], [(a, a, a)]) -> [((a, a, a), (a, a, a), (a, a, a))]
zipThrees (a, b, c) = zip3 a b c

accessable :: ((Char, Char, Char), (Char, Char, Char), (Char, Char, Char)) -> Bool
accessable (_, (_, '.', _), _) = False
accessable grid = countAtSigns grid < 5
  where
    countAtSigns ((a, b, c), (d, e, f), (g, h, i)) =
      length $ filter (== '@') [a, b, c, d, e, f, g, h, i]