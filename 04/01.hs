-- Correct answer: 1569

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let l = lines contents
      hpadded = concat (replicate (length $ head l) ".") : l ++ [concat (replicate (length $ head l) ".")]
      padded = map (\s -> '.' : s ++ ['.']) hpadded
      zipped = zip3 padded (tail padded) (tail (tail padded))
      deepZipped = map (applyTriple zipS) zipped
      conv2d = concatMap zipThrees deepZipped
      accs = map accessable conv2d
      count = sum $ map fromEnum accs

  print count

applyTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
applyTriple f (a, b, c) = (f a, f b, f c)

zipS :: String -> [(Char, Char, Char)]
zipS s = zip3 s (tail s) (tail (tail s))

zipThrees :: ([(a, a, a)], [(a, a, a)], [(a, a, a)]) -> [((a, a, a), (a, a, a), (a, a, a))]
zipThrees (a, b, c) = zip3 a b c

accessable :: ((Char, Char, Char), (Char, Char, Char), (Char, Char, Char)) -> Bool
accessable (_, (_, '.', _), _) = False
accessable ((a, b, c), (d, e, f), (g, h, i)) = sum (map (fromEnum . (== '@')) [a, b, c, d, e, f, g, h, i]) < 5