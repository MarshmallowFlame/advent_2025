-- Correct answer: 9280

main :: IO ()
main = do
  contents <- readFile "test.txt"
  let l = lines contents
      (count, finalGrid) = pickUntilDone l
  print count

  print finalGrid

pickUntilDone :: [[Char]] -> (Int, [[Char]])
pickUntilDone grid =
  let (newCount, newGrid) = pickStuff grid
   in if newCount == 0
        then (newCount, newGrid)
        else
          let (nextCount, nextGrid) = pickUntilDone newGrid
           in (newCount + nextCount, nextGrid)

pickStuff :: [[Char]] -> (Int, [[Char]])
pickStuff grid =
  let padded = fullPad grid
      zipped = zip3 padded (tail padded) (tail (tail padded))
      deepZipped = map (applyTriple zipS) zipped
      conv2d = concatMap zipThrees deepZipped
      picked = map pick conv2d
      count = length $ filter fst picked
      newGridFlat = map snd picked
      newGrid = splitAtX newGridFlat (length (head grid))
   in (count, newGrid)

fullPad :: [[Char]] -> [[Char]]
fullPad l =
  let hpadded = replicate (length $ head l) '.' : l ++ [replicate (length $ head l) '.']
      padded = map (\s -> '.' : s ++ ['.']) hpadded
   in padded

applyTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
applyTriple f (a, b, c) = (f a, f b, f c)

zipS :: String -> [(Char, Char, Char)]
zipS s = zip3 s (tail s) (tail (tail s))

zipThrees :: ([(a, a, a)], [(a, a, a)], [(a, a, a)]) -> [((a, a, a), (a, a, a), (a, a, a))]
zipThrees (a, b, c) = zip3 a b c

pick :: ((Char, Char, Char), (Char, Char, Char), (Char, Char, Char)) -> (Bool, Char)
pick (_, (_, '.', _), _) = (False, '.')
pick grid =
  let acc = countAtSigns grid < 5
      newC = if acc then '.' else '@'
   in (acc, newC)
  where
    countAtSigns ((a, b, c), (d, e, f), (g, h, i)) =
      length $ filter (== '@') [a, b, c, d, e, f, g, h, i]

splitAtX :: String -> Int -> [String]
splitAtX [] _ = []
splitAtX s x =
  let (l, r) = splitAt x s
   in l : splitAtX r x