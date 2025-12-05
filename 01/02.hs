main = do
  contents <- readFile "input.txt"
  let theWords = lines contents
  let (finalPosition, finalNrZeroes) = foldl applyTurn (50, 0) theWords
  print finalNrZeroes

getDirectedStep :: String -> Int
getDirectedStep a = case a of
  (d : steps) -> case d of
    'R' -> read steps
    'L' -> -read steps

-- Input is the current state (position, nrZeroes) and next turn as a string
applyTurn :: (Int, Int) -> String -> (Int, Int)
applyTurn (position, nrZeroes) nextTurn =
  let directedStep = getDirectedStep nextTurn

      newPositionRaw = (position + directedStep)
      newPosition = newPositionRaw `mod` 100

      -- There are a number of ways you can land on or pass 0.
      -- I see it like there is a symmetry around 0, and each
      -- multiple of 100 (on either side of absolute 0) corresponds
      -- to 0 mod 100.
      -- You always start in the first positive interval between 0 and 100,
      -- and so there are a number of zeroes you can cross or land on.
      -- I divide that into the "true" zero, and "fake" zeroes.

      -- This first check counts how many times we are passing or landing on
      -- these "other" zeroes, by dividing the new position with 100 mod 100.
      -- This will only account for crossing or landing on "fake" zeroes e.g.
      -- 100 or -200.
      zeroesFromWholeTurns = abs newPositionRaw `quot` 100

      -- The next check is for whether we crossed from positives into negatives,
      -- i.e. crossing the "true" zero. We do not want to count if we started on
      -- the "true" zero and continue into negatives, as that will be already have
      -- been accounted for in the previous turn.
      flipped = fromEnum $ signum newPositionRaw == -1 && position /= 0

      -- Finally we want to check if we landed on "true" zero. Landing on any
      -- other zeroes will be accounted for by zeroesFromWholeTurns.
      isZero = fromEnum $ newPositionRaw == 0
      newZeroes = nrZeroes + zeroesFromWholeTurns + flipped + isZero
   in (newPosition, newZeroes)
