import System.IO  
import Control.Monad

main = do  
    contents <- readFile "input.txt"
    let theWords = lines contents
    let (finalPosition, finalNrZeroes) = foldl applyTurn (50,0) theWords
    print finalNrZeroes

getDirectedStep :: String -> Integer
getDirectedStep a = case a of
        (d:steps) -> case d of
                'R' -> 1 * read steps
                'L' -> -1 * read steps

-- Input is the current state (position, nrZeroes) and next turn as a string
applyTurn :: (Integer, Integer) -> String -> (Integer, Integer)
applyTurn (position, nrZeroes) nextTurn =
        let directedStep = getDirectedStep nextTurn
            newPosition = mod (position + directedStep) 100
            isZero = if newPosition == 0 then 1 else 0
        in (newPosition, nrZeroes+isZero)
