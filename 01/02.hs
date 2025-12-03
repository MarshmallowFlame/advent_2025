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

            newPositionRaw = (position + directedStep)
            newPosition = mod newPositionRaw 100

            -- There are a number of ways you can land on or pass 0.
            zeroesFromWholeTurns = quot (abs newPositionRaw) 100
            flipped = if ((signum newPositionRaw == -1) && (position/=0)) then 1 else 0
            isZero = if newPositionRaw == 0 then 1 else 0
            newZeroes = nrZeroes+zeroesFromWholeTurns+flipped+isZero
        in (newPosition, newZeroes)
