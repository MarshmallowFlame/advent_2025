import System.IO  
import Control.Monad

main = do  
    contents <- readFile "input.txt"
    let theWords = lines contents
    let finalState = foldl applyTurn (50,0) theWords
    print finalState

getDirectedStep :: String -> Integer
getDirectedStep a = case a of
        (d:steps) -> case d of
                'R' -> 1 * read steps
                'L' -> -1 * read steps

-- Input is the current state (position, nrZeroes) and next turn as a string
applyTurn :: (Integer, Integer) -> String -> (Integer, Integer)
applyTurn (position, nrZeroes) nextTurn = do
        let directedStep = getDirectedStep nextTurn
        let newPosition = mod (position + directedStep) 100
        let isZero = case newPosition of
                0 -> 1
                _ -> 0 
        (newPosition, nrZeroes+isZero)
