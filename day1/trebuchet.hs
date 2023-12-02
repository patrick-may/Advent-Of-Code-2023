import System.IO
import Data.Char 

leftDigit :: String -> Char 
leftDigit s = head [c | c <- s, isDigit c]

rightDigit :: String -> Char 
rightDigit s = head [c | c <- reverse s, isDigit c]

solveLine :: String -> Int 
solveLine [] = 0
solveLine inp = 10 * digitToInt (leftDigit inp) + digitToInt(rightDigit inp)

main :: IO ()
main = do 
    content <- readFile "input.in"
    let linesOfCont = lines content
    let result = sum $ map solveLine linesOfCont
    print result
    -- part one: 53080
