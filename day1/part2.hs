import System.IO
import Data.Char 

leftDigit :: String -> Char 
leftDigit s = head [c | c <- s, isDigit c]

rightDigit :: String -> Char 
rightDigit s = head [c | c <- reverse s, isDigit c]

solveLine :: String -> Int 
solveLine [] = 0
solveLine inp = 10 * digitToInt (leftDigit inp) + digitToInt(rightDigit inp)

-- for part two
prefixToDigits :: String -> String 
prefixToDigits ('o' : 'n' : 'e' : rest) = "1" ++ prefixToDigits ("ne" ++ rest)
prefixToDigits ('t' : 'w' : 'o' : rest) = "2" ++  prefixToDigits ("wo" ++ rest)
prefixToDigits ('t' : 'h' : 'r' : 'e' : 'e' : rest) = "3" ++ prefixToDigits ("hree" ++ rest)
prefixToDigits ('f' : 'o' : 'u' : 'r' : rest) = "4" ++ prefixToDigits ("our" ++ rest)
prefixToDigits ('f' : 'i' : 'v' : 'e' : rest) = "5" ++ prefixToDigits ("ive" ++ rest)
prefixToDigits ('s' : 'i' : 'x' : rest) = "6" ++ prefixToDigits ("ix" ++ rest)
prefixToDigits ('s' : 'e' : 'v' : 'e' : 'n' : rest) = "7" ++ prefixToDigits ("even" ++ rest)
prefixToDigits ('e' : 'i' : 'g' : 'h' : 't' : rest) = "8" ++ prefixToDigits ("ight" ++ rest)
prefixToDigits ('n' : 'i' : 'n' : 'e' : rest) = "9" ++ prefixToDigits ("ine" ++ rest)
prefixToDigits (c : rest) = c : prefixToDigits rest
prefixToDigits [] = []

main :: IO ()
main = do 
    content <- readFile "input.in"
    let linesOfCont = lines content
    let fixWords = map prefixToDigits linesOfCont
    -- print partTwoProc
    -- part one: 53080
    let result = sum (map solveLine fixWords)
    print result
