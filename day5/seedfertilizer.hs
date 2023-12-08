module Day5 where 

import Debug.Trace
import Data.List.Split
import Data.Char


inputFile = "input.in"
testFile = "test.in"

data Range = Range {
    destStart :: Integer, -- transformed startpoint
    origStart :: Integer, -- initial startpoint
    rangeSpan      :: Integer -- span (inclusive left, exclusive right]
} deriving (Show, Eq)

data GardenMap = GardenMap {
    ranges      :: [Range], -- a garden map has multiple ranges
    source      :: String, -- also technically a start string
    destination :: String -- end string
} deriving (Show, Eq)

transmog :: Integer -> GardenMap -> Integer
transmog start converter = outval 
    where 
        -- helper function to see if a certain value is in a certain range
        inRange check currRange = (origStart currRange <= check && origStart currRange + rangeSpan currRange > check) 
        -- filter down to only the current range that works
        outvalRange = head $ filter (\x -> inRange start x) (ranges converter)
        -- shift range and start val to output value 
        outval = (start - origStart outvalRange) + destStart outvalRange

-- shamelessly yoinked from github solutions
parseInput :: String -> [String]
parseInput x = splitOn "\n\n \n" $ filter (\x -> isDigit x || x == '\n' || x == ' ') x

getSeeds :: String -> [Integer]
getSeeds x = [read v :: Integer | v <- splitOn " " (drop 1 x)] -- drop 1 for extra space

parseMapping :: String -> GardenMap 
parseMapping x = GardenMap {
    ranges = foundRanges,
    source = "X", -- source, dest unnecessary, we just go through paths sequentially
    destination = "X" -- ^^ 
}
    where 
        parseRange inp = Range {
            destStart = f!!0
            origStart = f!!1
            rangeSpan = f!!2
        }
        where 
            f = [read v :: Integer | v <- splitOn " " inp]
        foundRanges = map parseRange (splitOn "\n" x)


main :: IO ()
main = do 
    iinp <- lines <$> readFile inputFile
    tinp <- lines <$> readFile testFile
     
    -- part 1 Stuff 

    -- print(iinp)
    print(tinp)
