import Data.List.Split (splitOn)
import Data.Char
import Debug.Trace 
import Data.List
import qualified Data.HashMap.Strict as HM 
import Data.Hashable (Hashable)

testFile = "test.in"
inputFile = "input.in"
-- scratch card data container
data Scratcher = Scratcher {
    winners     :: [Int],
    foundNums   :: [Int],
    cardId      :: Int,
    winCt       :: Int
} deriving (Show, Eq)

-- parse a line to be a scratch card
scratcherParser :: String -> Scratcher 
scratcherParser inp =
    Scratcher {
        winners = foundWins,
        foundNums = foundEntries,
        cardId = foundCardId,
        winCt = foundWinCt
    } 
    where 
        -- split on ': ' line in half
        gameSpl = splitOn ": " inp
        -- helper value
        scratchWinsAndNums = concat $ tail gameSpl
        -- values from parsing 
        foundCardId = read [dig | dig <- head gameSpl, isDigit dig] :: Int
        -- pool is list of space separated strings before |
        foundWins = [read num :: Int | num <- words $ head $ splitOn " | " scratchWinsAndNums ]
        -- pool is list of space separated strings after |
        foundEntries = [read num :: Int | num <- words $ concat $ tail $ splitOn " | "  scratchWinsAndNums]
        -- ct of winning numbers 
        foundWinCt = length $ intersect foundWins foundEntries

-- for part 1
countScratcherPoints :: Scratcher -> Integer
countScratcherPoints scratchoff = if winCt > 0 then 2 ^ (winCt - 1) else 0
    where 
        winCt = length $ intersect (winners scratchoff) (foundNums scratchoff)

-- part 2
countQuantityScratchers :: Eq [Scratcher] => [Scratcher] -> Integer
countQuantityScratchers n = 1

-- make map of card Ids to generated cardIds
linkScratchers :: Eq [Scratcher] => HM.HashMap Int [Int]
linkScratchers scratchlist = foldr insertData HM.empty scratchlist
    where
        k = cardId d 
        v = [1 + val | val <- [0..(winCt d)]]

        insertData hm kvpair = HM.insert k v hm

        
main :: IO () -- entrypoint 
main = do 
    testContent <- fmap lines $ readFile testFile
    inpContent <- fmap lines $ readFile inputFile 
    
    let testPart1 = sum $ map countScratcherPoints $ map scratcherParser testContent 
    let inputPart1 = sum $ map countScratcherPoints $ map scratcherParser inpContent 
    print ("Test Part 1 Value is " ++ show testPart1)
    print ("Input Part 1 Value is " ++ show inputPart1)
    
    let relearn2a = linkScratchers $ map scratcherParser testContent
    let relearn2b = map scratcherParser inpContent
    print ("Parsed Input 1 is " ++ show relearn2a)
    --print ("Parsed Input 2 is " ++ show relearn2b)

    -- let testPart2 = countQuantityScratchers $ map scratcherParser testContent
    -- let inputPart2 = countQuantityScratchers $ map scratcherParser inpContent
    -- print ("Test Part 2 Value is " ++ show testPart2)
    -- print ("Input Part 2 Value is " ++ show inputPart2)
