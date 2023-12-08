import Data.List.Split (splitOn)
import Data.Char
import Debug.Trace 
import Data.List

testFile = "test.in"
inputFile = "input.in"
-- scratch card data container
data Scratcher = Scratcher {
    winners     :: [Int],
    foundNums   :: [Int],
    cardId      :: Int
} deriving (Show, Eq)

data QueuedCard = QueuedCard {
    numMatches      :: Int, 
    queuedQuantity  :: Int 
} deriving (Show, Eq)

type Queue = [QueuedCard]

-- parse a line to be a scratch card
scratcherParser :: String -> Scratcher 
scratcherParser inp =
    Scratcher {
        winners = foundWins,
        foundNums = foundEntries,
        cardId = foundCardId } 
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

-- for part 1
countScratcherPoints :: Scratcher -> Integer
countScratcherPoints scratchoff = if winCt > 0 then 2 ^ (winCt - 1) else 0
    where 
        winCt = length $ intersect (winners scratchoff) (foundNums scratchoff)

-- get range of winners
getWinningQty :: Scratcher -> [Int]
getWinningQty scratcher = [1..(length $ intersect (winners scratcher) (foundNums scratcher))]

-- had to do :set -XFlexibleContexts ???
countQuantityScratchers :: Eq [Scratcher] => [Scratcher] -> Integer
countQuantityScratchers [] = 0 
countQuantityScratchers (first:rest) = 
    -- trace ("CurrCard is " ++ show first ++
    --     "\nRest Card IDs are " ++ show remCardIdx ++ 
    --     "\nAdditional Cards added are: " ++ show duplicateCards ++
    --     "\nAdditional Card ids added are: " ++ show duplicateIds ++
    --     "\n") $
    1 + countQuantityScratchers (duplicateCards ++ rest)
    where 
        -- extra scratches generated
        duplicateIds = [cardId first + offset | offset <- getWinningQty first]
        duplicateCards = [remCard | remCard <- nub (rest), cardId remCard `elem` duplicateIds]
        remCardIdx = [cardId c | c <- rest]

-- generate list of pairs of Int (card id) to list of Ints (card ids spawned from that first card)
--
gens :: [Scratcher] -> [(Int, Int)]
gens [] = []
gens (first:rest) = [(cardId first, wonCards)] ++ gens rest 
    where 
        wonCards = length $ getWinningQty first

traverseQueue :: [(Int, Int)] -> Integer
traverseQueue [] = 0
traverseQueue ((_, b):rest) = 1 + traverseQueue (rest ++ spawnedCards)
    where 
        spawnedCards = take b rest


backwardsFold :: (Int, Int) -> [(Int, Int)] -> (Int, Int)
backwardsFold (loc, spawns) pool = (loc, 1 + childCards)
    where 
        childCards = sum [snd v | v <- pool, fst v > loc, fst v <= loc + spawns]

constructCardCosts :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
constructCardCosts x:xs pool = (backwardsFold x pool) ++ pool
-- countCreated :: [(Int, [Int])] -> Integer
-- countCreated [] = 0
-- countCreated ((_, spawns):rest) = 
main :: IO () -- entrypoint 
main = do 
    testContent <- fmap lines $ readFile testFile
    inpContent <- fmap lines $ readFile inputFile 
    
    let testPart1 = sum $ map countScratcherPoints $ map scratcherParser testContent 
    let inputPart1 = sum $ map countScratcherPoints $ map scratcherParser inpContent 
    print ("Test Part 1 Value is " ++ show testPart1)
    print ("Input Part 1 Value is " ++ show inputPart1)

    -- let testPart1 = countQuantityScratchers $ map scratcherParser testContent
    -- let inputPart2 = countQuantityScratchers $ map scratcherParser inpContent
    -- print ("Test Part 2 Value is " ++ show testPart2)
    -- print ("Input Part 2 Value is " ++ show inputPart2)


