import Data.List.Split (splitOn)
import Data.Char
import Debug.Trace

data CubeGame = CubeGame {
    gameId:: Int,
    green :: Int,
    blue  :: Int, 
    red   :: Int
} deriving Show

-- the limits of the game
cubeLimit = CubeGame {
    gameId = (-1),
    green = 13,
    blue = 14,
    red = 12
}

-- compare a game to the limits of the game
isPossible :: CubeGame -> Bool
isPossible game = (green game) <= (green cubeLimit)
    && (blue game) <= (blue cubeLimit)
    && (red game) <= (red cubeLimit)

-- return a cube game struct of the line
parseGame :: String -> CubeGame 
parseGame line = 
                CubeGame {
                    gameId = gameUUID,
                    green = greenMax,
                    blue = blueMax,
                    red = redMax
                } 
                where 
                    -- evil imperative style splicing...
                    gameSpl = splitOn ": " line
                    subGames = concat [splitOn "; " g | g <- tail gameSpl]
                    subVals = concat [splitOn ", " g | g <- subGames]
                    greenMax = maximum [read $ head $ words v :: Int | v <- subVals, "green" `elem` (words v)]
                    redMax = maximum [read $ head $ words v :: Int | v <- subVals, "red" `elem` (words v)]
                    blueMax = maximum [read $ head $ words v :: Int | v <- subVals, "blue" `elem` (words v)]
                    gameUUID = read [ch | ch <- head gameSpl, isDigit ch] :: Int


main :: IO () 
main = do
    content <- readFile "input.in"
    let games = lines content
--    let gameVals = map parseGame games
--    let posses = map isPossible gameVals
--    minor tweak here, that it!!! (wtf)
    let possible = sum [blue g * red g * green g | g <- (map parseGame games)]
    print possible
