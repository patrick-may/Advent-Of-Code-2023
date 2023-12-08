testFile = "test.in"
inputFile = "input.in"

-- heavily inspired by https://lettier.github.io/posts/2016-04-29-breadth-first-search-in-haskell.html 
data Node = Node {
        label :: [Char]
    ,   neighbors :: [[Char]]
    ,   distance :: Int 
    ,   predecessor :: [Char]
} deriving (Show)

-- a graph is a list of nodes, each node contains information about adjacents
data Graph = Graph [Node] deriving (Show)


main :: IO ()
main = do 
    testInput <- readFile testFile 
    problemInput <- readFile inputFile 
    print testInput
--    print problemInput
