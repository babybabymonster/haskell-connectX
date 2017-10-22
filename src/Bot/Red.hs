module Bot.Red where

import Data.Board
--import Data.Player
import Data.List
import Data.Ord
import Data.Cell
import Data.Player
import Data.Column
import Data.List.Split (splitOn)
import Data.Universe.Helpers (diagonals)

data Tree a = Node{root :: a,
                   subForest :: Forest a} deriving (Show,Eq)

type Forest a = [Tree a]

type Moves = [Index]

type S = (Score, Moves)


makeMove :: Board -> LookAhead -> Int
makeMove b depth = head $ snd $ head $ maxAlg b (genTree b depth [])
   --     RedBot -> fst $ head $ snd $ minAlg b (genTree b depth [])
--     | -> error "not a valid player"

-- generate a tree of [Index]
genTree :: Board -> LookAhead -> Moves -> Tree Moves
genTree _ 0 ms = (Node ms [])
genTree bod n ms = (Node ms $ map (genTree bod (n-1)) $ buildNodes bod ms)

buildNodes :: Board -> Moves -> [Moves]
buildNodes bo mo = map (add mo) $ validIndexes bo mo
    where add bs b = bs ++ [b]

validIndexes :: Board -> Moves -> Moves
validIndexes bd mov = filter (\x -> x `notElem` movedExceeds) ordIndexes
-- t bd mov = filter (\x -> x `notElem` movedExceeds) ordIndexes
    where
            -- prioritise the center column
            ordIndexes = sortBy (comparing (\i -> abs $ (width + 1) `div` 2 - i)) indexes
            -- delete the index of column which have already fulled
            indexes = filter (`notElem` exceedIndexes) [1..width]
            width = fst $ dimension bd
            height = snd $ dimension bd
            -- find the indexes of the columns that have fulled originally
            exceedIndexes = map fst $ filter (\x -> snd x >= height) tupleColumns
            -- calculate the height of each column
            lengthOfCol = map length (board bd)
            -- the index of each column
            colIndexes = [1..width]
            -- make a lists of tuples: (column index, column height)
            tupleColumns = zip colIndexes lengthOfCol
            -- get the occurrences of each sorted index in moves
            occurrences = map length $ group $ sort mov
            -- sort the indexes in moves and delete duplicates
            moveIndex = map head $ group $ sort mov
            -- make a list of tuples: (move index, index occurrence)
            tupleMoves = zip moveIndex occurrences
            -- remain only the column index that have been put pieces in
            movedColIndexes = filter (\x -> fst x `elem` (map fst tupleMoves)) tupleColumns
            -- the current height of each column: (index, height)
            currentHeight = zipWith (\x y -> (fst x, (snd x)+(snd y))) tupleColumns tupleMoves
            -- get the column indexes which height >= board height
            movedExceeds = map fst $ filter (\x -> snd x >= height) currentHeight

evaluate :: Board -> Board -> Score
evaluate boad bd = (myGetScore bd (turn boad)) - (myGetScore bd (otherPlayer $ turn boad))

maxAlg :: Board -> Tree Moves -> [(Score, Moves)]
maxAlg maxb (Node x []) = [(evaluate maxb (myUpdateBoard maxb x), x)]
maxAlg maxb (Node _ ls) = mapMin $ map (minAlg maxb) ls

mapMin :: [[(Score, Moves)]] -> [(Score, Moves)]
mapMin [] = []
mapMin (xs:rest) = n : (omit n rest)
  where n = minOfTuple xs
        omit :: (Score, Moves) -> [[(Score, Moves)]] -> [(Score, Moves)]
        omit _ [] = []
        omit n (xs:rest) | minLeq n xs = omit n rest
                         | otherwise   = k : omit k rest
                             where k = minOfTuple xs

        minLeq ::  (Score, Moves) -> [(Score, Moves)] -> Bool
        minLeq _ [] = False
        minLeq n (y:ys) | fst y <= (fst n) = True
                        | otherwise = minLeq n ys

-- choose the tuple that has the largest first element
maxOfTuple :: [(Score, Moves)] -> (Score, Moves)
maxOfTuple tps = head $ filter(\z -> fst z == bestScore) tps
    where bestScore = maximum $ map fst tps

minAlg :: Board -> Tree Moves -> [(Score, Moves)]
minAlg minb (Node x []) = [(evaluate minb (myUpdateBoard minb x), x)]
minAlg minb (Node _ ls) = mapMax $ map (maxAlg minb) ls

mapMax :: [[(Score, Moves)]] -> [(Score, Moves)]
mapMax [] = []
mapMax (xs:rest) = n : (omit' n rest)
  where n = minOfTuple xs
        omit' _ [] = []
        omit' n (xs:rest) | maxLeq n xs = omit' n rest
                          | otherwise   = k : omit' k rest
                              where k = minOfTuple xs
        maxLeq _ [] = False
        maxLeq n (y:ys) | fst y >= (fst n) = True
                        | otherwise = maxLeq n ys

-- choose the tuple that has the smallest first element
minOfTuple :: [(Score, Moves)] -> (Score, Moves)
minOfTuple tps = head $ filter(\z -> fst z == bestScore) tps
    where bestScore = minimum $ map fst tps

myGetScore :: Board -> Player -> Score
myGetScore b p = sum [columnScore, rowScore, diagonalScore, otherDiagScore]
    where
        streak    = connect b
        minStreak = 1 + streak `div` 2
        otherBot  = otherPlayer p

        columnScore    =  calColScore $ filledMatrix
        rowScore       =  calScore $ transpose $ filledMatrix
        diagonalScore  =  calScore $ diagonals $ filledMatrix
        otherDiagScore =  calScore $ diagonals $ map reverse $ filledMatrix

        filledMatrix   = map (fillColumn Empty $ snd $ dimension b) $ board b

        calColScore mat = sum $ (map (streakScore . length)) $ map concat
                              $ map (splitOn [Empty]) $ filter ((>=streak).length)
                                    $ map last $ map (splitOn [corresCell otherBot]) mat

        calScore mat = sum $ (map (streakScore . length)) $ concatMap (splitOn [Empty])
                           $ filter ((>=streak).length) $ map concat
                                $ map (splitOn [corresCell otherBot]) mat

        streakScore :: Int -> Score
        streakScore i
                    | i < minStreak     = 0
                    | i < (streak -1)   = 1 * 100
                    | i == (streak -1)  = i * 1000
                    | i >= streak       = i * 10000
                    | otherwise         = 0


myUpdateBoard :: Board -> Moves -> Board
myUpdateBoard grid (y : ys) = myUpdateBoard (updateBoardNoScore grid y) ys
myUpdateBoard grid [] = grid

test = Board{board = [[],[Red],[Red,Blue,Blue],[Red],[]], blueScore = 0, redScore = 0, turn = BlueBot, dimension = (5,3), connect = 4}
b = Board{board = [[],[],[],[Red],[Red],[Blue],[Red],[Red],[Blue],[],[],[],[Blue],[],[],[],[],[],[]], blueScore = 0, redScore = 0, turn = BlueBot, dimension = (20,1), connect = 5}
bb = Board{board = [[],[],[],[],[Blue,Red],[Blue,Red,Blue,Red,Blue,Red,Red,Blue,Red,Blue],[],[],[],[]], blueScore = 0, redScore= 0, turn = BlueBot,dimension = (11,10),connect = 5}
o = Board{board = [[],[Blue,Red],[Blue,Red,Red,Blue],[]], blueScore = 0, redScore= 0, turn = BlueBot,dimension = (4,4),connect = 3}