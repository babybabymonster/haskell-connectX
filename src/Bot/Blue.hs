-- Assignment completed by
-- Name    :Yutong Wang
-- UID     :u6293753
-- Tutor   :James Kenneth Richardson
-- Lab Time:Friday 9:00 am - 11:00 am

module Bot.Blue where

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
    where add ys y = ys ++ [y]

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
            currentHeight = zipWith (\x y -> (fst x, (snd x)+(snd y))) movedColIndexes tupleMoves
            -- get the column indexes which height >= board height
            movedExceeds = map fst $ filter (\x -> snd x >= height) currentHeight

evaluate :: Board -> Board -> Score
evaluate boad bd = (myGetScore bd (turn boad)) - (myGetScore bd (otherPlayer $ turn boad))

maxAlg :: Board -> Tree Moves -> [(Score, Moves)]
maxAlg maxb (Node x []) = [(evaluate maxb (myUpdateBoard maxb x), x)]
maxAlg maxb (Node _ ls) = checkMinPrun $ map (minAlg maxb) ls

checkMinPrun :: [[(Score, Moves)]] -> [(Score, Moves)]
checkMinPrun [] = []
checkMinPrun (xs:xss) = n : (minPrun n xss)
      where  n = minOfTuple xs
             minPrun :: (Score, Moves) -> [[(Score, Moves)]] -> [(Score, Moves)]
             minPrun _ [] = []
             minPrun e (cs:css)  | minCompare e xs = minPrun e css
                                 | otherwise   = (minOfTuple cs) : minPrun (minOfTuple cs) css

             minCompare ::  (Score, Moves) -> [(Score, Moves)] -> Bool
             minCompare _ [] = False
             minCompare s (y:ys) | fst y <= (fst s) = True
                                 | otherwise = minCompare s ys

-- choose the tuple that has the largest first element
maxOfTuple :: [(Score, Moves)] -> (Score, Moves)
maxOfTuple tps = head $ filter(\z -> fst z == bestScore) tps
    where bestScore = maximum $ map fst tps

minAlg :: Board -> Tree Moves -> [(Score, Moves)]
minAlg minb (Node x []) = [(evaluate minb (myUpdateBoard minb x), x)]
minAlg minb (Node _ ls) = checkMaxPrun $ map (maxAlg minb) ls

checkMaxPrun :: [[(Score, Moves)]] -> [(Score, Moves)]
checkMaxPrun [] = []
checkMaxPrun (ms:mss) = n : (maxPrun n mss)
      where  n = minOfTuple ms
             maxPrun _ [] = []
             maxPrun e (zs:zss)
                    | maxCompare e zs = maxPrun e zss
                    | otherwise   = (minOfTuple zs) : maxPrun (minOfTuple zs) zss

             maxCompare _ [] = False
             maxCompare s (y:ys)
                    | fst y >= (fst s) = True
                    | otherwise = maxCompare s ys

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

-- test = Board{board = [[],[Red],[Red,Blue,Blue],[Red],[]], blueScore = 0, redScore = 0, turn = BlueBot, dimension = (5,3), connect = 4}
-- b = Board{board = [[],[],[],[Red],[Red],[Blue],[Red],[Red],[Blue],[],[],[],[Blue],[],[],[],[],[],[]], blueScore = 0, redScore = 0, turn = BlueBot, dimension = (20,1), connect = 5}
-- bb = Board{board = [[],[],[],[],[Blue,Red],[Blue,Red,Blue,Red,Blue,Red,Red,Blue,Red,Blue],[],[],[],[]], blueScore = 0, redScore= 0, turn = BlueBot,dimension = (11,10),connect = 5}
-- o = Board{board = [[],[Blue,Red],[Blue,Red,Red,Blue],[]], blueScore = 0, redScore= 0, turn = BlueBot,dimension = (4,4),connect = 3}