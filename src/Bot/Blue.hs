-- Assignment completed by
-- Name    :Yutong Wang
-- UID     :u6293753
-- Tutor   :James Kenneth Richardson
-- Lab Time:Friday 9:00 am - 11:00 am

module Bot.Blue where

import Data.Board
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

-- | genTree
-- >>> genTree Board{board = [[],[Blue,Red],[Blue,Red,Red,Blue],[]], blueScore = 0, redScore= 0, turn = BlueBot,dimension = (4,4),connect = 3} 2 []
-- Node {root = [], subForest = [Node {root = [2], subForest = [Node {root = [2,2], subForest = []},Node {root = [2,1], subForest = []},Node {root = [2,4], subForest = []}]},Node {root = [1], subForest = [Node {root = [1,2], subForest = []},Node {root = [1,1], subForest = []},Node {root = [1,4], subForest = []}]},Node {root = [4], subForest = [Node {root = [4,2], subForest = []},Node {root = [4,1], subForest = []},Node {root = [4,4], subForest = []}]}]}
--
-- | makeMove
-- >>> makeMove Board{board = [[],[Red],[Red,Blue,Blue],[Red],[]], blueScore = 0, redScore = 0, turn = BlueBot, dimension = (5,3), connect = 4} 2
-- 2
--
-- | buildNodes
-- >>> buildNodes Board{board = [[],[Red],[Red,Blue,Blue],[Red],[]], blueScore = 0, redScore = 0, turn = BlueBot, dimension = (5,3), connect = 4} [1,4]
-- [[1,4,2],[1,4,4],[1,4,1],[1,4,5]]
--
-- | validIndexes
-- >>> validIndexes Board{board = [[],[Red],[Red,Blue,Blue],[Red],[]], blueScore = 0, redScore = 0, turn = BlueBot, dimension = (5,3), connect = 4} []
-- [2,4,1,5]
--
-- | myGetScore
-- >>> myGetScore Board{board = [[],[],[],[Red],[Red],[Blue],[Red],[Red],[Blue],[],[],[],[Blue],[],[],[],[],[],[]], blueScore = 0, redScore = 0, turn = BlueBot, dimension = (20,1), connect = 5} RedBot
-- 2000
--
-- | checkMinPrun
-- >>> checkMinPrun [[(1000,[2,3,1])],[(2000,[2,3,2])],[(900,[2,3,3])]]
-- [(1000,[2,3,1])]
--
-- | maxOfTuple
-- >>> maxOfTuple [(2,[3,1]),(10,[3,2]),(0,[3,3])]
-- (10,[3,2])


makeMove :: Board -> LookAhead -> Int
makeMove b depth = head $ snd $ head $ maxAlg b (genTree b depth [])

-- generate a tree of Moves
genTree :: Board -> LookAhead -> Moves -> Tree Moves
genTree _ 0 ms = (Node ms [])
genTree bod n ms = (Node ms $ map (genTree bod (n-1)) $ buildNodes bod ms)


buildNodes :: Board -> Moves -> [Moves]
buildNodes bo mo = map (add mo) $ validIndexes bo mo
    where   -- append each index in the list of valid indexes to the original Moves
            add ys y = ys ++ [y]

-- remain only the valid indexes to form new nodes
validIndexes :: Board -> Moves -> Moves
validIndexes bd mov = filter (\x -> x `notElem` movedExceeds) ordIndexes
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

-- evaluate the board of terminal nodes
evaluate :: Board -> Board -> Score
evaluate boad bd = (myGetScore bd (turn boad)) - (myGetScore bd (otherPlayer $ turn boad))

-- max algorithm of minimax
maxAlg :: Board -> Tree Moves -> [(Score, Moves)]
maxAlg maxb (Node x []) = [(evaluate maxb (myUpdateBoard maxb x), x)]
maxAlg maxb (Node _ ls) = checkMinPrun $ map (minAlg maxb) ls

-- possible pruning
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

-- min algorithm of minimax
minAlg :: Board -> Tree Moves -> [(Score, Moves)]
minAlg minb (Node x []) = [(evaluate minb (myUpdateBoard minb x), x)]
minAlg minb (Node _ ls) = checkMaxPrun $ map (maxAlg minb) ls

-- possible pruning
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

-- my getScore function
myGetScore :: Board -> Player -> Score
myGetScore bg p = sum [columnScore, rowScore, diagonalScore, otherDiagScore]
    where
        streak    = connect bg
        minStreak = 1 + streak `div` 2
        otherBot  = otherPlayer p

        columnScore    =  calColScore $ filledMatrix
        rowScore       =  calScore $ transpose $ filledMatrix
        diagonalScore  =  calScore $ diagonals $ filledMatrix
        otherDiagScore =  calScore $ diagonals $ map reverse $ filledMatrix

        filledMatrix   = map (fillColumn Empty $ snd $ dimension bg) $ board bg

        -- get the last part of each column only contains player's bot,
        -- the length of empty pieces plus the length of player's bot must >= streak
        -- which means it is possible to make achieve streak
        calColScore mat = sum $ (map (streakScore . length)) $ map concat
                              $ map (splitOn [Empty]) $ filter ((>=streak).length)
                                    $ map last $ map (splitOn [corresCell otherBot]) mat

        -- remain only the parts that has enough space to win
        y mat = map (splitOn [Empty]) $ filter ((>=streak).length)
                $ concatMap (splitOn [corresCell otherBot]) mat
        -- find the situation when there spaces before and after my bots
        emptyTwoEnd mat = filter (\x -> head x == [] && last x == []) (y mat)
        -- add bonus for the above situation
        bonusTwoEnd mat = sum $ map (bonusScore . length) $ filter ((>=(streak -2)).length) $ map concat (emptyTwoEnd mat)
        -- add bonus for the situation that there are several possibilities to win
        number mat = 10 * length (y mat)
        -- find the situation when there is only one space in the middle: [o,o,o, ,o,o]
        -- calculate the length of my bot of each part that has a chance to win
        calScore mat = (number mat) * (sum $ map (streakScore . length) (map concat $ y mat)) + (bonusTwoEnd mat)


        streakScore :: Int -> Score
        streakScore i
            | i < (streak `div` 2)  = 0
            | i < minStreak         = i * 50
            | i < (streak -2)       = 1 * 100
            | i == (streak -2)      = i * 800
            | i == (streak -1)      = i * 1000
            | i >= streak           = i * 20000
            | otherwise             = 0

        bonusScore s = s * 600

-- update a board using a list of indexes
myUpdateBoard :: Board -> Moves -> Board
myUpdateBoard grid (y : ys) = myUpdateBoard (updateBoardNoScore grid y) ys
myUpdateBoard grid [] = grid