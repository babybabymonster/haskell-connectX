module Bot.Red where

import Data.Board
import Data.List
import Data.Ord
import Data.Cell
import Data.Player
import Data.Column
import Data.List.Split (splitOn)
import Data.Universe.Helpers (diagonals)
-- Define 'Move' type that contains the index of the column and
-- the score of the board after inserting the piece into this column.
type Move = (Index, Score)


--import Data.Player


data Tree a = Node{root :: a,
                   subForest :: Forest a} deriving (Show,Eq)

type Forest a = [Tree a]

type GN = (Index, Player)

type GNS = [(Index, Player)]


test = genTree 2 Board{board = [[],[Red,Blue,Red,Blue],[]], blueScore = 0, redScore = 0, turn = RedBot, dimension = (3,4), connect = 4} 2 []

makeMove :: Board -> LookAhead -> Int
makeMove b depth
    | even depth = fst $ head $ snd $ maxAlg b (genTree 2 b depth [])
    | otherwise = fst $ head $ snd $ maxAlg b (genTree 1 b depth [])
--     RedBot -> fst $ head $ snd $ minAlg b (genTree b depth [])
--     | -> error "not a valid player"

-- generate a tree of [(Index, Player)]
genTree :: Int -> Board -> LookAhead -> GNS -> Tree GNS
genTree _ _ 0 gs = (Node gs [])
genTree x bod n gs = case x of
    1 -> (Node gs $ map (genTree 1 bod (n-1)) $ filter (\x -> validIndexes bod x) $ buildNodes gs (genGNSodd n bod))
    2 -> (Node gs $ map (genTree 2 bod (n-1)) $ filter (\x -> validIndexes bod x) $ buildNodes gs (genGNSeven n bod))
        -- where




validIndexes :: Board -> GNS -> Bool
validIndexes bb tuples
    | snd (head (maxOfSecOfTuple tupleMovedIndexes)) <= (snd $ dimension bb) = True
    | snd (head (maxOfSecOfTuple totalHeight)) <= (snd $ dimension bb) = True
    | otherwise = error "something's wrong"
    where
            33
            -- check hasWon

            -- the height of each column which has been put piece on
            totalHeight = zipWith (\x y -> (fst x, (snd x) + (snd y))) movedIndexesTuples tupleMovedIndexes
            movedIndexesTuples = filter (\x -> (fst x) `elem` (map fst tupleMovedIndexes)) tupleColIndexes
            tupleColIndexes = zip [1..fst $ dimension bb]lengthOfCol
            lengthOfCol = map length (board bb)
            -- get a list of tuples: (index, the occurence of this index)
            tupleMovedIndexes = zip noDuplicates lengthOfIndexes
            -- delete duplicate indexes in the list and sort the list
            noDuplicates = map head $ group $ sort $ map fst tuples
            -- get the occurence of each moved column indexes
            lengthOfIndexes = map length $ group $ sort $ map fst tuples

--             tupleMovedIndexes = zip noDuplicates lengthOfIndexes
--             -- delete duplicate indexes in the list and sort the list
--             noDuplicates = map head $ group $ sort listOfIndexes
--             -- get the occurence of each moved column indexes
--             lengthOfIndexes = map length groupedIndexes
--             groupedIndexes = group $ sort listOfIndexes
--             listOfIndexes = map fst tuples

hasWon :: Board -> Bool
hasWon b = columnWin || rowWin || diagonalWin || otherDiagWin
    where
        streak      = connect b
        unfilledMat = board b
        height      = snd $ dimension b

        identicalElems :: (Eq a) => [a] -> Bool
        identicalElems list = case list of
            x : y : xs -> x == y && identicalElems (y : xs)
            _          -> True

        columnWin    = or $ map winInColumn unfilledMat
        rowWin       = or $ map winInColumn
                            $ concatMap (splitOn [Empty])
                                $ transpose
                                    $ map (fillColumn Empty height) unfilledMat
        diagonalWin  = or $ map winInColumn
                            $ concatMap (splitOn [Empty])
                                $ diagonals
                                    $ map (fillColumn Empty height) unfilledMat
        otherDiagWin = or $ map winInColumn
                                $ concatMap (splitOn [Empty])
                                    $ diagonals
                                        $ map (reverse . (fillColumn Empty height))
                                            unfilledMat

        winInColumn :: Column Cell -> Bool
        winInColumn list = case list of
            []     -> False
            _ : xs
                | length list >= streak ->
                    identicalElems (take streak list)
                    || winInColumn xs
                | otherwise             -> False

-- choose the tuple that has the largest second element
maxOfSecOfTuple :: [(Index, Int)] -> [(Index, Int)]
maxOfSecOfTuple tps = filter (\z -> snd z == bestScore) tps
    where bestScore = maximum $ map snd tps


-- mostCommon list = fst . maximumBy (compare `on` snd) $ elemCount
--       where  elemCount = map (head &&& length) . group . sort $ list

buildNodes :: GNS -> GNS -> [GNS]
buildNodes gs1 gs2 = map (add gs1) gs2
    where add gns g = gns ++ [g]

genGNSodd :: LookAhead -> Board -> GNS
genGNSodd look bd
    | odd look = zip ordIndexes (replicate (length ordIndexes) (turn bd))
    | even look = zip ordIndexes (replicate (length ordIndexes) (otherPlayer (turn bd)))
    where   ordIndexes = sortBy (comparing (\i -> abs $ (fst(dimension bd) + 1) `div` 2 - i)) indexes
            indexes = filter (\x -> x `notElem` exceedIndexes) [1..fst(dimension bd)]
            exceedIndexes = map fst $ filter (\x -> snd x >= (snd $ dimension bd)) tupleColIndexes
            tupleColIndexes = zip [1..fst $ dimension bd] lengthOfCol
            lengthOfCol = map length (board bd)



genGNSeven :: LookAhead -> Board -> GNS
genGNSeven look bd
    | even look = zip ordIndexes (replicate (length ordIndexes) (turn bd))
    | odd look = zip ordIndexes (replicate (length ordIndexes) (otherPlayer (turn bd)))
    where   ordIndexes = sortBy (comparing (\i -> abs $ (fst(dimension bd) + 1) `div` 2 - i)) indexes
            indexes = filter (\x -> x `notElem` exceedIndexes) [1..fst(dimension bd)]
            exceedIndexes = map fst $ filter (\x -> snd x >= (snd $ dimension bd)) tupleColIndexes
            tupleColIndexes = zip [1..fst $ dimension bd] lengthOfCol
            lengthOfCol = map length (board bd)

evaluate :: Board -> Board -> Score
evaluate boad bd = (myGetScore bd (turn boad)) - (myGetScore bd (otherPlayer $ turn boad))

maxAlg :: Board -> Tree GNS -> (Score, GNS)
maxAlg maxb (Node x []) = (evaluate maxb (myUpdateBoard maxb x), x)
maxAlg maxb (Node _ ls) = maxOfTuple $ map (minAlg maxb) ls

-- choose the tuple that has the largest first element
maxOfTuple :: [(Score, GNS)] -> (Score, GNS)
maxOfTuple tps = head $ filter(\z -> fst z == bestScore) tps
    where bestScore = maximum $ map fst tps

minAlg :: Board -> Tree GNS -> (Score, GNS)
minAlg minb (Node x []) = (evaluate minb (myUpdateBoard minb x), x)
minAlg minb (Node _ ls) = minOfTuple $ map (maxAlg minb) ls

-- choose the tuple that has the smallest first element
minOfTuple :: [(Score, GNS)] -> (Score, GNS)
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

--         calculateScore mat =
--             sum $ (map (streakScore . length))
--                 $ filter ((>=minStreak).length)
--                     $ concatMap
--                         (concatMap (splitOn [corresCell otherBot]))
--                             $ map (splitOn [Empty]) mat

        streakScore :: Int -> Score
        streakScore i
            | i < minStreak     = 0
            | i < (streak -1)   = 1 * 100
            | i == (streak -1)  = i * 1000
            | i >= streak       = i * 10000
            | otherwise         = 0

myUpdateBoard :: Board -> GNS -> Board
myUpdateBoard grid (y : ys) = myUpdateBoard (updateBoardNoScore grid $ fst y) ys
myUpdateBoard grid [] = grid
