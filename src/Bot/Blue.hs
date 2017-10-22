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

type GN = (Index, Player)

type GNS = [(Index, Player)]



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

        streakScore :: Int -> Score
        streakScore i
                    | i < minStreak     = 0
                    | i < (streak -1)   = 1 * 100
                    | i == (streak -1)  = i * 1000
                    | i >= streak       = i * 10000
                    | otherwise         = 0

-- myGetScore' :: Board -> Player -> Score
myGetScore' b p = rowScore
-- myGetScore b p = sum [columnScore, rowScore, diagonalScore, otherDiagScore]
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
--         tt =
        -- remain only the parts that has enough space to win
        y mat = map (splitOn [Empty]) $ filter ((>=streak).length)
                $ concatMap (splitOn [corresCell otherBot]) mat
        -- find the situation when there spaces before and after my bots
        emptyTwoEnd mat = filter (\x -> head x == [] && last x == []) (y mat)
        -- add bonus for the above situation
        bonusTwoEnd mat = sum $ map (bonusScore . length) $ filter ((>=(streak -2)).length) $ map concat (emptyTwoEnd mat)
        -- add bonus for the situation that there are several possibilities to win
        number mat = 10 * length (y mat)
        -- calculate the length of my bot of each part that has a chance to win
        calScore mat = (number mat) * (sum $ map (streakScore . length) (map concat $ y mat)) + (bonusTwoEnd mat)

--         calScore mat = sum $ (map (streakScore . length)) $ map concat $ map (splitOn [Empty])
--                            $ map (filter ((>=streak).length))
--                                 $ map (splitOn [corresCell otherBot]) mat

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

        bonusScore s = s * 600

myUpdateBoard :: Board -> GNS -> Board
myUpdateBoard grid (y : ys) = myUpdateBoard (updateBoardNoScore grid $ fst y) ys
myUpdateBoard grid [] = grid

test = Board{board = [[],[Red],[Red,Blue,Blue],[Red],[]], blueScore = 0, redScore = 0, turn = BlueBot, dimension = (5,3), connect = 4}
b = Board{board = [[],[],[],[Red],[Red],[Red],[],[Blue],[],[Red],[Red],[Red],[Blue],[Red],[Red],[Red],[],[],[]], blueScore = 0, redScore = 0, turn = BlueBot, dimension = (20,1), connect = 5}