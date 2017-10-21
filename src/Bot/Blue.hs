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
    | odd depth = fst $ head $ snd $ maxAlg b (genTree 1 b depth [])
    | otherwise = fst $ head $ snd $ maxAlg b (genTree 2 b depth [])
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
            indexes = [1..fst(dimension bd)]

genGNSeven :: LookAhead -> Board -> GNS
genGNSeven look bd
    | even look = zip ordIndexes (replicate (length ordIndexes) (turn bd))
    | odd look = zip ordIndexes (replicate (length ordIndexes) (otherPlayer (turn bd)))
    where   ordIndexes = sortBy (comparing (\i -> abs $ (fst(dimension bd) + 1) `div` 2 - i)) indexes
            indexes = [1..fst(dimension bd)]

evaluate :: Board -> Score
evaluate bd = (myGetScore bd BlueBot) - (myGetScore bd RedBot)

maxAlg :: Board -> Tree GNS -> (Score, GNS)
maxAlg maxb (Node x []) = (evaluate (myUpdateBoard maxb x), x)
maxAlg maxb (Node _ ls) = maxOfTuple $ map (minAlg maxb) ls

-- choose the tuple that has the largest first element
maxOfTuple :: [(Score, GNS)] -> (Score, GNS)
maxOfTuple tps = head $ filter(\z -> fst z == bestScore) tps
    where bestScore = maximum $ map fst tps

minAlg :: Board -> Tree GNS -> (Score, GNS)
minAlg minb (Node x []) = (evaluate (myUpdateBoard minb x), x)
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


--------------------------------------------------------



--
-- genDepthBoards :: Int -> Board -> [(Index, Board)]
-- genDepthBoards 1 bb = genPossibleBoards bb
-- genDepthBoards d bb = concatMap (genDepthBoards (d-1)) $ map snd (genPossibleBoards bb)
--
-- -- generate the next possible board situation
-- genPossibleBoards :: Board -> [(Index, Board)]
-- genPossibleBoards root = zip orderedIndexes (map (updateBoardNoScore root) orderedIndexes)
--       where orderedIndexes = sortBy (comparing (\i -> abs $ (fst(dimension root) + 1) `div` 2 - i)) validIndexes
--             validIndexes = filter (validMove root) [1..fst(dimension root)]
--
-- genOneLayer :: LookAhead -> Int -> Board -> Tree (Index, Board)
-- genOneLayer 0 i r = (Node (i, r) [])
-- genOneLayer d i r = (Node (i, r) (map (genOneLayer (d-1)) $ map (updateBoardNoScore r) orderedIndexes))
--         where   -- order the index in a order that will prioritise the center columns.
--                 orderedIndexes b = sortBy (comparing (\i -> abs $ (fst(dimension b) + 1) `div` 2 - i)) validIndexes
--                 validIndexes b = filter (validMove b) [1..fst(dimension b)]


-- | it works!
-- genOneLayer :: LookAhead -> Board -> Tree Board
-- genOneLayer 0 r = (Node r [])
-- genOneLayer d r = (Node r (map (genOneLayer (d-1)) $ map (updateBoardNoScore r) orderedIndexes))
--         where   -- order the index in a order that will prioritise the center columns.
--                 orderedIndexes = sortBy (comparing (\i -> abs $ (fst(dimension r) + 1) `div` 2 - i)) validIndexes
--                 validIndexes = filter (validMove r) [1..fst(dimension r)]

-- treeMap :: (a -> b) -> RoseTree a -> RoseTree
-- treeMap f (Node x []) = Node (f x) []
-- treeMap f (Node x list) = Node (f x) map (treeMap f) list
-- zip  $ map (updateBoardNoScore root) orderedIndexes

--
--




-- generate the next possible board situation












-- we want the output [Int]
--validIndex :: Board -> [Int] -> [Int]
--validIndex b is = case is of
  --  [] -> []
   -- x : xs
    --    | length((board b) !! x) < height -> [x] ++ validIndex b xs
     --   | otherwise -> validIndex b xs
       -- where height = snd $ dimension b