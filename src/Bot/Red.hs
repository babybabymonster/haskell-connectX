module Bot.Red where

import Data.Board

-- Define 'Move' type that contains the index of the column and
-- the score of the board after inserting the piece into this column.
type Move = (Index, Score)


makeMove :: Board -> LookAhead -> Int
makeMove b _ = maxCorrIndex $ map (move b) validIndexes
    where validIndexes = filter (validMove b) [1..fst(dimension b)]

--max $ map (\ z -> getScore z (turn b)) possibleBoards
  --  where possibleBoards = map (updateBoardNoScore b) (filter (validMove b) [1..fst(dimension b)])

move :: Board -> Index -> Move
move b i = (i, getScore (updateBoardNoScore b i) (turn b))

-- find the maximum number of the second element of a list of tuples
-- extract the corresponding first element of that tuple
-- maybe more than one tuple have the bestScore, just choose the head
-- of them.
maxCorrIndex :: [(Index, Score)] -> Index
maxCorrIndex lst = fst $ head $ filter (\ z -> snd z == bestScore) lst
    where bestScore = maximum $ map snd lst