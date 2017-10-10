-- Assignment completed by
-- Name    :Yutong Wang
-- UID     :u6293753
-- Tutor   :James Kenneth Richardson
-- Lab Time:Friday 9:00 am - 11:00 am

module Bot.Blue where
    
import Data.Board

--data Tree Board = Node Board [Tree Board]

makeMove :: Board -> LookAhead -> Int
makeMove b _ = head $ filter (validMove b) [1..fst(dimension b)]






















    -- Blue can win in 1 move.

       -- or $ map hasWon $ map (updateBoard b) (validIndex b [1..maxIndex]) = 0
       --where maxIndex = fst(dimension b) - 1
    -- Red can win in 1 move, need to defend.
    -- Minimax Choose the move with the highest score for Blue
    -- and the lowest score for Red.



------------------------------------------------
--getIndex :: Board -> [Int] -> [Int]
--getIndex b i = case i of
  --  [] -> []
   -- x : xs
 --       | or $ map (hasWon) (map (updateBoard b) (validIndex b i)) -> i

-- we want the output [Int]
--validIndex :: Board -> [Int] -> [Int]
--validIndex b is = case is of
  --  [] -> []
   -- x : xs
    --    | length((board b) !! x) < height -> [x] ++ validIndex b xs
     --   | otherwise -> validIndex b xs
       -- where height = snd $ dimension b