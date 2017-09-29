--
-- Code by Steven X. Han
--

module Data.Board (
    Board (Board, board, blueScore, redScore, turn, connect),
    Index,          -- = Int
    Dimension,      -- = (Int, Int)
    LookAhead,      -- = Int
    Score,          -- = Int
    dimension,      -- :: Board -> Dimension
    scorise,        -- :: Board -> Board
    validMove,      -- :: Board -> Index -> Bool,
    winBonus,       -- :: Board -> Score
    initialiseBoard -- :: Dimension -> Player -> Int -> Board
) where

import Data.Cell (Cell (Empty))
import Data.Matrix (Matrix)
import Data.Player (Player (BlueBot, RedBot, Finished), corresCell, otherPlayer)
import Data.Column (fillColumn)

import Data.List (intercalate, transpose)
import Data.List.Split (splitOn)
import Data.Universe.Helpers (diagonals)

data Board = Board { board     :: Matrix Cell
                   , blueScore :: Score
                   , redScore  :: Score
                   , turn      :: Player
                   , dimension :: Dimension
                   , connect   :: Int
                   }
    deriving (Eq)
                   
type Dimension = (Int, Int)

type LookAhead = Int

type Index     = Int

type Score     = Int

instance Show Board where
    show b = 
        intercalate " | " (map show [1 .. x]) ++ "\n" ++
        replicate (4 * x - 3) '-' ++ "\n" ++
        (intercalate "\n" 
            $ map (intercalate " | ")
                $ map (map show) 
                    $ reverse . transpose $ filledMat) ++
        "\n" ++ replicate (4 * x - 3) '-' ++
        "\n" ++ intercalate " | " (map show [1 .. x])
    
        where
            (x, _)    = dimension b
            boardMat  = board b
            filledMat = map (fillColumn Empty (snd $ dimension b)) boardMat

initialiseBoard :: Dimension -> Player -> Int -> Board
initialiseBoard d@(x, _) p i = Board {
    board     = replicate x [],
    blueScore = 0,
    redScore  = 0,
    turn      = p,
    dimension = d,
    connect   = i
    }
    
        
validMove :: Board -> Index -> Bool
validMove b i = case turn b of
    Finished -> False
    _        -> i >= 1 && i <= width && length (mat !! i') < height
    where
        width = fst $ dimension b
        height = snd $ dimension b
        i' = i - 1
        mat = board b
        
scorise :: Board -> Board
scorise b = b { blueScore = getScore b BlueBot,
                redScore  = getScore b RedBot }
                
getScore :: Board -> Player -> Score
getScore b p = sum [columnScore, rowScore, diagonalScore, otherDiagScore]
    where
        streak    = connect b
        minStreak = 1 + streak `div` 2
        otherBot  = otherPlayer p
        
        columnScore    = calculateScore $ filledMatrix
        rowScore       = calculateScore $ transpose $ filledMatrix
        diagonalScore  = calculateScore $ diagonals $ filledMatrix
        otherDiagScore = calculateScore $ diagonals $ map reverse $ filledMatrix
        
        filledMatrix   = map (fillColumn Empty $ snd $ dimension b) $ board b
        
        calculateScore mat = 
            sum $ (map (streakScore . length)) 
                $ filter ((>=minStreak).length) 
                    $ concatMap 
                        (concatMap (splitOn [corresCell otherBot])) 
                            $ map (splitOn [Empty]) mat
        
        streakScore :: Int -> Score
        streakScore i
            | i < minStreak = 0
            | otherwise     = i * 5
            
winBonus :: Board -> Score
winBonus b = uncurry (*) $ dimension b