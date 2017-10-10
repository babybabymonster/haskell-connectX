module Bot.Red where

import Data.Board

makeMove :: Board -> LookAhead -> Int
makeMove b _ = head $ filter (validMove b) [1..fst(dimension b)]