{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Web.Scotty
import Data.Aeson (decode)
import Data.List (transpose, nub, group)
import Data.Maybe (isJust, isNothing)

type Board = [[Int]]

isValidGroup :: [Int] -> Bool
isValidGroup xs = let nonZero = filter (/= 0) xs
                  in length nonZero == length (nub nonZero)

hasConsecFour :: [Int] -> Bool
hasConsecFour xs = any (\g -> length g == 4 && head g /= 0) (group xs)

validArray :: (Bool -> Bool) -> (([Int] -> Bool) -> [[Int]] -> Bool) -> ([Int] -> Bool) -> Maybe Board -> Maybe Board
validArray _ _ _ Nothing = Nothing
validArray resultCondition groupValidator validator (Just board) =
    if resultCondition (groupValidator validator board)
    then Just board
    else Nothing

validSudokuRow :: Maybe Board -> Maybe Board
validSudokuRow = validArray id all isValidGroup

validSudokuColumn :: Maybe Board -> Maybe Board
validSudokuColumn = fmap transpose . validSudokuRow . fmap transpose

validConnect4Row :: Maybe Board -> Maybe Board
validConnect4Row = validArray not any hasConsecFour

validConnect4Column :: Maybe Board -> Maybe Board
validConnect4Column = fmap transpose . validConnect4Row . fmap transpose

getSubGrid :: [[Int]] -> Int -> Int -> [Int]
getSubGrid board row col =
    [board !! r !! c | r <- [row..row+2], c <- [col..col+2]]

isValidSubGrid :: Maybe Board -> Maybe Board
isValidSubGrid Nothing = Nothing
isValidSubGrid (Just board) = validSudokuRow (Just subGrids)
    where subGrids = [getSubGrid board r c | r <- [0,3,6], c <- [0,3,6]]

isValidSudokuBoard :: Maybe Board -> Maybe Board
isValidSudokuBoard = isValidSubGrid . validSudokuRow . validSudokuColumn

hasWinnerConnect4Board :: Maybe Board -> Maybe Board
hasWinnerConnect4Board = validConnect4Row . validConnect4Column

main :: IO ()
main = scotty 6969 $ do
    post "/validate-sudoku" $ do
        body <- body
        let board = decode body :: Maybe [[Int]]
        json $ isJust (isValidSudokuBoard board)

    post "/validate-connect4" $ do
        body <- body
        let board = decode body :: Maybe [[Int]]
        json $ isNothing (hasWinnerConnect4Board board)
