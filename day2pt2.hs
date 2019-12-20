--Advent of Code: Day 2, Part 2
--Solution by Adam Gibbs (venomz75 on GitHub). Needed a lot of help on this one!
--Read about the challenge here: https://adventofcode.com/2019/day/2

import qualified Data.Map.Strict as M --data structure used
import Data.Maybe 

input :: [Int]
input = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,13,19,1,9,19,23,1,6,23,27,2,27,9,31,2,6,31,35,1,5,35,39,1,10,39,43,1,43,13,47,1,47,9,51,1,51,9,55,1,55,9,59,2,9,59,63,2,9,63,67,1,5,67,71,2,13,71,75,1,6,75,79,1,10,79,83,2,6,83,87,1,87,5,91,1,91,9,95,1,95,10,99,2,9,99,103,1,5,103,107,1,5,107,111,2,111,10,115,1,6,115,119,2,10,119,123,1,6,123,127,1,127,5,131,2,9,131,135,1,5,135,139,1,139,10,143,1,143,2,147,1,147,5,0,99,2,0,14,0]

input' :: Int -> Int -> M.Map Int Int
input' noun verb = M.insert 2 verb $ M.insert 1 noun $ M.fromList $ zip [0..] input

opcode :: Int -> Maybe (Int -> Int -> Int)
opcode 1 = Just (+)
opcode 2 = Just (*)
opcode _ = Nothing

bruteForce :: [(Int, Int, M.Map Int Int)]
bruteForce = do
    noun <- [0..99]
    verb <- [0..99]
    pure $ (noun, verb, input' noun verb)

process :: Int -> M.Map Int Int -> Maybe (M.Map Int Int)
process n xs = do
    operator <- M.lookup n xs
    case operator  of
        99 -> pure xs
        o -> do
            aPos <- M.lookup (n + 1) xs
            aVal <- M.lookup aPos xs
            bPos <- M.lookup (n + 2) xs
            bVal <- M.lookup bPos xs
            c <- M.lookup (n + 3) xs
            op <- opcode o
            process (n + 4) $ M.insert c (op aVal bVal) xs

main :: IO ()
main = 
    case filter (\(_, _, x) -> (process 0 x >>= M.lookup 0) == Just 19690720) bruteForce of
        ((noun, verb, _):_) -> print $ 100 * noun + verb