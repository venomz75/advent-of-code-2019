--Advent of Code: Day 1 
--Solution by Adam Gibbs (venomz75 on GitHub)
--Read about the challenge here: https://adventofcode.com/2019/day/1
import Data.Function (fix) --for recursive lambda in solution 2

input :: [Int]
input = [115810, 58892, 76569, 87782, 103850, 103320, 62798, 98400, 71197, 124777, 97523, 52210, 122364, 112858, 58303, 72246, 130616, 118911, 120467, 62299, 71680, 83273, 87791, 89728, 112402, 94325, 118423,
        54979, 99132, 70851, 89887, 54131, 103911, 139205, 97804, 68670, 113097, 104705, 109659, 85259, 138145, 56602, 140942, 144354, 104776, 63627, 100050, 90929, 130607, 104809, 69613, 93375, 136009, 81838,
        84705, 61669, 84975, 95055, 107505, 126406, 116391, 57303, 128320, 93274, 78225, 116717, 84915, 109201, 102855, 61361, 146332, 127109, 78523, 61900, 59891, 135089, 55323, 51659, 87020, 86431, 132494, 
        51020, 126660, 81594, 73209, 71717, 135977, 78521, 82396, 118952, 144343, 149121, 119233, 79917, 125447, 127014, 138309, 107308, 146818, 63364] 

main :: IO ()
main = putStrLn ("First answer: " ++ show (sum (map (\x -> x `div` 3 - 2) input)) ++ --divide each element by 3, round the result down and subtract 2. add all of the results together
                "\nSecond answer:" ++ show (sum (map (fix (\f -> (\x -> if x >= 9 then (x `div` 3 - 2) + f(x `div` 3 - 2) else 0))) input))) --similar to the first, but results go through the same operations again(recursively) and each additional result is added to the total