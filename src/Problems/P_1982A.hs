module Problems.P_1982A where

import           Data.List      (intercalate)
import           Problems.Class

data P_1982A = P_1982A

instance Problem P_1982A where
  solve _ input = do
    let xs = zip (tail $ lines input) [0..]
        ys = zip
              (filter (\(_, i) -> i `mod` 2 == 0) xs)
              (filter (\(_, i) -> i `mod` 2 /= 0) xs)
    intercalate "\n" $ map (\((x, _), (y, _)) -> classify (parse x) (parse y)) ys
    where
      parse :: String -> [Int]
      parse = map read . words

      classify :: [Int] -> [Int] -> String
      classify [x1, y1] [x2, y2] =
        if (x1 > y1 && x2 < y2) || (y1 > x1 && y2 < x2)
        then "NO"
        else "YES"
