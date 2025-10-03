{-# LANGUAGE ScopedTypeVariables #-}
module Problems.P_231A where

import           Problems.Class

data P_231A = P_231A

instance Problem P_231A where
  solve _ input =
    let entries :: [[Int]] =  fmap (fmap read . words) $ tail $ lines input in
    show $ foldl (\total decisions -> total + (if sum decisions >= 2 then 1 else 0)) 0 entries
