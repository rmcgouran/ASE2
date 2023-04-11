module Lib where

qsort :: [Int] -> [Int]
qsort [] = []
qsort (pivot : restOfList) = 
    let smallerItems = filter (< pivot) restOfList
        largerItems = filter (> pivot) restOfList
    in
        qsort smallerItems ++ [pivot] ++ qsort largerItems
		
-- Function taken from https://stackoverflow.com/questions/1735146/ways-to-get-the-middle-of-a-list-in-haskell
middle :: [a] -> [a]
middle xs = take (signum ((l + 1) `mod` 2) + 1) $ drop ((l - 1) `div ` 2) xs
  where l = length xs
