module Util.MyList
  (
    removeIndices
  , selectIndices
  ) where

-- Given a sorted list of indices, remove (in linear time) all of those positions from a list
removeIndices :: [a] -> [Int] -> [a]
removeIndices xs is = del (zip xs [0..]) is

del :: [(a,Int)] -> [Int] -> [a]
del xs [] = map fst xs
del [] _ = []
del ((x,xi):xs) ind@(i:is) = if xi == i then del xs is
                             else x : (del xs ind)

selectIndices :: [a] -> [Int] -> [a]
selectIndices xs is = sel (zip xs [0..]) is

sel :: [(a,Int)] -> [Int] -> [a]
sel _ [] = []
sel [] _  = []
sel ((x,xi):xs) ind@(i:is) = if xi == i then x : (sel xs is)
                             else sel xs ind
