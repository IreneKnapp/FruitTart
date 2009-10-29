module Lists (merge, mergeBy) where

merge :: Ord a => [[a]] -> [a]
merge lists = mergeBy compare lists

mergeBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeBy function [a] = a
mergeBy function [a, b] = mergeTwoBy function a b
mergeBy function (a:rest) = foldl (mergeTwoBy function) a rest

mergeTwoBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeTwoBy function a [] = a
mergeTwoBy function [] b = b
mergeTwoBy function a@(headA:tailA) b@(headB:tailB)
    = case function headA headB of
        LT -> headA : mergeTwoBy function tailA b
        GT -> headB : mergeTwoBy function a tailB
        EQ -> headA : headB : mergeTwoBy function tailA tailB
