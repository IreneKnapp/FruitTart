module Network.FruitTart.Util.Lists (merge, mergeBy, mergeByM, split) where

import Control.Monad
import Data.List

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


mergeByM :: Monad m => (a -> a -> m Ordering) -> [[a]] -> m [a]
mergeByM function [a] = return a
mergeByM function [a, b] = mergeTwoByM function a b
mergeByM function (a:rest) = foldM (mergeTwoByM function) a rest

mergeTwoByM :: Monad m => (a -> a -> m Ordering) -> [a] -> [a] -> m [a]
mergeTwoByM function a [] = return a
mergeTwoByM function [] b = return b
mergeTwoByM function a@(headA:tailA) b@(headB:tailB) = do
  ordering <- function headA headB
  case ordering of
    LT -> do
      mergedTail <- mergeTwoByM function tailA b
      return $ headA : mergedTail
    GT -> do
      mergedTail <- mergeTwoByM function a tailB
      return $ headB : mergedTail
    EQ -> do
      mergedTail <- mergeTwoByM function tailA tailB
      return $ headA : headB : mergedTail


split :: (Eq a) => a -> [a] -> [[a]]
split item list
    = case elemIndex item list of
        Just index -> let (first, rest) = splitAt index list in
                      [first] ++ split item (tail rest)
        Nothing -> [list]
