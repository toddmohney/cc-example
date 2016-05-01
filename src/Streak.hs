module Streak
( Streak (..)
, appendToStreak
, collectStreaks
, filterStreaks
) where

import           Data.List (filter)

newtype Streak a = Streak [[a]]
  deriving (Show, Eq, Ord)

collectStreaks :: [[a]] -> [Streak a]
collectStreaks []       = []
collectStreaks [x]      = [Streak [x]]
collectStreaks (x:y:zs) = collectStreaks' (x:y:zs) (Streak [x]) []

collectStreaks' :: [[a]] -> Streak a -> [Streak a] -> [Streak a]
collectStreaks' [] currStreak streaks     = streaks ++ [currStreak]
collectStreaks' (x:[]) currStreak streaks = streaks ++ [currStreak]
collectStreaks' (x:y:zs) currStreak streaks
  | (length y) > (length x) = collectStreaks' (y:zs) (appendToStreak y currStreak) streaks
  | otherwise = collectStreaks' (y:zs) (Streak [y]) (streaks ++ [currStreak])

filterStreaks :: Int -> [Streak a] -> [Streak a]
filterStreaks minStreakLength = filter (\(Streak a) -> length a >= minStreakLength)

appendToStreak :: [a] -> Streak a -> Streak a
appendToStreak a (Streak s) = (Streak (s ++ [a]))
