module MathStuff
	( mean
	) where

import Data.List

-- not efficient, see https://donsbot.wordpress.com/2008/06/04/haskell-as-fast-as-c-working-at-a-high-altitude-for-low-level-performance/
data P = P !Double !Int
mean :: [Double] -> Double
mean xs = s / fromIntegral l
    where
    P s l = foldl' k (P 0 0) xs
    k (P s l) a = P (s+a) (l+1)
