{-	Module for ((possibly temporarily) inefficient) implementations of basic
	math functions (sadly) not found in stable Haskell libraries.
-}

module MathStuff

where

import Data.List
	( sort )

-- not efficient, see https://donsbot.wordpress.com/2008/06/04/haskell-as-fast-as-c-working-at-a-high-altitude-for-low-level-performance/
--data P = P !Double !Int
--mean :: [Double] -> Double
--mean xs = s / fromIntegral l
--    where
--    P s l = foldl' k (P 0 0) xs
--    k (P s l) a = P (s+a) (l+1)

mean :: (Fractional a) => [a] -> a
mean x = sum x / (fromIntegral $ length x)

median :: (Ord a) => [a] -> a
median x
	| odd len  = sort x !! (div len 2)
	| even len = sort x !! (div len 2) {- close enough for purpose -}
	where
		len = length x
