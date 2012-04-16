{-	Module for ((possibly temporarily) inefficient) implementations of basic
	math functions (sadly) not found in stable Haskell libraries.
-}

module MathStuff

where

import Data.List
	( sort 
	, isPrefixOf
	)

import Debug.Trace

import Data.ByteString.Unsafe as BU
	( unsafeTail )

-- not efficient, see https://donsbot.wordpress.com/2008/06/04/haskell-as-fast-as-c-working-at-a-high-altitude-for-low-level-performance/
--data P = P !Double !Int
--mean :: [Double] -> Double
--mean xs = s / fromIntegral l
--    where
--    P s l = foldl' k (P 0 0) xs
--    k (P s l) a = P (s+a) (l+1)

mean :: (Fractional a) => [a] -> a
mean x = sum x / fromIntegral (length x)

median :: (Ord a) => [a] -> Maybe a
median x
	| null x   = Nothing
	| odd  len = Just    $ sort x !! div len 2
	| even len = Just    $ sort x !! div len 2 {- close enough for purpose -}
	where
		len = length x


-- Adapted from Data.ByteString.findSubstrings.
-- | Find the indexes of all (possibly overlapping) occurances of a
-- sublist in a list.
--
findSublists :: Eq a => [a] -> [a] -> [Int]
findSublists pat lst
	| null pat  = [0 .. length lst]
	| otherwise = search 0 lst
	where
	search a b | a `seq` b `seq` False = undefined
	search n l
		| null l             = []
		| pat `isPrefixOf` l = n : search (n+1) (tail l)
		| otherwise          =     search (n+1) (tail l)


-- | The 'isPrefixOf' function takes two lists and returns 'True'
-- iff the first list is a prefix of the second.
genIsPrefixOf :: [a -> Bool] -> [a] -> Bool
genIsPrefixOf [] _          = True
genIsPrefixOf _  []         = False
genIsPrefixOf (f:fs) (x:xs) = f x && genIsPrefixOf fs xs

genFindSublists :: [a -> Bool] -> [a] -> [Int]
genFindSublists sub lst
	| null sub  = [0 .. length lst]
	| otherwise = search 0 lst
	where
	search a b | a `seq` b `seq` False = undefined
	search n l
		| null l              = []
		| genIsPrefixOf sub l = n : search (n+1) (tail l)
		| otherwise           =     search (n+1) (tail l)
