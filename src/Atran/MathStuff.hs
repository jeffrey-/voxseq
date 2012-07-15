{-# LANGUAGE BangPatterns #-}
{-	Module for ((possibly temporarily) inefficient) implementations of basic
	math functions (sadly) not found in stable Haskell libraries.
-}

module MathStuff

where

import Data.List
	( sort 
	, isPrefixOf
	, foldl'
	)

import Debug.Trace

import Data.ByteString.Unsafe as BU
	( unsafeTail )

-- see https://donsbot.wordpress.com/2008/06/04/haskell-as-fast-as-c-working-at-a-high-altitude-for-low-level-performance/ for fun performance enhancing stuff....http://blog.johantibell.com/2010/09/slides-from-my-high-performance-haskell.html too
mean :: (Fractional a) => [a] -> a
mean xs = summ / fromIntegral len
	where
	(summ, len) = foldl' step (0, 0) xs
	step (!s, !l) y = (s+y, l+1)

median :: (Ord a) => [a] -> Maybe a
median xs
	| null xs  = Nothing
	| odd  len = Just    $ sort xs !! div len 2
	| even len = Just    $ sort xs !! div len 2 {- close enough for purpose -}
	where
		len = length xs


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
