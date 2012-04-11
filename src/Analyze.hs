module Analyze

where

import MathStuff
	( mean
	, median
	)

import qualified Data.List.Split as List
	( chunk )

chunksperbeat = 4

chunksize :: Int
chunksize = fromInteger $ round $ 44100 / 1028 * 4 * 60 / 60 / chunksperbeat

chunk = List.chunk chunksize

--unchunk = concat

snapFreq :: Fractional a => ([a] -> a) -> [a] -> [a]
snapFreq f = concatMap reper . chunk
	where
	reper x = replicate (length x) (f x)

medFreq = snapFreq median

chunksToNotes :: Fractional a => ([a] -> a) -> [a] -> [a]
chunksToNotes f = concatMap (\ x -> [f x]) . chunk

medNotes = chunksToNotes median
