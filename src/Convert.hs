module Convert
--	( bla2
--	, timeDelta
--	, smooth
--	, smooth2
--	) 
where

import qualified Data.List as List
	( transpose
	, zip5
	, groupBy
	, sort
	, sortBy
	, elemIndex
	)

import Data.Maybe
	( fromMaybe )

import qualified Input
	( waveIn )

import qualified FFT
	( fftout )


-- this will go away
import Control.Monad


{-  options
-}
frameSize :: Int
frameSize = 1024*1 -- do make this a power of 2

overSamp :: Int
overSamp = 4 -- do make this a power of 2


{-  TODO: make this adjust to input
-}
sampleFreq :: Double
sampleFreq = 44100


{-  easier just to give these names
-}
frameSizeD :: Double
frameSizeD = fromIntegral frameSize

overSampD :: Double
overSampD = fromIntegral overSamp

frameLen = frameSizeD / sampleFreq
freqSpacing = sampleFreq / frameSizeD




timeDelta = frameSizeD / sampleFreq / overSampD

maxer (c, d)
	| null d	= 0
	| otherwise	= fromMaybe 0 ((!!) d `liftM` List.elemIndex (maximum c) c)

maxerBound (c, d)
	| null d 			= 0
	| max<80||max>1100  = 0
	| otherwise         = max
		where
		max = fromMaybe 0 ((!!) d `liftM` List.elemIndex (maximum c) c)

maxerFixer x = map

-- must not use true bin method if using this
paraMaxer (x, y)
	| null y 			= 0
	| max<80||max>1100  = 0
	| otherwise         = max
	where
		max = b - (((b-a)^2*(fb-fc))-((b-c)^2*(fb-fa))) / (((b-a)*(fb-fc))-((b-c)*(fb-fa))) / 2
		(a, b, c) = (y !! (e - 1), y !! e, y !! (e + 1))
		(fa, fb, fc) = (x !! (e - 1), x !! e, x !! (e + 1))
--		e' = fromMaybe 0 (List.elemIndex (maximum x) x)
--		e | e'==0 = 1 | otherwise = e'
		e = fromMaybe 1 (List.elemIndex (maximum x) x)

ampget :: [Double] -> Double
ampget = sum

vocalMask :: (Real a) => a -> a
vocalMask x
	| x>80 || x<1100 = 1
	| otherwise      = 0

vocalAmp :: [([Double], [Double])] -> [Double]
vocalAmp = map (sum . map ma . uncurry zip)
	where
	ma (amp, freq) = amp * vocalMask freq

ampfloor' :: [Double] -> Double
ampfloor' xs = List.sort [m x | x <- [0..4]] !! 2
	where
	m k = minimum $ take n $ drop k xs
	n = div (length xs) 5

ampfloor :: [Double] -> [Double]
ampfloor x = map (\ y -> y - ampfloor' x) x

{-
amppeak :: [Double] -> [Double]
amppeak = peakloop [] 0
	where
	peakloop ys a (x:xs) = peakloop ((maximum [0, a-x]):ys) (maximum [0, x]) xs
	peakloop ys a [] = reverse $ 0:ys
-}

smooth :: Fractional a => Int -> [a] -> [a]
smooth n x = thing [] (replicate (div n 2) (head x)++x++replicate (div n 2) (last x))
	where
	thing :: Fractional a => [a] -> [a] -> [a]
	thing ys (x:xs) 
		| length xs >= div n 2	= thing (((x + sum (take (n-1) xs))/fromIntegral n):ys) xs
		| otherwise				= reverse ys 

smooth2 :: (Ord a, Fractional a) => Int -> [a] -> [a]
smooth2 n x = thing [] (x++replicate n (last x))
	where
	thing :: (Ord a, Fractional a) => [a] -> [a] -> [a]
	thing ys (x:xs) 
		| length xs >= n 	= thing (maximum (x:take (n-1) xs):ys) xs
		| otherwise			= reverse ys 

peakfindb :: [Double] -> [Double]
peakfindb x = peakloop [] (smooth2 20 x)
	where
	peakloop ys (x0:x1:x2:xs)
		| not (null xs) && (x0 >= x1 || x1 < x2) = peakloop (0:ys) (x1:x2:xs)
		| not (null xs) && (x0 < x1 && x1 >= x2) = peakloop (1:ys) (x1:x2:xs)
		| otherwise                              = reverse (0:0:0:ys)

peakfinda :: [Double] -> [Double]
peakfinda x = peakloop [] (smooth2 20 x)
	where
	peakloop ys (x0:x1:x2:xs)
		| not (null xs) && (x0 >= x1 || x1 < x2) = peakloop (0:ys) (x1:x2:xs)
		| not (null xs) && (x0 < x1 && x1 >= x2) = peakloop (x0:ys) (x1:x2:xs)
		| otherwise                              = reverse (0:0:0:ys)

bla x = (freq, peaks)
	where
	q = FFT.fftout x
	freq = map maxerBound q
	amp = map ampget $ fst $ unzip q
	peaks = peakfinda amp


bla2 :: String -> IO ([Double], [Double])
bla2 = liftM bla . Input.waveIn




toFreq :: [([Double], [Double])] -> [Double]
toFreq = map maxerBound

toAmp :: [([Double], [Double])] -> [Double]
--toAmp = (map ampget) . fst . unzip
toAmp = vocalAmp

toPeak :: [([Double], [Double])] -> [Double]
toPeak = peakfinda . toAmp

