module Convert
	( bla2
	, timeDelta
	, smooth
	, smooth2
	) 
where

import Data.Complex as Complex
import Bits
--import Data.WAVE as WAVE
import qualified Data.List as List (transpose, zip5, groupBy, sortBy)
import qualified System (getArgs)
import qualified Numeric.FFT (fft, ifft)

import Data.List
import Control.Monad
import Data.Maybe

import qualified Input
	( waveIn )


sampleFreq :: Double
sampleFreq = 44100

frameSize :: Int
--frameSize = 1024*1 -- do make this a power of 2
frameSize = 1024*1 -- do make this a power of 2

overSamp :: Int
overSamp = 4 -- do make this a power of 2

freqMultiplier, ampMult :: Double
freqMultiplier = 2**((0)/12)
ampMult = 1 -- 4 -- 0.001
--
frameSizeD :: Double
frameSizeD = fromIntegral frameSize

overSampD :: Double
overSampD = fromIntegral overSamp

frameLen = frameSizeD / sampleFreq
freqSpacing = sampleFreq / frameSizeD

--                window = -.5*cos(2.*M_PI*(double)k/(double)fftFrameSize)+.5;
--blackman = (\a0 a1 a2 x -> a0 - a1 * cos (2 * pi * x / (frameSizeD - 1)) + a2 * cos (4 * pi * x / (frameSizeD - 1)))
blackman a0 a1 a2 x = a0 - a1 * cos (2 * pi * x / (frameSizeD - 1)) + a2 * cos (4 * pi * x / (frameSizeD - 1))

window :: [Double]
--window = [1,1..]
window = map ((0.5+) . ((-0.5)*) . cos . ((2 * pi / (frameSizeD-1))*)) (take frameSize [0,1..]) -- hann
invWindow = window
--window = map ((0.5434782608695652+) . ((-0.45652173913043476)*) . cos . ((2 * pi / (frameSizeD-1))*)) (take frameSize [0,1..]) -- hamming 25/46 and 21/46
--invWindow = window
--window = map (blackman 0.42 0.5 0.08) (take frameSize [0,1..])
--invWindow = [1,1..]


--getSamples :: WAVE -> WAVESamples
--getSamples (WAVE h ss) = ss

--
--samplesToDoubles :: WAVESamples -> [Double]
--samplesToDoubles = map (sampleToDouble . head)

--
mkComplexList :: [Double] -> [Complex Double]
mkComplexList x = zipWith id (map (Complex.:+) x) [0, 0 ..]

mkDoubleList = map Complex.realPart
--
framer :: Int -> Int -> [Double] -> [[Double]]
framer _ _ [] = []
framer size overlap x = (take size x) : framer size overlap (drop (div size overlap) x)


-- takes a list of sample points and returns a pair of lists - amplitudes and phases
transform :: [Double] -> ([Double], [Double])
transform x = (map Complex.magnitude rawFFT, map Complex.phase rawFFT)
	where
	rawFFT = (thefft . mkComplexList) x
	thefft = {-# SCC "Numeric.FFT.fft" #-} Numeric.FFT.fft

transformR :: [Double] -> ([Double], [Double])
transformR x = (map (2*) {-to copy smb-} $ chop $ fst $ transform x, chop $ snd $ transform x)
	where
	chop = take (div (length x) 2 + 1)




trueBin :: Double -> Double -> Double -> Double
trueBin overSamp bin phaseDiff = overSamp * ((phaseDiff/2/pi) - (fromIntegral qpd2)/2)
	where
	qpd2 :: Int
	qpd2 = if qpd >= 0 then qpd + (qpd .&. 1) else qpd - (qpd .&. 1)
	qpd :: Int
	qpd = truncate((phaseDiff/pi) - (2*bin/overSamp)) -- changed from floor to ceiling because smb seems to do that

trueBinList' :: Double -> [[Double]] -> [[Double]]
trueBinList' _ [x] = []
--trueBinList' overSamp (x0:x1:xs) = map (uncurry $ trueBin overSamp) (zip [0..] (zipWith (-) x1 x0))
--									: trueBinList' overSamp (x1:xs)
trueBinList' overSamp (x0:x1:xs) = zipWith (trueBin overSamp) [0..] (zipWith (-) x1 x0) : trueBinList' overSamp (x1:xs)

trueBinList :: Double -> [[Double]] -> [[Double]]
trueBinList overSamp x = trueBinList' overSamp ((take (length $ head x) [0,0..]) : x)





timeDelta = frameSizeD / sampleFreq / overSampD

maxer (c, d)
--  | length d == 0	= 0
	| null d	= 0
	| otherwise	= fromMaybe 0 ((!!) d `liftM` (elemIndex (maximum c) c))

maxerBound (c, d)
--  | (length d == 0)   = 0
	| null d 			= 0
	| (max<80||max>1100)= 0
	| (otherwise)       = max
		where
		max = fromMaybe 0 ((!!) d `liftM` (elemIndex (maximum c) c))

maxerFixer x = map

paraMaxer (x, y)
--  | (length y == 0)   = 0
	| null y 			= 0
	| (max<80||max>1100)= 0
	| (otherwise)       = max
	where
		max = b - (((b-a)^2*(fb-fc))-((b-c)^2*(fb-fa))) / (((b-a)*(fb-fc))-((b-c)*(fb-fa))) / 2
		(a, b, c) = (y !! (e - 1), y !! e, y !! (e + 1))
		(fa, fb, fc) = (x !! (e - 1), x !! e, x !! (e + 1))
		e' = fromMaybe 0 (elemIndex (maximum x) x)
		e | e'==0 = 1 | otherwise = e'

ampget :: [Double] -> Double
ampget = sum

ampfloor' :: [Double] -> Double
ampfloor' xs = sort [m x | x <- [0..4]] !! 2
	where
	m k = minimum $ take n $ drop k xs
	n = div (length xs) 5

ampfloor :: [Double] -> [Double]
ampfloor x = map (\ y -> y - (ampfloor' x)) x

{-
amppeak :: [Double] -> [Double]
amppeak = peakloop [] 0
	where
	peakloop ys a (x:xs) = peakloop ((maximum [0, a-x]):ys) (maximum [0, x]) xs
	peakloop ys a [] = reverse $ 0:ys
-}

smooth :: Fractional a => Int -> [a] -> [a]
smooth n x = thing [] ((replicate (div n 2) (head x))++x++(replicate (div n 2) (last x)))
	where
	thing :: Fractional a => [a] -> [a] -> [a]
	thing ys (x:xs) 
		| length xs >= div n 2	= thing (((x + (sum $ take (n-1) xs))/(fromIntegral n)):ys) xs
		| otherwise				= reverse ys 

smooth2 :: (Ord a, Fractional a) => Int -> [a] -> [a]
smooth2 n x = thing [] (x++(replicate n (last x)))
	where
	thing :: (Ord a, Fractional a) => [a] -> [a] -> [a]
	thing ys (x:xs) 
		| length xs >= n 	= thing ((((maximum $ x:take (n-1) xs))):ys) xs
		| otherwise			= reverse ys 

peakfindb :: [Double] -> [Double]
peakfindb x = peakloop [] (smooth2 20 x)
	where
	peakloop ys (x0:x1:x2:xs)
		| (not $ null xs) && (x0 >= x1 || x1 < x2)	= peakloop (0:ys) (x1:x2:xs)
		| (not $ null xs) && (x0 < x1 && x1 >= x2)	= peakloop (1:ys) (x1:x2:xs)
		| null []									= reverse (0:0:0:ys)

peakfinda :: [Double] -> [Double]
peakfinda x = peakloop [] (smooth2 20 x)
	where
	peakloop ys (x0:x1:x2:xs)
		| (not $ null xs) && (x0 >= x1 || x1 < x2)	= peakloop (0:ys) (x1:x2:xs)
		| (not $ null xs) && (x0 < x1 && x1 >= x2)	= peakloop (x0:ys) (x1:x2:xs)
		| null []									= reverse (0:0:0:ys)

bla x = (freq, peaks)
	where
--	(a, b) = unzip $ map transformR $ map (zipWith (*) window) $ framer frameSize overSamp (samplesToDoubles $ getSamples x)
	(a, b) = unzip $ map (transformR . zipWith (*) window) $ framer frameSize overSamp x
	q = zip a (map (map (((sampleFreq / frameSizeD) * ))) (trueBinList overSampD b))
	freq = map maxerBound q
	amp = map ampget a
	peaks = peakfinda amp

{-
:l Convert
import Data.WAVE
import Graphics.Gnuplot.Simple
x <- getWAVEFile "b.wav"
let	(a, b) = unzip $ map (transformR . zipWith (*) window) $ framer frameSize overSamp (samplesToDoubles $ getSamples x)
let     q = zip a (map (map (((sampleFreq / frameSizeD) * ))) (trueBinList overSampD b))
let p = map maxerBound q
plotList [] p

let amp = map ampget a
let s = smooth 7 $ ampfloor $ amp
let peaks = peakfind amp
plotList [] peaks
-}

bla2 :: String -> IO ([Double], [Double])
bla2 = (liftM bla) . Input.waveIn

--main = do
--	--args <- System.getArgs
--	let args = ["test5.wav"]
--	audio <- getWAVEFile (args !! 0)
--	let b = bla audio
--	print b
--	-- putWAVEFile (args !! 1) b
