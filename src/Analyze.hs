module Analyze
	( analysis )

where

import MathStuff
	( mean
	, median
	)

import qualified FFT
	( fullTransform )

import qualified Data.List as List
	( elemIndex )

import Data.Maybe
	( fromMaybe )

import Data.Fixed
	( mod'
	, div'
	) 

import Control.Monad
	( liftM )

import qualified Data.List.Split as LSplit
	( chunk )

import Bits



--------------------------------------------------
-- FFT
--------------------------------------------------

fftFrameSize :: Integral a => a
fftFrameSize = 512*2 -- do make this a power of 2

fftOverSamp :: Integral a => a
fftOverSamp = 1 -- do make this a power of 2

fft :: [Double] -> [([Double], [Double])]
fft = FFT.fullTransform fftFrameSize fftOverSamp



--------------------------------------------------
-- Bin/Primary Frequency Analysis
--------------------------------------------------

trueBin :: RealFloat a => a -> a -> a
trueBin bin phaseDiff = os * ((phaseDiff/2/pi) - fromIntegral qpd2 / 2)
	where
	os = fromIntegral fftOverSamp
	qpd2 :: Int
	qpd2 = if qpd >= 0 then qpd + (qpd .&. 1) else qpd - (qpd .&. 1)
	qpd :: Int
	qpd = truncate((phaseDiff/pi) - (2*bin/os)) -- changed from floor to ceiling because smb seems to do that

trueBinList' :: RealFloat a => [[a]] -> [[a]]
trueBinList' [x] = []
trueBinList' (x0:x1:xs) = zipWith trueBin (map fromIntegral [0..]) (zipWith (-) x1 x0) : trueBinList' (x1:xs)

trueBinList :: RealFloat a => [[a]] -> [[a]]
trueBinList x = trueBinList' (take (length $ head x) (map fromIntegral [0,0..]) : x)

whatIsThis :: RealFloat a => [[a]] -> [[a]]
whatIsThis = map (map ((44100 / fromIntegral fftFrameSize) * ))

maxerBound :: RealFloat a => ([a], [a]) -> a
maxerBound (c, d)
	| null d           = 0
	| max<80||max>1100 = 0
	| otherwise        = max
		where
		max = fromMaybe 0 ((!!) d `liftM` List.elemIndex (maximum c) c)

priFreq :: RealFloat a => [([a], [a])] -> [a]
priFreq t = zipWith (curry maxerBound) zip (amps t) (freqs t)
	where
	amps = fst . unzip
	freqs :: RealFloat a => [([a], [a])] -> [[a]]
	freqs = whatIsThis . trueBinList . snd . unzip



--------------------------------------------------
-- Pitch Analysis
--------------------------------------------------

freqToPitch :: (Ord a, Floating a) => a -> a
freqToPitch x = fix
	where
	fix = minimum [maximum [num, 0], 127]
	num = 12 * logBase 2 (x / 440) + 57


chunksperbeat :: Integral a => a
chunksperbeat = 4

chunksize :: Integral a => a
chunksize = round (44100 / toRational fftFrameSize * toRational fftOverSamp * 60 / 60 / toRational chunksperbeat)

chunksToNotes :: ([a] -> a) -> [a] -> [a]
chunksToNotes f = concatMap (\ x -> [f x]) . LSplit.chunk chunksize

medNotes :: Ord a => [a] -> [a]
medNotes = chunksToNotes median


scaleRound :: (RealFrac a, Integral b) => a -> b
scaleRound x
	| x>= 0   && x<=1   =  0    --root
	| x>  1   && x< 3   =  2    --2
	| x>= 3   && x<=4.5 =  4    --M3
	| x>  4.5 && x< 6   =  5    --4
	| x>= 6   && x<=8   =  7    --5
	| x>  8   && x<10   =  9    --M6
	| x>=10   && x<11.5 = 11    --M7
	| x>=11.5 && x<12   = 12    --octave

scalePitch :: (Integral a, RealFrac b) => a -> b -> a
scalePitch _ 0 = 0
scalePitch s x = scaleRound top + base + 57
	where
	base = div' p 12 * 12 - s
	top = mod' p 12
	p = x - 57 + fromIntegral s

-- this will definitely change
pitcher :: (RealFloat a, Integral b) => [a] -> [b]
pitcher = map (scalePitch 8) . medNotes . map freqToPitch



--------------------------------------------------
-- Amplitude Analysis
--------------------------------------------------

vocalMask :: (Real a) => a -> a
vocalMask x
	| x>80 || x<1100 = 1
	| otherwise      = 0

vocalAmp :: [([Double], [Double])] -> [Double]
vocalAmp = map (sum . map ma . uncurry zip)
	where
	ma (amp, freq) = amp * vocalMask freq



-------------------------------------------------
-- Exported Functions
-------------------------------------------------

analysis :: Integral a => (a, [Double]) -> [a]
analysis (fr, ss) = (pitcher . priFreq . fft) ss
