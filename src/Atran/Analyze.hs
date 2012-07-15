module Analyze
--	( analysis )

where

import Monobit

import MathStuff
	( mean
	, median
	, genFindSublists
	)

import qualified FFT
	( fullTransform )

import qualified Data.List as List
	( elemIndex 
	, sort
	)

import Data.Maybe
	( fromMaybe )

import Data.Fixed
	( mod'
	, div'
	) 

import Control.Monad
	( liftM )

import qualified Data.List.Split as LSplit
	( chunk 
	, wordsBy
	)

import Bits

import Debug.Trace


--------------------------------------------------
-- Options
--------------------------------------------------

-- these should be powers of 2
fftFrameSize = 1024
fftOverSamp = 1

-- filter outside this frequency range (in Hz)
highPass :: RealFloat a => a
highPass = 80

lowPass :: RealFloat a => a
lowPass = 1100



--------------------------------------------------
-- FFT
--------------------------------------------------

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

maxerBound :: RealFloat a => [a] -> [a] -> a
maxerBound amps freqs
--	| null freqs               = 0
	| max<highPass || max>1100 = 0
	| otherwise                = max
		where
		max = fromMaybe 0 ((!!) freqs `liftM` List.elemIndex (maximum amps) amps)

whatIsThis :: RealFloat a => a -> [[a]] -> [[a]] -- don't remember why this came about :(
whatIsThis sampRate = map (map ((sampRate / fromIntegral fftFrameSize) * ))

priFreq :: RealFloat a => a -> [([a], [a])] -> [a]
priFreq sampRate t = zipWith maxerBound (amps t) (freqs t)
	where
	amps = fst . unzip
	freqs = whatIsThis sampRate . trueBinList . snd . unzip



--------------------------------------------------
-- Amplitude Analysis
--------------------------------------------------

vocalMask :: (RealFloat a) => a -> a
vocalMask x
	| x>highPass || x<lowPass = 1
	| otherwise               = 0

vocalAmp :: [([Double], [Double])] -> [Double]
vocalAmp = map (sum . map ma . uncurry zip)
	where
	ma (amp, freq) = amp * vocalMask freq



--------------------------------------------------
-- Quantization
--------------------------------------------------

chunksize :: (RealFrac a, Integral b) => a -> b
chunksize spc = round (spc * fromIntegral fftOverSamp / fromIntegral fftFrameSize)

gate :: [Double] -> Double
gate x = 1.25 * minimum (map maximum (LSplit.chunk 16 x)) -- 16 is just a guess

noteHeads :: Ord a => a -> [a] -> [Int]
noteHeads gate = genFindSublists [(<=gate), (>gate)]

noteLasts :: Ord a => a -> [a] -> [Int]
noteLasts gate = genFindSublists [(>gate), (<=gate)]

quantIndex :: RealFrac a => a -> Int -> Int
quantIndex spc = (chunksize spc *) . round . (/ (toRational $ chunksize spc)) . toRational

quantize :: RealFrac a => a -> [(Double, Double)] -> [(Double, Double)]
quantize spc x = take (length x) $ foldl wat (repeat (0, 0)) (zip indices chunks)
	where
	amps    = (fst . unzip) x
	indices = (map (quantIndex spc) . noteHeads g) amps
	chunks  = LSplit.wordsBy ((<=g) . fst) x
	g       = gate amps

	wat result (index, notes) = beg ++ zipWith maxAmp mid notes ++ end
		where
		maxAmp (a0, f0) (a1, f1) = if a0>=a1 then (a0, f0) else (a1, f1)
		r   = result ++ repeat (0, 0)
		beg = take index r
		mid = take (length notes) (drop index r)
		end = drop (index + length notes) r



--------------------------------------------------
-- Frequency Smoothing
--------------------------------------------------

--chunksToNotes :: ([a] -> a) -> [a] -> [a]
--chunksToNotes f = concatMap (\ x -> [f x]) . LSplit.chunk chunksize
--
--medNotes :: (Num a, Ord a) => [a] -> [a]
--medNotes = chunksToNotes (fromMaybe 0 . median)

mediator :: (RealFrac a, Num b, Ord b) => a -> [(b, b)] -> [(b, b)]
mediator spc x = zipWith zfunc ((fst . unzip) x) (medList x)
	where
	zfunc x y | x>0 = (x, y) | otherwise = (0, 0)
	medList = concatMap (replicate (chunksize spc) . fromMaybe 0 . median . map snd . filter fil) . LSplit.chunk (chunksize spc)
	fil (x, y) = x>0 && y>0



--------------------------------------------------
-- Pitch Conversion
--------------------------------------------------

freqToPitch :: (Ord a, Floating a) => a -> a
freqToPitch x = minimum [maximum [trans, 0], 127]
	where
	trans = 12 * logBase 2 (x / 440) + 57

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

freqToScalePitch :: (Integral a, RealFloat b) => a -> b -> a  -- mode=8 -> C major?
freqToScalePitch mode = scalePitch mode . freqToPitch



--------------------------------------------------
-- Make Notes
--------------------------------------------------

-- convert notes to pitch bend ranges
-- map [-2, 2] to [0, 16383]
bendTrans :: Integral a => a -> a
bendTrans = min 16383 . max 0 . (*4096) . (+2)

basicNotesTB :: Integral a => [a] -> Melody
basicNotesTB = change 0 0 0 0
	where
--	change n k o p (hx:tx) | trace ("change " ++ show n ++ " " ++ show k ++ " " ++ show o ++ " " ++ show p ++ " " ++ show hx) False = undefined
	change _ _ _ _ [] = []
	change n k o p (hx:tx)
		| p==hx     = {-no change-}                                       change (n+1) k o  hx tx
		| p==0      = NoteOn    (n-k) (fromIntegral hx) 90              : change (n+1) n hx hx tx
		| hx==0     = NoteOff   (n-k) (fromIntegral o ) 90              : change (n+1) n hx hx tx
		| otherwise = PitchBend (n-k) (fromIntegral $ bendTrans (hx-o)) : change (n+1) n o  hx tx



-------------------------------------------------
-- Other
-------------------------------------------------

samplesPerChunk sampRate beatsPerMinute chunksPerBeat = sampRate * 60 / beatsPerMinute / chunksPerBeat

analysis :: Integral a => a -> [Double] -> [(a, a)]
analysis sr ss = zip (map round amps) (map (freqToScalePitch 8) freqs)
	where
	(amps, freqs) = (unzip . mediator spc . quantize spc) 
	                $ zip ((vocalAmp . fft) ss) ((priFreq (fromIntegral sr) . fft) ss)
	spc = samplesPerChunk (fromIntegral sr) 60 4



-------------------------------------------------
-- Exported Functions
-------------------------------------------------

basic :: Integral a => (a, [Double]) -> Melody
basic (sr, ss) = (basicNotesTB . snd . unzip . analysis sr) ss

--envelope :: envelope


