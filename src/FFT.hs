module FFT
	( fftout )
where

import qualified Numeric.FFT as FFT
	( fft
	, ifft
	)

import Data.Complex as Complex

import Bits


{-	options
-}
frameSize :: Int
frameSize = 1024*1 -- do make this a power of 2

overSamp :: Int
overSamp = 4 -- do make this a power of 2


{-	TODO: make this adjust to input
-}
sampleFreq :: Double
sampleFreq = 44100


{-	easier just to give these names
-}
frameSizeD :: Double
frameSizeD = fromIntegral frameSize

overSampD :: Double
overSampD = fromIntegral overSamp

frameLen = frameSizeD / sampleFreq
freqSpacing = sampleFreq / frameSizeD


{-	choose a window
-}
window :: [Double]

--window = [1,1..]
--invWindow = window

window = map ((0.5+) . ((-0.5)*) . cos . ((2 * pi / (frameSizeD-1))*)) (take frameSize [0,1..]) -- hann
invWindow = window

--window = map ((0.5434782608695652+) . ((-0.45652173913043476)*) . cos . ((2 * pi / (frameSizeD-1))*)) (take frameSize [0,1..]) -- hamming 25/46 and 21/46
--invWindow = window

--blackman a0 a1 a2 x = a0 - a1 * cos (2 * pi * x / (frameSizeD - 1)) + a2 * cos (4 * pi * x / (frameSizeD - 1))
--window = map (blackman 0.42 0.5 0.08) (take frameSize [0,1..])
--invWindow = [1,1..]


{-
-}
mkComplexList :: [Double] -> [Complex Double]
mkComplexList x = zipWith id (map (Complex.:+) x) [0, 0 ..]


{-
-}
framer :: Int -> Int -> [Double] -> [[Double]]
framer _ _ [] = []
framer size overlap x = take size x : framer size overlap (drop (div size overlap) x)


{-
-}
-- takes a list of sample points and returns a pair of lists - amplitudes and phases
transform :: [Double] -> ([Double], [Double])
transform x = (map Complex.magnitude rawFFT, map Complex.phase rawFFT)
    where
    rawFFT = (thefft . mkComplexList) x
    thefft = {-# SCC "FFT.fft" #-} FFT.fft

transformR :: [Double] -> ([Double], [Double])
transformR x = (map (2*) {-to copy smb-} $ chop $ fst $ transform x, chop $ snd $ transform x)
    where
    chop = take (div (length x) 2 + 1)


{-
-}
trueBin :: Double -> Double -> Double -> Double
trueBin overSamp bin phaseDiff = overSamp * ((phaseDiff/2/pi) - fromIntegral qpd2 / 2)
    where
    qpd2 :: Int
    qpd2 = if qpd >= 0 then qpd + (qpd .&. 1) else qpd - (qpd .&. 1)
    qpd :: Int
    qpd = truncate((phaseDiff/pi) - (2*bin/overSamp)) -- changed from floor to ceiling because smb seems to do that

trueBinList' :: Double -> [[Double]] -> [[Double]]
trueBinList' _ [x] = []
trueBinList' overSamp (x0:x1:xs) = zipWith (trueBin overSamp) [0..] (zipWith (-) x1 x0) : trueBinList' overSamp (x1:xs)

trueBinList :: Double -> [[Double]] -> [[Double]]
trueBinList overSamp x = trueBinList' overSamp (take (length $ head x) [0,0..] : x)




fftout x = q
    where
    (a, b) = unzip $ map (transformR . zipWith (*) window) $ framer frameSize overSamp x
    q = zip a (map (map ((sampleFreq / frameSizeD) * )) (trueBinList overSampD b))

