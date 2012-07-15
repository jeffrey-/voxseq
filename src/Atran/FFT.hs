module FFT
	( fullTransform )
where



import qualified Numeric.FFT as FFT
	( fft )

import Data.Complex as Complex

import Control.Parallel.Strategies
	( parBuffer
	, runEval
	, rdeepseq
	)



--------------------------------------------------
-- Windows
--------------------------------------------------

window :: Integral a => a -> [Double]

--window = [1,1..]

window fs = map ((0.5+) . ((-0.5)*) . cos . ((2 * pi / (fromIntegral fs-1))*)) (take (fromIntegral fs) [0,1..]) -- hann

--window = map ((0.5434782608695652+) . ((-0.45652173913043476)*) . cos . ((2 * pi / (frameSizeD-1))*)) (take frameSize [0,1..]) -- hamming 25/46 and 21/46

--blackman a0 a1 a2 x = a0 - a1 * cos (2 * pi * x / (frameSizeD - 1)) + a2 * cos (4 * pi * x / (frameSizeD - 1))
--window = map (blackman 0.42 0.5 0.08) (take frameSize [0,1..])



--------------------------------------------------
-- Data Prep
--------------------------------------------------

mkComplexList :: [Double] -> [Complex Double]
mkComplexList x = zipWith id (map (Complex.:+) x) [0, 0 ..]

framer :: Integral a => a -> a -> [Double] -> [[Double]]
framer _ _ [] = []
framer size overlap x = take (fromIntegral size) x : framer size overlap (drop (fromIntegral (div size overlap)) x)



--------------------------------------------------
-- Transform
--------------------------------------------------

-- takes a list of sample points and returns a pair of lists - amplitudes and phases
transform :: [Double] -> ([Double], [Double])
transform x = (map Complex.magnitude rawFFT, map Complex.phase rawFFT)
    where
    rawFFT = (thefft . mkComplexList) x
    thefft = {-# SCC "FFT.fft" #-} FFT.fft

--transform :: [Double] -> ([Double], [Double])
--transform x = (map Complex.magnitude rawFFT, map Complex.phase rawFFT)
--    where
--    rawFFT = (V.fromVector . fft (length x) . V.vector . mkComplexList) x

transformR :: [Double] -> ([Double], [Double])
--transformR x = (map (2*) {-to copy smb-} $ chop $ fst $ transform x, chop $ snd $ transform x)
transformR x = ( (map (2*) {-to copy smb-} . chop) amp
               , chop phase
               )
	where
	(amp, phase) = transform x
	chop = take (div (length x) 2 + 1)



--------------------------------------------------
-- Exported Functions
--------------------------------------------------

fullTransform :: Integral a => a -> a -> [Double] -> [([Double], [Double])]
fullTransform fs os x = map (transformR . zipWith (*) (window fs)) (framer fs os x)

--not nice with ghci
--fullTransformPar :: Integral a => a -> a -> [Double] -> [([Double], [Double])]
--fullTransformPar fs os x = runEval $ parBuffer 2 rdeepseq $ (take 20 (map go y) ++ (repeat (0.0, 0.0)))
--	where
--	go = transformR . zipWith (*) (window fs)
--	y  = (framer fs os x)

