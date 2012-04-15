{-	For now, this module simply holds the waveIn function, which takes a path to
	a wave file (as String) as input and outputs a list of samples (as [Double]) 
	wrapped in IO.
-}

module Input

where

import Data.WAVE
	( WAVE (WAVE)
	, WAVEHeader (WAVEHeader)
	, getWAVEFile
	, sampleToDouble
	)

getInput :: String -> IO WAVE
getInput = getWAVEFile

samples :: WAVE -> [Double]
samples (WAVE h ss) = map (sampleToDouble . head) ss

sampleRate :: WAVE -> Int
sampleRate (WAVE h ss) = r h
	where
	r (WAVEHeader _ r _ _) = r

-------------------------------------------------
-- Exported Functions
-------------------------------------------------

-- ah so dumb... TODO
input :: Integral a => WAVE -> (a, [Double])
input w = (fromIntegral (sampleRate w), samples w)
