module Input
	( getInput
	, input 
	)

where

import Data.WAVE
	( WAVE (WAVE)
	, WAVEHeader (WAVEHeader)
	, getWAVEFile
	, sampleToDouble
	)

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

getInput :: String -> IO WAVE
getInput = getWAVEFile
