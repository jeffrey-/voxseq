{-	For now, this module simply holds the waveIn function, which takes a path to
	a wave file (as String) as input and outputs a list of samples (as [Double]) 
	wrapped in IO.
-}

module Input
	( waveIn )
where

import Data.WAVE
	( WAVE (WAVE)
	, WAVESamples
	, getWAVEFile
	, sampleToDouble
	)

import Control.Monad
	( liftM )

getSamples :: WAVE -> WAVESamples
getSamples (WAVE h ss) = ss

samplesToDoubles :: WAVESamples -> [Double]
samplesToDoubles = map (sampleToDouble . head)

waveIn :: String -> IO [Double]
waveIn x = liftM (samplesToDoubles . getSamples) (getWAVEFile x)
