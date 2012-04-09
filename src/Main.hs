module Main {-(main)-} where

import Input
import FFT
import Convert
import Analyze
import Midi

import Control.Monad

import qualified System
	( getArgs )

rawfft wavPath = liftM fftout (waveIn wavPath)

main = do
	args <- System.getArgs
	let wavPath = head args
	r <- rawfft wavPath
	a <- (doertemp (wavPath ++ ".mid") . medNotes) (toFreq r)

	return a

	print "ok!"

{-

freq <- load freqPath
amp <- load ampPath

plotList [] freq

-}
