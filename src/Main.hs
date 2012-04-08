module Main {-(main)-} where

import Input
import FFT
import Convert
import Analyze
import Midi

import Control.Monad

import Graphics.Gnuplot.Simple

--------------------------------------------

{-	useful for caching fft result
-}
load :: FilePath -> IO [Double]
load f = do
	s <- readFile f
	return (read s :: [Double])


save :: (Show a) => a -> FilePath -> IO ()
save x f = writeFile f (show x)

---------------------------------------------

wavPath = "../test/t2.wav"
freqPath = "../test/t2.freq"
ampPath = "../test/t2.amp"

rawfft = liftM fftout (waveIn wavPath)

freq = liftM toFreq rawfft
amp = liftM toAmp rawfft
peak = liftM toPeak rawfft

cache = do
	a <- rawfft
	save (toFreq a) freqPath
	save (toAmp a) ampPath
	print "ok!" 

main = do
	freq <- load freqPath
	amp <- load ampPath

	a <- (doertemp . medNotes) freq

	return a

	print "ok!"

{-

freq <- load freqPath
amp <- load ampPath

plotList [] freq

-}
