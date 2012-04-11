module Test {-(main)-} where

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

path = "../test/t2"
wavPath = path ++ ".wav"
midPath = path ++ ".mid"
freqPath = path ++ ".freq"
ampPath = path ++ ".amp"

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

	a <- (doertemp midPath . medNotes) freq

	return a

	print "ok!"

{-

freq <- load freqPath
amp <- load ampPath

plotList [] freq

-}
